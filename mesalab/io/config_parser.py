# mesalab/io/config_parser.py (Recommended Revision)

import os
import argparse
import yaml
import sys
import logging
from datetime import datetime

logging.basicConfig(level=logging.DEBUG, format='%(levelname)s: %(message)s')

def parsing_options():
    parser = argparse.ArgumentParser(description="Analyze MESA stellar evolution grid runs and generate GYRE input files.")

    # Define ALL command-line arguments, including their default values.
    # IMPORTANT: The default values here are the ultimate fallback if not
    # specified in YAML or on the command line.
    parser.add_argument("--config", type=str, help="Path to a YAML configuration file.")
    parser.add_argument("-i", "--input-dir", type=str, default=None, # Set default to None to easily check if it was set
                        help="Directory containing MESA run subdirectories.")
    parser.add_argument("-o", "--output-dir", type=str, default='./mesagrid_output',
                        help="Output base directory for analysis results.")
    parser.add_argument("--analyze-blue-loop", action="store_true", default=False,
                        help="Perform blue loop analysis.")
    parser.add_argument("--inlist-name", type=str, default="inlist_project",
                        help="Inlist filename to identify runs.")
    parser.add_argument("--generate-heatmaps", action="store_true", default=False,
                        help="Generate heatmaps from cross-grid data.")
    parser.add_argument("--force-reanalysis", action="store_true", default=False,
                        help="Force reanalysis even if summary files exist.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'], default='all',
                        help="Blue loop output type for detail files.")
    parser.add_argument("--generate-plots", action="store_true", default=False,
                        help="Generate general plots.")

    # NEW GYRE-RELATED ARGUMENTS (ensure default=False for store_true flags)
    parser.add_argument('--run-gyre-workflow', action='store_true', default=False, # <--- CRITICAL: default=False
                        help='Execute the full GYRE workflow: generate input CSV and run GYRE simulations.')
    parser.add_argument('--gyre-config-path', type=str, default='gyre_config.in',
                        help='Path to the GYRE-specific configuration file (e.g., gyre_config.in).')

    parser.add_argument("--generate-hr-diagrams", type=str, choices=['none', 'all', 'drop_zams'], default='none',
                        help='Control HR diagram generation.')
    parser.add_argument("--generate-blue-loop-plots-with-bc", action="store_true", default=False,
                        help="Generate blue loop plots including bolometric corrections.")
    parser.add_argument("--debug", action="store_true", default=False,
                        help="Enable debug mode for more verbose logging output.")

    # Parse arguments from the command line first. This gives us CLI values and argparse's defaults.
    args_from_cli = parser.parse_args()

    # Load settings from YAML file if a config file was specified on the CLI
    config_from_yaml = {}
    if args_from_cli.config:
        if not os.path.exists(args_from_cli.config):
            logging.error(f"Configuration file not found: {args_from_cli.config}")
            sys.exit(1)
        try:
            with open(args_from_cli.config, 'r') as f:
                config_from_yaml = yaml.safe_load(f)
            logging.info(f"Loaded configuration from: {args_from_cli.config}")
        except yaml.YAMLError as e:
            logging.error(f"Error parsing YAML configuration file {args_from_cli.config}: {e}")
            sys.exit(1)

    # Create a new Namespace object to hold the final combined arguments.
    # Initialize it with values from YAML, if present, otherwise with CLI-parsed defaults.
    final_args = argparse.Namespace()
    for arg_name, cli_value in vars(args_from_cli).items():
        # The 'config' argument is handled separately; we don't want it in the final_args for processing.
        if arg_name == 'config':
            continue

        # Get the default value for this argument as defined by argparse.
        # This is important for 'store_true' flags where default is False.
        arg_action = None
        for action in parser._actions:
            if action.dest == arg_name:
                arg_action = action
                break
        
        default_value = arg_action.default if arg_action else None # Get original argparse default

        # Prioritize values: CLI > YAML > Argparse_Default

        # Start with the default or None
        value_to_set = default_value
        
        # If the argument is in YAML, use the YAML value
        if arg_name in config_from_yaml:
            value_to_set = config_from_yaml[arg_name]

        # If the CLI value is different from its argparse default (meaning it was explicitly set on CLI)
        # Or if it's a 'store_true' flag and CLI value is True (overriding a default False)
        # Or if it's not a boolean flag and CLI value is not None (meaning it was provided)
        if (isinstance(arg_action, (argparse._StoreTrueAction, argparse._StoreFalseAction)) and cli_value != default_value) or \
           (not isinstance(arg_action, (argparse._StoreTrueAction, argparse._StoreFalseAction)) and cli_value is not None):
            value_to_set = cli_value

        setattr(final_args, arg_name, value_to_set)

    # Final validation for required arguments (like input_dir)
    if not hasattr(final_args, 'input_dir') or final_args.input_dir is None:
        logging.critical("ERROR: --input-dir is required either via command-line or in the config file.")
        sys.exit(1)
    
    # Set default for output_dir if not specified (explicitly defining default here as well)
    if not hasattr(final_args, 'output_dir') or final_args.output_dir is None:
        final_args.output_dir = './mesagrid_output'
        logging.warning(f"No --output-dir specified. Using default: {final_args.output_dir}")

    logging.debug(f"Final combined arguments: {final_args}")
    return final_args