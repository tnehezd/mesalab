# mesalab/io/config_parser.py

import os
import argparse
import yaml
import sys
import logging
from datetime import datetime # Added for logging, if needed elsewhere


# Set up basic logging for this module.
logging.basicConfig(level=logging.DEBUG, format='%(levelname)s: %(message)s')


def parsing_options():
    """
    Parses command-line arguments and loads settings from a YAML configuration file.
    Command-line arguments take precedence over YAML settings.

    Returns:
        argparse.Namespace: An object containing all parsed arguments and
                            configuration settings.
    """
    parser = argparse.ArgumentParser(description="Analyze MESA stellar evolution grid runs and generate GYRE input files.")

    # Define command-line arguments
    parser.add_argument("--config", type=str,
                        help="Path to a YAML configuration file. Command-line arguments will override file settings.")

    parser.add_argument("-i", "--input-dir", required=False,
                        help="Directory containing MESA run subdirectories (e.g., 'run_M2.0_Z0.01'). This is the main MESA output root.")
    parser.add_argument("-o", "--output-dir", required=False,
                        help="Output base directory for analysis results (summary, plots, GYRE inputs, etc.).")
    parser.add_argument("--analyze-blue-loop", action="store_true",
                        help="Perform blue loop analysis.")
    parser.add_argument("--inlist-name", default="inlist_project",
                        help="Inlist filename to identify runs (default: inlist_project).")
    parser.add_argument("--generate-heatmaps", action="store_true",
                        help="Generate heatmaps from cross-grid data.")
    parser.add_argument("--force-reanalysis", action="store_true",
                        help="Force reanalysis even if summary files exist.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'], default='all',
                        help="Blue loop output type for detail files: 'summary' includes selected key columns, 'all' includes all columns from the relevant blue loop phase. Default: 'all'.")
    parser.add_argument("--generate-plots", action="store_true",
                        help="Generate general plots (e.g., HR diagrams, if enabled).") 

    # --- NEW GYRE-RELATED ARGUMENTS START HERE ---
    parser.add_argument('--run-gyre-workflow', action='store_true',
                        help='Execute the full GYRE workflow: generate input CSV and run GYRE simulations. Requires gyre_config.in.')
    parser.add_argument('--gyre-config-path', type=str, default='gyre_config.in',
                        help='Path to the GYRE-specific configuration file (e.g., gyre_config.in). This config drives gyre_modules.')
    # --- NEW GYRE-RELATED ARGUMENTS END HERE ---

    parser.add_argument("--generate-hr-diagrams", type=str, choices=['none', 'all', 'drop_zams'], default='none',
                        help='Control HR diagram generation: "none", "all" profiles, or "drop_zams" (excluding ZAMS).')
    parser.add_argument("--generate-blue-loop-plots-with-bc", action="store_true",
                        help="Generate blue loop plots including bolometric corrections.")
    parser.add_argument("--debug", action="store_true",
                        help="Enable debug mode for more verbose logging output.")


    # Parse command-line arguments first
    cmd_args = parser.parse_args()

    # Get default values from parser
    defaults = {action.dest: action.default for action in parser._actions if action.dest != argparse.SUPPRESS}
    
    # Initialize combined_args with parser defaults
    combined_args = argparse.Namespace(**defaults)

    # Load settings from YAML file if provided
    config_from_yaml = {}
    if cmd_args.config:
        if not os.path.exists(cmd_args.config):
            logging.error(f"Configuration file not found: {cmd_args.config}")
            sys.exit(1)
        try:
            with open(cmd_args.config, 'r') as f:
                config_from_yaml = yaml.safe_load(f)
            logging.info(f"Loaded configuration from: {cmd_args.config}")
        except yaml.YAMLError as e:
            logging.error(f"Error parsing YAML configuration file {cmd_args.config}: {e}")
            sys.exit(1)

    # Apply YAML settings (overriding defaults)
    for key, value in config_from_yaml.items():
        # Only set if the argument exists in the parser
        if hasattr(combined_args, key):
            setattr(combined_args, key, value)
        else:
            logging.warning(f"YAML config contains unknown key '{key}'. Ignoring.")

    # Apply command-line arguments (overriding YAML and defaults)
    for key, value in vars(cmd_args).items():
        # 'config' argument is handled separately, don't re-apply
        if key == 'config':
            continue
        
        # Only overwrite if the command-line argument was actually provided (not its default)
        # This is a bit tricky for 'action="store_true"' flags, as their default is False.
        # If default is False and cmd_args.key is True, it was set.
        # If cmd_args.key is explicitly None (e.g., for optional string args not given), don't overwrite.
        
        # A more robust check for whether a CLI arg was explicitly set:
        # Check if the value from cmd_args is different from the original parser default for that arg.
        # For store_true flags, this effectively checks if it was switched from False to True.
        
        # Get the original default for this argument from the parser
        original_parser_default = parser.get_default(key)

        if value != original_parser_default or (value is True and original_parser_default is False):
            setattr(combined_args, key, value)
        elif value is not None and original_parser_default is None: # For args with no default set by argparse but provided by CLI
             setattr(combined_args, key, value)
             
    # Special handling for --input-dir and --output-dir as they are critical
    # If not provided via CLI or YAML, they need to be specified or handle defaults properly
    if not hasattr(combined_args, 'input_dir') or combined_args.input_dir is None:
        logging.critical("ERROR: --input-dir is required either via command-line or in the config file.")
        sys.exit(1)
    
    if not hasattr(combined_args, 'output_dir') or combined_args.output_dir is None:
        # Default if not specified anywhere. Can be overridden by YAML/CLI.
        combined_args.output_dir = './mesagrid_output' 
        logging.warning(f"No --output-dir specified. Using default: {combined_args.output_dir}")


    logging.debug(f"Final combined arguments: {combined_args}")
    return combined_args