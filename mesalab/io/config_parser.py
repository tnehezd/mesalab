# mesalab/io/config_parser.py (Final Version for Flat YAML)

import os
import argparse
import yaml
import sys
import logging
from datetime import datetime

# Initialize logging for this module
logger = logging.getLogger(__name__)
# Ensure basicConfig is only called once if this is the main entry point,
# otherwise it will be configured by the main script.
if not logging.root.handlers:
    logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')

def parsing_options():
    parser = argparse.ArgumentParser(description="Analyze MESA stellar evolution grid runs and generate GYRE input files.")

    # Define ALL command-line arguments with their DEFAULT VALUES.
    # These defaults will be overridden by YAML, then by CLI.
    parser.add_argument("--config", type=str, help="Path to a YAML configuration file.")
    parser.add_argument("-i", "--input-dir", type=str, default=None, # Will be explicitly validated later
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

    # GYRE-RELATED ARGUMENTS
    parser.add_argument('--run-gyre-workflow', action='store_true', default=False,
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
    # At this stage, args_from_cli contains:
    # 1. Values explicitly provided on CLI
    # 2. Argparse's default values for arguments not provided on CLI
    args_from_cli = parser.parse_args()

    # Create a dictionary from the parsed CLI arguments (including their argparse defaults)
    final_config = vars(args_from_cli).copy()

    # Load settings from YAML file if a config file was specified on the CLI
    if args_from_cli.config:
        if not os.path.exists(args_from_cli.config):
            logger.error(f"Configuration file not found: {args_from_cli.config}")
            sys.exit(1)
        try:
            with open(args_from_cli.config, 'r') as f:
                config_from_yaml = yaml.safe_load(f)
            logger.info(f"Loaded configuration from: {args_from_cli.config}")
            logger.debug(f"YAML content raw: {config_from_yaml}")

            # Merge YAML config into final_config.
            # YAML values (from 'config_from_yaml') will OVERWRITE argparse defaults.
            # However, CLI values (already in 'final_config') should take precedence.
            # So, iterate through YAML and only apply if CLI didn't explicitly set it.

            for key, yaml_value in config_from_yaml.items():
                # Check if this key exists as an argparse argument
                if key in final_config:
                    arg_action = None
                    for action in parser._actions:
                        if action.dest == key:
                            arg_action = action
                            break

                    # If it's a boolean flag (store_true/store_false) and CLI value is its default,
                    # or if it's not a boolean flag and CLI value is None (meaning not set by CLI)
                    # then apply the YAML value.
                    # This ensures CLI explicit values (even if they happen to match YAML default) are kept.
                    if arg_action:
                        is_boolean_flag = isinstance(arg_action, (argparse._StoreTrueAction, argparse._StoreFalseAction))

                        if (is_boolean_flag and final_config[key] == arg_action.default) or \
                           (not is_boolean_flag and final_config[key] is None):
                            final_config[key] = yaml_value
                    else:
                        # If it's not a direct argparse argument (e.g., a nested YAML key in old parser),
                        # just apply it directly. (This should not happen with flat YAML)
                        final_config[key] = yaml_value
                else:
                    # If the key is in YAML but not an argparse argument, keep it in final_config
                    # This happens for nested structures in old parser. For flat, this is not needed.
                    final_config[key] = yaml_value

            logger.debug(f"Config after YAML merge: {final_config}")

        except yaml.YAMLError as e:
            logger.error(f"Error parsing YAML configuration file {args_from_cli.config}: {e}")
            sys.exit(1)

    # Convert the final_config dictionary back to a Namespace object for consistency
    final_args_namespace = argparse.Namespace(**final_config)

    # Final validation for required arguments (like input_dir)
    if final_args_namespace.input_dir is None:
        logger.critical("ERROR: 'input_dir' must be specified either via command-line (--input-dir) or in the config file.")
        sys.exit(1)

    # Set logger level based on debug setting
    if final_args_namespace.debug:
        logging.root.setLevel(logging.DEBUG)
        logger.setLevel(logging.DEBUG) # Set module specific logger too
        logger.debug("Debug mode enabled.")

    logger.info(f"Final resolved configuration: {final_args_namespace}")
    return final_args_namespace

if __name__ == '__main__':
    # This block is for testing the config parser itself
    # Run with: python -m mesalab.io.config_parser --config your_config.yaml -i /path/to/override
    parsed_config = parsing_options()
    print("\n--- Parsed Config (Namespace) ---")
    print(parsed_config)
    # You can access like: parsed_config.input_dir, parsed_config.analyze_blue_loop
