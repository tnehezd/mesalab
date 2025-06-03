# mesa_tools/config_parser.py

import os
import argparse
import yaml
import sys
import logging # <-- ADDED: Import the logging module

# Set up basic logging for this module.
# This ensures logging works even if this file is run directly or imported
# before a global logging config is set up in cli.py.
logging.basicConfig(level=logging.DEBUG, format='%(levelname)s: %(message)s')


def parsing_options():
    """
    Parses command-line arguments and loads settings from a YAML configuration file.
    Command-line arguments take precedence over YAML settings.

    Returns:
        argparse.Namespace: An object containing all parsed arguments and
                            configuration settings.
    """
    parser = argparse.ArgumentParser(description="Analyze MESA stellar evolution grid runs.")

    # Define command-line arguments
    parser.add_argument("--config", type=str,
                        help="Path to a YAML configuration file. Command-line arguments will override file settings.")

    parser.add_argument("-i", "--input-dir", required=False,
                        help="Directory containing MESA run subdirectories (e.g., 'run_M2.0_Z0.01').")
    parser.add_argument("-o", "--output-dir", required=False,
                        help="Output directory for results.")
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
                        help="Generate plots for analysis (beyond heatmaps, typically full HRD).")
    parser.add_argument("--generate-blue-loop-plots-with-bc", action="store_true",
                        help="Generate blue loop specific HRD/CMD/logg-L plots with bolometric corrections.")


    # Parse arguments from the command line first.
    # These values are initial defaults or explicitly set by the user via CLI.
    args = parser.parse_args()

    # --- Initial check for config file status ---
    # This block logs whether a config file was specified.
    if args.config:
        logging.info(f"Configuration file specified: {args.config}")
        # Log current args values *before* YAML loading (they'll be defaults or CLI-set)
        logging.debug(f"Args BEFORE YAML loading: {args}")
    else:
        logging.info("No configuration file specified. Using command-line/default arguments.")

    # Load configuration from file if provided
    if args.config:
        config_path = os.path.abspath(args.config) # Convert to absolute path for robustness
        if not os.path.exists(config_path):
            logging.error(f"Configuration file not found: {config_path}. Exiting.")
            sys.exit(1) # Exit if the config file isn't found
        try:
            with open(config_path, 'r') as f:
                config_data = yaml.safe_load(f)

            logging.debug(f"Raw config_data loaded from YAML: {config_data}") # DEBUG: Raw YAML data

            if config_data:
                logging.debug("Applying YAML configuration settings...")
                # Iterate through items in the loaded YAML data
                for key, value in config_data.items():
                    # Check if the YAML key corresponds to an argument defined in argparse
                    if hasattr(args, key):
                        current_arg_value = getattr(args, key)
                        default_arg_value = parser.get_default(key)

                        logging.debug(f"  YAML Key: '{key}', YAML Value: '{value}' (Type: {type(value)})")
                        logging.debug(f"  Current args.{key}: '{current_arg_value}' (Type: {type(current_arg_value)})")
                        logging.debug(f"  Default for {key}: '{default_arg_value}' (Type: {type(default_arg_value)})")

                        # **IMPROVED LOGIC: CLI > YAML > Default**
                        # Override with YAML value ONLY if the command-line argument was NOT explicitly provided.
                        # This is determined by checking if the current argument's value is still its default.
                        # For boolean flags (action="store_true", default is False), if the flag was present
                        # on the CLI, its value will be True (not equal to default), so it won't be overridden.
                        # If not on CLI, its value is False (equal to default), so it can be overridden by YAML.
                        if current_arg_value == default_arg_value:
                            logging.debug(f"  Overriding default value for '{key}' ('{current_arg_value}') with YAML value: '{value}'")
                            setattr(args, key, value) # Set the 'args' object's attribute to the YAML value
                        else:
                            logging.debug(f"  Keeping command-line value for '{key}' ('{current_arg_value}'), which overrides YAML value: '{value}'")
                    else:
                        logging.warning(f"  YAML key '{key}' does not correspond to a defined command-line argument. Skipping.")

            else:
                logging.warning("YAML configuration file is empty or contains no data. Using command-line/default arguments.")

        except yaml.YAMLError as e:
            logging.error(f"Error parsing YAML configuration file {config_path}: {e}. Proceeding with command-line/default arguments.")
        except Exception as e:
            logging.error(f"An unexpected error occurred while processing config file {config_path}: {e}. Proceeding with command-line/default arguments.")

    # Make input_dir and output_dir mandatory after config loading
    # These parser.error calls will exit the program if required arguments are missing.
    # Note: If these were not `required=True` in add_argument, they must be checked here.
    if args.input_dir is None:
        parser.error("--input-dir is required if not specified in the config file.")
#    if args.output_dir is None:
#        parser.error("--output-dir is required if not specified in the config file.")

    return args

# This block ensures that if config_parser.py is run directly, it will execute
# the parsing_options function. In a typical setup, this file would be imported
# by another script (like cli.py), and this block would not run.
if __name__ == "__main__":
    print("Running config_parser.py directly for testing.")
    parsed_args = parsing_options()
    print(f"Parsed Arguments: {parsed_args}")