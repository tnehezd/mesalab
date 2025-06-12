# mesagrid/config_parser.py

import os
import argparse
import yaml
import sys
import logging

# Set up basic logging for this module.
# This ensures logging works even if this file is run directly or imported
# before a global logging config is set up in cli.py.
# The level here can be DEBUG as cli.py will override it with its global setting.
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
                        help="Generate general plots (apart from heatmaps, typically full HRD).")
    parser.add_argument("--generate-blue-loop-plots-with-bc", action="store_true",
                        help="Generate blue loop specific HRD/CMD/logg-L plots with bolometric corrections.")

    # --- MODIFIED HR DIAGRAM GENERATION ARGUMENT ---
    # This now accepts an optional string value. If the flag is present but no value
    # is given, it defaults to 'default'. If the flag is not present at all, it defaults to 'none'.
    parser.add_argument("--generate-hr-diagrams",
                        type=str,
                        nargs='?',  # Makes the argument value optional (0 or 1 value)
                        const='default', # Value if the flag is present without a subsequent value
                        default='none', # Value if the flag is not present at all
                        choices=['default', 'drop_zams'],
                        help="Generate Hertzsprung-Russell diagrams. 'none' (do not generate HRDs), 'default' (generate standard HRDs with full tracks), 'drop_zams' (generate HRDs with pre-main sequence (ZAMS) trimming). If no value is specified after --generate-hr-diagrams, 'default' is assumed. Default: 'none'.")
    # -------------------------------------------------

    parser.add_argument("--debug", action="store_true",
                        help="Enable debug logging for more detailed output.")

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

                        logging.debug(f"   YAML Key: '{key}', YAML Value: '{value}' (Type: {type(value)})")
                        logging.debug(f"   Current args.{key}: '{current_arg_value}' (Type: {type(current_arg_value)})")
                        logging.debug(f"   Default for {key}: '{default_arg_value}' (Type: {type(default_arg_value)})")

                        # Override with YAML value ONLY if the command-line argument was NOT explicitly provided.
                        # This logic works for `type=str` arguments as well.
                        # argparse.get_default() correctly retrieves the default for store_true actions too,
                        # but for `type=str` arguments, direct comparison is sufficient.
                        # Special handling for nargs='?' and const:
                        # If --generate-hr-diagrams was called without a value, current_arg_value is 'default' (from const).
                        # If it was not called at all, current_arg_value is 'none' (from default).
                        # We want to override only if it was the *true* default ('none') from not being present,
                        # or if it was the 'default' from `const` but a YAML value is provided for *that specific* flag.
                        # However, the simpler logic of 'current_arg_value == default_arg_value' still holds for most cases,
                        # as 'default' from `const` is different from `default` from `default`.
                        # Let's rely on argparse's internal precedence: CLI always wins over YAML.
                        # The core idea here is that if a CLI argument was *not* provided (thus it holds its argparse `default` or `const` value),
                        # then it can be overridden by the config file.
                        
                        # A robust check: if an argument's source was not the command line
                        # (i.e., it's still its original default or const value after initial parse),
                        # then override it with the YAML value.
                        # This is a bit tricky with nargs='?', but generally, if `sys.argv` does not contain the flag
                        # with a subsequent value, or if the flag itself is not present, then it can be overridden.
                        
                        # Simpler approach that usually works: if the parsed argument value is the
                        # *default value that argparse would assign if the argument wasn't mentioned on CLI*,
                        # then allow YAML to override it.
                        
                        # For --generate-hr-diagrams:
                        # If not on CLI: args.generate_hr_diagrams == 'none' (from parser default)
                        # If on CLI as `--generate-hr-diagrams`: args.generate_hr_diagrams == 'default' (from const)
                        # If on CLI as `--generate-hr-diagrams some_value`: args.generate_hr_diagrams == 'some_value'
                        
                        # We only want to override if it was `none` (not present on CLI).
                        # If it was `default` (present without value), it's considered explicitly set by CLI.
                        # If it was `some_value`, it's explicitly set by CLI.
                        
                        # The original logic (current_arg_value == default_arg_value) works for this scenario
                        # as the "default_arg_value" returned by get_default for this specific argument
                        # with nargs='?' and const will be 'none', not 'default'.
                        if current_arg_value == parser.get_default(key):
                            logging.debug(f"   Overriding original default value for '{key}' ('{current_arg_value}') with YAML value: '{value}'")
                            setattr(args, key, value) # Set the 'args' object's attribute to the YAML value
                        else:
                            logging.debug(f"   Keeping command-line value for '{key}' ('{current_arg_value}'), which overrides YAML value: '{value}'")
                    else:
                        logging.warning(f"   YAML key '{key}' does not correspond to a defined command-line argument. Skipping.")

            else:
                logging.warning("YAML configuration file is empty or contains no data. Using command-line/default arguments.")

        except yaml.YAMLError as e:
            logging.error(f"Error parsing YAML configuration file {config_path}: {e}. Proceeding with command-line/default arguments.")
        except Exception as e:
            logging.error(f"An unexpected error occurred while processing config file {config_path}: {e}. Proceeding with command-line/default arguments.")

    # Make input_dir mandatory after config loading
    # These parser.error calls will exit the program if required arguments are missing.
    if args.input_dir is None:
        parser.error("--input-dir is required if not specified in the config file.")

    # Set the logging level based on the --debug argument.
    # This should be done *after* parsing args, but before any significant logging.
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
    else:
        # Set default logging level if --debug is not present
        # If cli.py sets a global level, it will override this.
        # For now, let's keep it at INFO if not debug.
        logging.getLogger().setLevel(logging.INFO)

    return args

# This block ensures that if config_parser.py is run directly, it will execute
# the parsing_options function. In a typical setup, this file would be imported
# by another script (like cli.py), and this block would not run.
if __name__ == "__main__":
    print("Running config_parser.py directly for testing.")
    # Example command line for testing:
    # python config_parser.py --input-dir my_runs --generate-hr-diagrams
    # python config_parser.py --input-dir my_runs --generate-hr-diagrams drop_zams
    # python config_parser.py --input-dir my_runs
    parsed_args = parsing_options()
    print(f"Parsed Arguments: {parsed_args}")
    print(f"HR Diagram setting: {parsed_args.generate_hr_diagrams}")
