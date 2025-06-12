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
                        help="Generate general plots (apart from heatmaps, typically full HRD).")
    parser.add_argument("--generate-blue-loop-plots-with-bc", action="store_true",
                        help="Generate blue loop specific HRD/CMD/logg-L plots with bolometric corrections.")

    # --- MODIFIED HR DIAGRAM GENERATION ARGUMENT ---
    # This now accepts an optional string value. If the flag is present but no value
    # is given, it defaults to 'default'. If the flag is not present at all, it defaults to 'none'.
    parser.add_argument("--generate-hr-diagrams",
                        type=str,
                        nargs='?', 
                        const='default', 
                        default='none', 
                        choices=['default', 'drop_zams'],
                        help="Generate Hertzsprung-Russell diagrams. 'none' (do not generate HRDs), 'default' (generate standard HRDs with full tracks), 'drop_zams' (generate HRDs with pre-main sequence (ZAMS) trimming). If no value is specified after --generate-hr-diagrams, 'default' is assumed. Default: 'none'.")
    # -------------------------------------------------

    parser.add_argument("--debug", action="store_true",
                        help="Enable debug logging for more detailed output.")

    # --- NEW: Arguments for GYRE Input File Generation ---
    parser.add_argument("--generate-gyre-in", action="store_true",
                        help="Enable generation of GYRE input files.")
    parser.add_argument("--gyre-mesa-base-dir", type=str, required=False, # Make it required=True if only used standalone
                        help="Base directory containing the MESA 'run_model_MSUN_z' directories for GYRE input generation. If not specified, --input-dir is used.")
    parser.add_argument("--gyre-output-dir", type=str, required=False, # Make it required=True if only used standalone
                        help="Output directory for the generated GYRE .in files. If not specified, a subdirectory within --output-dir will be used.")
    parser.add_argument("--gyre-model-name", type=str, required=False,
                        help="Identifier for the MESA model (e.g., 'nad_convos') used for GYRE path construction. If not specified, a default or derived name will be used.")
    parser.add_argument("--gyre-blue-loop-csv", type=str, required=False,
                        help="Path to the 'sorted_mass_Z_min_max.csv' file. Required if --generate-gyre-in is used standalone (without --analyze-blue-loop). If --analyze-blue-loop is also used, this file might be generated and used directly from memory.")
    # -----------------------------------------------------

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

                        # Override with YAML value ONLY if the command-line argument was NOT explicitly provided.
                        # This logic works for `type=str` arguments as well.
                        # argparse.get_default() correctly retrieves the default for store_true actions too,
                        # but for `type=str` arguments, direct comparison is sufficient.
                        
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
                            logging.debug(f"  Overriding original default value for '{key}' ('{current_arg_value}') with YAML value: '{value}'")
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

    # --- Post-parsing Argument Validation and Defaulting ---
    # Set default values for GYRE-specific arguments if they weren't provided via CLI or config,
    # and default them based on existing analysis arguments to streamline pipeline usage.
    if args.generate_gyre_in: # Only set these defaults if GYRE generation is explicitly enabled
        if not args.gyre_mesa_base_dir:
            # If GYRE base directory is not specified, default to the main input_dir
            args.gyre_mesa_base_dir = args.input_dir 
            logging.debug(f"Defaulting --gyre-mesa-base-dir to --input-dir: {args.gyre_mesa_base_dir}")
        
        if not args.gyre_output_dir:
            # If GYRE output directory is not specified, create a 'gyre_inputs' sub-directory within the main output_dir
            if args.output_dir:
                args.gyre_output_dir = os.path.join(args.output_dir, "gyre_inputs")
                logging.debug(f"Defaulting --gyre-output-dir to {args.gyre_output_dir} based on --output-dir.")
            else:
                # If output_dir is also not set, default to a local 'gyre_inputs'
                args.gyre_output_dir = "gyre_inputs"
                logging.warning(f"Neither --output-dir nor --gyre-output-dir specified. Defaulting GYRE output to: {args.gyre_output_dir}")

        if not args.gyre_model_name:
            # If GYRE model name is not specified, try to derive from existing args or use a generic default
            # A more robust way might be to parse from the 'mesa_output_dir' if it has a pattern
            # For now, let's use a simple default
            args.gyre_model_name = "default_mesa_model"
            logging.warning(f"No --gyre-model-name specified. Defaulting to: {args.gyre_model_name}")

        # Validation for --gyre-blue-loop-csv:
        # It's only strictly required if GYRE generation is requested *without* analysis,
        # otherwise, the data will come from the analysis results in memory.
        if not args.analyze_blue_loop and not args.gyre_blue_loop_csv:
             parser.error("--gyre-blue-loop-csv is required when --generate-gyre-in is used without --analyze-blue-loop.")
        elif args.analyze_blue_loop and args.gyre_blue_loop_csv:
             logging.info("--generate-gyre-in and --analyze-blue-loop are both set. The in-memory analysis results will take precedence for GYRE input. The provided --gyre-blue-loop-csv will be used as a fallback if in-memory data is unavailable.")


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
    print(f"GYRE generation enabled: {parsed_args.generate_gyre_in}")
    print(f"GYRE MESA Base Dir: {parsed_args.gyre_mesa_base_dir}")
    print(f"GYRE Output Dir: {parsed_args.gyre_output_dir}")
    print(f"GYRE Model Name: {parsed_args.gyre_model_name}")
    print(f"GYRE Blue Loop CSV: {parsed_args.gyre_blue_loop_csv}")