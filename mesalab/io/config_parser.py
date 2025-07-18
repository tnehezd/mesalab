# mesalab/io/config_parser.py

import os
import argparse
import yaml
import sys
import logging
from datetime import datetime

# To handle nested configuration as attributes (e.g., config.general_settings.input_dir)
# If addict is not installed, you might need to 'pip install addict'
try:
    from addict import Dict
except ImportError:
    print("Error: 'addict' library not found. Please install it using 'pip install addict'.", file=sys.stderr)
    sys.exit(1)


# Initialize logging for this module
# This basic configuration ensures logs appear even before the main app's setup.
logger = logging.getLogger(__name__)

def parsing_options():
    """
    Parses command-line arguments, loads settings from a YAML configuration file,
    and merges them into a final configuration object.
    CLI arguments take precedence over YAML settings, which take precedence over environment variables,
    which take precedence over defaults.

    Returns:
        addict.Dict: A nested Dict object containing the final resolved configuration.
    """
    # 1. Define command-line arguments. These arguments can override YAML settings.
    parser = argparse.ArgumentParser(
        description="Analyze MESA stellar evolution grid runs and generate GYRE input files.",
        add_help=False # Let the main cli.py script handle the overall help display
    )

    # CLI argument for the config file path itself
    parser.add_argument("--config", type=str, default="config.yaml",
                        help="Path to a YAML configuration file.")

    # General Settings (can be overridden by CLI)
    parser.add_argument("-i", "--input-dir", type=str, default=None,
                        help="Override general_settings.input_dir. Directory containing MESA run subdirectories.")
    parser.add_argument("-o", "--output-dir", type=str,
                        help="Override general_settings.output_dir. Output base directory for analysis results.")
    parser.add_argument("--inlist-name", type=str,
                        help="Override general_settings.inlist_name. Inlist filename to identify runs.")
    parser.add_argument("--force-reanalysis", action="store_true",
                        help="Override general_settings.force_reanalysis. Force reanalysis even if summary files exist.")
    parser.add_argument("--debug", action="store_true",
                        help="Override general_settings.debug. Enable debug mode for more verbose logging output.")

    # --- NEW/MODIFIED: CLI arguments for MESASDK_ROOT and GYRE_DIR ---
    parser.add_argument('--mesasdk-root', type=str,
                        help='Override MESASDK_ROOT path. (Environment variable MESASDK_ROOT or config.yaml).')
    parser.add_argument('--gyre-dir', type=str,
                        help='Override GYRE_DIR path. (Environment variable GYRE_DIR or config.yaml).')
    # --- END NEW/MODIFIED ---

    # Blue Loop Analysis Settings (can be overridden by CLI)
    parser.add_argument("--analyze-blue-loop", action="store_true",
                        help="Override blue_loop_analysis.analyze_blue_loop. Perform blue loop analysis.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'],
                        help="Override blue_loop_analysis.blue_loop_output_type. Blue loop output type for detail files.")

    # Plotting Settings (can be overridden by CLI)
    # The --no-plots flag is derived internally, so we don't need a direct CLI arg for 'generate_plots'
    parser.add_argument("--generate-heatmaps", action="store_true",
                        help="Override plotting_settings.generate_heatmaps. Generate heatmaps from cross-grid data.")
    parser.add_argument("--generate-hr-diagrams", type=str, choices=['none', 'all', 'drop_zams'],
                        help='Override plotting_settings.generate_hr_diagrams. Control HR diagram generation.')
    parser.add_argument("--generate-blue-loop-plots-with-bc", action="store_true",
                        help="Override plotting_settings.generate_blue_loop_plots_with_bc. Generate blue loop plots including bolometric corrections.")

    gyre_group = parser.add_argument_group('GYRE Workflow Settings')
    gyre_group.add_argument('--run-gyre-workflow', action='store_true',
                            help='Override gyre_workflow.run_gyre_workflow. Execute the full GYRE workflow.')
    gyre_group.add_argument('--gyre-run-mode', type=str, choices=['ALL_PROFILES', 'FILTERED_PROFILES'],
                            help='Override gyre_workflow.run_mode. Set the GYRE run mode.')
    gyre_group.add_argument('--gyre-threads', type=int,
                            help='Override gyre_workflow.num_gyre_threads. Number of OpenMP threads for each GYRE run.')
    # Using type=str and then converting manually for booleans, as argparse's bool parsing can be tricky
    gyre_group.add_argument('--gyre-parallel', type=str, choices=['True', 'False'],
                            help='Override gyre_workflow.enable_parallel. Enable/disable parallel GYRE runs (True/False).')
    gyre_group.add_argument('--gyre-max-concurrent', type=int,
                            help='Override gyre_workflow.max_concurrent_gyre_runs. Maximum number of concurrent GYRE runs.')
    gyre_group.add_argument('--gyre-inlist-template-path', type=str,
                            help='Override gyre_workflow.gyre_inlist_template_path. Full or relative path to the GYRE inlist template file.')

    # --- ÚJ: RSP Workflow Settings ---
    rsp_group = parser.add_argument_group('RSP Workflow Settings')
    rsp_group.add_argument('--run-rsp-workflow', action='store_true',
                           help='Override rsp_workflow.run_rsp_workflow. Execute the full RSP workflow.')
    rsp_group.add_argument('--rsp-inlist-template-path', type=str,
                           help='Override rsp_workflow.rsp_inlist_template_path. Full or relative path to the RSP inlist template file.')
    rsp_group.add_argument('--rsp-mesa-output-base-dir', type=str,
                           help='Override rsp_workflow.rsp_mesa_output_base_dir. Base directory for RSP MESA output files.')
    # --- VÉGE: RSP Workflow Settings ---


    # Parse arguments from the command line.
    cli_args, unknown_args = parser.parse_known_args()

    # Immediately set logging based on CLI debug flag. This is crucial for early debugging.
    # Set root logger level to capture all messages from now on
    if cli_args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
        logger.debug("Debug logging enabled via CLI in config_parser (early stage).")
    else:
        # Keep original logic if debug is false for early stage
        logger.info("Default logging level is INFO (early stage).")


    # 2. Define default configuration values with the desired nested structure
    default_config = {
        'general_settings': {
            'input_dir': None, # This will be explicitly validated later as required
            'output_dir': './mesalab_output',
            'inlist_name': 'inlist_project',
            'force_reanalysis': False,
            'debug': False,
            'mesasdk_root': None,
            'gyre_dir': None
        },
        'blue_loop_analysis': {
            'analyze_blue_loop': True,
            'blue_loop_output_type': 'all'
        },
        'plotting_settings': {
            'generate_heatmaps': False,
            'generate_hr_diagrams': 'none', # Default HR diagram generation
            'generate_blue_loop_plots_with_bc': False,
            'generate_plots': False # This will be derived later, not directly set by user
        },
        'gyre_workflow': {
            'run_gyre_workflow': True,
            'gyre_inlist_template_path': 'config/gyre.in', # This will be replaced by a path
            'run_mode': 'ALL_PROFILES', # Default mode
            'num_gyre_threads': 1, # Default to 1 thread
            'enable_parallel': False, # Default to sequential
            'max_concurrent_gyre_runs': 4, # Default max concurrent runs for parallel mode
            'filtered_profiles_csv_name': 'sorted_blue_loop_profiles.csv'
        },
        # --- ÚJ: RSP Workflow Defaults ---
        'rsp_workflow': {
            'run_rsp_workflow': False, # Default to disabled
            'rsp_inlist_template_path': 'config/rsp.inlist_template', # Example path
            'rsp_mesa_output_base_dir': './rsp_mesa_profiles' # Example output directory
        }
        # --- VÉGE: RSP Workflow Defaults ---
    }

    # Convert default_config to an Addict Dict for easier manipulation
    final_config_dict = Dict(default_config)

    # 3. Load user-provided YAML configuration
    user_yaml_config = {}
    config_file_path = cli_args.config # Get the YAML path from CLI arguments
    
    # Resolve config file path relative to current working directory
    resolved_config_file_path = os.path.abspath(config_file_path)

    if os.path.exists(resolved_config_file_path):
        try:
            with open(resolved_config_file_path, 'r') as f:
                user_yaml_config = yaml.safe_load(f)
                if user_yaml_config is None: # Handle empty YAML file
                    user_yaml_config = {}
            logger.info(f"Loaded configuration from '{resolved_config_file_path}'.")
        except yaml.YAMLError as e:
            logger.critical(f"Error parsing YAML configuration file '{resolved_config_file_path}': {e}")
            sys.exit(1)
        except Exception as e:
            logger.critical(f"An unexpected error occurred while loading config from '{resolved_config_file_path}': {e}")
            sys.exit(1)
    else:
        logger.warning(f"Configuration file '{resolved_config_file_path}' not found. Using default settings and CLI arguments.")

    # Apply YAML settings over defaults (deep merge if needed, but addict handles nested dicts naturally)
    # Using addict's update method for deep merge-like behavior if structure matches
    final_config_dict.update(user_yaml_config)


    # --- NEW/MODIFIED: Apply Environment Variables as an intermediate layer ---
    # These override defaults but are themselves overridden by CLI args
    env_mesasdk_root = os.getenv('MESASDK_ROOT')
    if env_mesasdk_root:
        final_config_dict.general_settings.mesasdk_root = env_mesasdk_root
        logger.debug(f"MESASDK_ROOT set from environment variable: {env_mesasdk_root}")
    
    env_gyre_dir = os.getenv('GYRE_DIR')
    if env_gyre_dir:
        final_config_dict.general_settings.gyre_dir = env_gyre_dir
        logger.debug(f"GYRE_DIR set from environment variable: {env_gyre_dir}")
    # --- END NEW/MODIFIED ---

    # 4. Apply CLI arguments, overriding YAML, Environment Variables, and defaults
    # Iterate through all CLI arguments and apply them to the final_config_dict
    for arg_action in parser._actions:
        arg_name = arg_action.dest
        cli_value = getattr(cli_args, arg_name, None)

        if arg_name == 'config':
            continue # Skip the config file path itself

        # Determine if CLI argument was explicitly provided by the user.
        # This logic is crucial to distinguish between 'None' (not set) and False/default values.
        # For store_true/store_false actions, if cli_value is NOT the action's default, it was explicitly set.
        # For other types, if cli_value is not None and not the action's default, it was explicitly set.
        cli_set_explicitly = False
        if isinstance(arg_action, argparse._StoreTrueAction) or isinstance(arg_action, argparse._StoreFalseAction):
            # For action='store_true', default is False. If it's True, it was set.
            # For action='store_false', default is True. If it's False, it was set.
            if cli_value is not None: # Means it was present in CLI args
                # If the current CLI value is different from its argparse default, it was explicitly provided
                if cli_value != arg_action.default:
                    cli_set_explicitly = True
        else:
            # For other argument types, if cli_value is not None and not the parser's default, it was explicitly set.
            # Note: arg_action.default is argparse's default, not our 'default_config'.
            if cli_value is not None and cli_value != arg_action.default:
                cli_set_explicitly = True

        if cli_set_explicitly:
            logger.debug(f"Applying CLI override: --{arg_name} = {cli_value}")
            if arg_name in ['input_dir', 'output_dir', 'inlist_name', 'force_reanalysis', 'debug',
                            'mesasdk_root', 'gyre_dir']:
                final_config_dict.general_settings[arg_name] = cli_value
            elif arg_name in ['analyze_blue_loop', 'blue_loop_output_type']:
                final_config_dict.blue_loop_analysis[arg_name] = cli_value
            elif arg_name in ['generate_heatmaps', 'generate_hr_diagrams', 'generate_blue_loop_plots_with_bc']:
                final_config_dict.plotting_settings[arg_name] = cli_value
            # --- MODIFIED: GYRE CLI MAPPINGS ---
            elif arg_name == 'run_gyre_workflow':
                final_config_dict.gyre_workflow.run_gyre_workflow = cli_value
            elif arg_name == 'gyre_run_mode':
                final_config_dict.gyre_workflow.run_mode = cli_value
            elif arg_name == 'gyre_threads':
                final_config_dict.gyre_workflow.num_gyre_threads = cli_value
            elif arg_name == 'gyre_parallel': # Custom handling for boolean string
                final_config_dict.gyre_workflow.enable_parallel = (cli_value.lower() == 'true')
            elif arg_name == 'gyre_max_concurrent':
                final_config_dict.gyre_workflow.max_concurrent_gyre_runs = cli_value
            elif arg_name == 'gyre_inlist_template_path':
                final_config_dict.gyre_workflow.gyre_inlist_template_path = cli_value
            # --- END MODIFIED GYRE ---
            # --- ÚJ: RSP CLI MAPPINGS ---
            elif arg_name == 'run_rsp_workflow':
                final_config_dict.rsp_workflow.run_rsp_workflow = cli_value
            elif arg_name == 'rsp_inlist_template_path':
                final_config_dict.rsp_workflow.rsp_inlist_template_path = cli_value
            elif arg_name == 'rsp_mesa_output_base_dir':
                final_config_dict.rsp_workflow.rsp_mesa_output_base_dir = cli_value
            # --- VÉGE: RSP CLI MAPPINGS ---
            else:
                logger.debug(f"CLI argument '{arg_name}' with value '{cli_value}' was provided but not explicitly mapped to a config setting. It will be ignored.")

    # --- NEW LOGIC START ---
    # Dynamically set 'generate_plots' based on whether any specific plotting option is enabled.
    final_config_dict.plotting_settings.generate_plots = (
        final_config_dict.plotting_settings.generate_heatmaps or
        final_config_dict.plotting_settings.generate_hr_diagrams != 'none' or
        final_config_dict.plotting_settings.generate_blue_loop_plots_with_bc
    )
    if final_config_dict.plotting_settings.generate_plots:
        logger.debug("Internal 'generate_plots' flag set to True as a specific plotting option is enabled.")
    # --- NEW LOGIC END ---

    # 5. Final validation for required arguments
    if final_config_dict.general_settings.input_dir is None:
        logger.critical("ERROR: 'input_dir' must be specified either via command-line (--input-dir) or in the config file.")
        sys.exit(1)

    # --- NEW: Final validation for GYRE workflow if enabled ---
    # ONLY perform GYRE specific validations if run_gyre_workflow is True
    if final_config_dict.gyre_workflow.get('run_gyre_workflow', False): # Use .get for safety
        logger.debug("GYRE workflow enabled. Performing final validation of GYRE parameters.")
        required_gyre_params = [
            'run_mode', 'gyre_inlist_template_path', 'num_gyre_threads',
            'enable_parallel', 'max_concurrent_gyre_runs'
        ]
        for param in required_gyre_params:
            # Using getattr with a default of None to safely check for existence
            if getattr(final_config_dict.gyre_workflow, param, None) is None:
                # Special check for 'enable_parallel' and 'max_concurrent_gyre_runs'
                # where a default of 0 or False might be intended, but None is an error.
                if param in ['num_gyre_threads', 'max_concurrent_gyre_runs'] and not isinstance(final_config_dict.gyre_workflow.get(param), (int, float)):
                    logger.critical(f"Missing or invalid required GYRE workflow parameter: 'gyre_workflow.{param}'. Please check config.yaml or CLI arguments.")
                    sys.exit(1)
                elif param == 'enable_parallel' and not isinstance(final_config_dict.gyre_workflow.get(param), bool):
                    logger.critical(f"Missing or invalid required GYRE workflow parameter: 'gyre_workflow.{param}'. Please check config.yaml or CLI arguments.")
                    sys.exit(1)
                else: # For other string/path params
                    logger.critical(f"Missing required GYRE workflow parameter: 'gyre_workflow.{param}'. Please check config.yaml or CLI arguments.")
                    sys.exit(1)
            # Additional validation for positive integers where applicable
            if param in ['num_gyre_threads', 'max_concurrent_gyre_runs'] and final_config_dict.gyre_workflow[param] <= 0:
                logger.critical(f"GYRE workflow parameter 'gyre_workflow.{param}' must be a positive integer.")
                sys.exit(1)
            
        gyre_template_path_to_check = final_config_dict.gyre_workflow.gyre_inlist_template_path
        if not os.path.isabs(gyre_template_path_to_check):
                # If it's a relative path, assume it's relative to the CWD where mesalab is run
            gyre_template_path_to_check = os.path.abspath(gyre_template_path_to_check)

        if not os.path.exists(gyre_template_path_to_check):
            logger.critical(f"GYRE inlist template file not found at: '{gyre_template_path_to_check}'. Please ensure the path is correct in your config or via CLI.")
            sys.exit(1)
    # --- END NEW GYRE VALIDATION ---

    # --- ÚJ: Final validation for RSP workflow if enabled ---
    if final_config_dict.rsp_workflow.get('run_rsp_workflow', False):
        logger.debug("RSP workflow enabled. Performing final validation of RSP parameters.")
        required_rsp_params = ['rsp_inlist_template_path', 'rsp_mesa_output_base_dir']
        for param in required_rsp_params:
            if getattr(final_config_dict.rsp_workflow, param, None) is None:
                logger.critical(f"Missing required RSP workflow parameter: 'rsp_workflow.{param}'. Please check config.yaml or CLI arguments.")
                sys.exit(1)
        
        rsp_template_path_to_check = final_config_dict.rsp_workflow.rsp_inlist_template_path
        if not os.path.isabs(rsp_template_path_to_check):
            rsp_template_path_to_check = os.path.abspath(rsp_template_path_to_check)

        if not os.path.exists(rsp_template_path_to_check):
            logger.critical(f"RSP inlist template file not found at: '{rsp_template_path_to_check}'. Please ensure the path is correct in your config or via CLI.")
            sys.exit(1)
        
        # Ensure rsp_mesa_output_base_dir is a valid path (even if it doesn't exist yet, parent should be writable)
        rsp_output_base_dir = final_config_dict.rsp_workflow.rsp_mesa_output_base_dir
        if not os.path.isabs(rsp_output_base_dir):
            rsp_output_base_dir = os.path.abspath(rsp_output_base_dir)
        
        # We don't check for existence of the output dir itself, but ensure its parent is writable
        parent_dir_rsp = os.path.dirname(rsp_output_base_dir)
        if not os.path.exists(parent_dir_rsp):
            logger.critical(f"Parent directory for RSP MESA output base directory does not exist: '{parent_dir_rsp}'. Please create it or provide a valid path.")
            sys.exit(1)
        if not os.access(parent_dir_rsp, os.W_OK):
            logger.critical(f"Parent directory for RSP MESA output base directory is not writable: '{parent_dir_rsp}'. Please check permissions.")
            sys.exit(1)

    # --- VÉGE: RSP Validation ---


    # 6. Return the final configuration (which is already an Addict Dict)
    # The conversion to argparse.Namespace is no longer needed if using Addict's Dict
    # The cli.py expects an object with attribute access, which Dict provides.
    
    # Ensure debug setting is applied to root logger and this module's logger after all config is merged
    if final_config_dict.general_settings.debug:
        logging.root.setLevel(logging.DEBUG)
        logger.setLevel(logging.DEBUG)
        logger.debug("Debug mode enabled after full config merge in config_parser.")
    else:
        # ORIGINAL LOGIC HERE: If not debug, set the root logger level back to INFO or higher for normal operation.
        # This will affect all loggers unless they have a more specific level set.
        # Ensure that this module's logger also respects the desired level for non-debug mode.
        # Based on your last request "NE VÁLTOZTASS DEBUG LEVELT!", leaving it as it was before my previous change.
        logger.info("Default logging level is INFO after full config merge in config_parser.") # This will show

    logger.info(f"Final resolved configuration: {final_config_dict}")
    return final_config_dict

# This block is for testing the config_parser.py module itself
if __name__ == '__main__':
    # Set up a basic console handler for standalone testing
    if not logging.getLogger().handlers:
        handler = logging.StreamHandler(sys.stdout)
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s: %(message)s')
        handler.setFormatter(formatter)
        logging.getLogger().addHandler(handler)
        logging.getLogger().setLevel(logging.DEBUG) # Default to DEBUG for standalone testing

    print("--- Testing config_parser.py ---")
    try:
        parsed_config = parsing_options()
        print("\n--- Parsed Config (Addict Dict) ---")
        print(parsed_config)
        print(f"Input Directory: {parsed_config.general_settings.input_dir}")
        print(f"MESASDK Root: {parsed_config.general_settings.mesasdk_root}")
        print(f"GYRE Directory: {parsed_config.general_settings.gyre_dir}")
        print(f"Analyze Blue Loop: {parsed_config.blue_loop_analysis.analyze_blue_loop}")
        print(f"Run GYRE Workflow: {parsed_config.gyre_workflow.run_gyre_workflow}")
        print(f"GYRE Run Mode: {parsed_config.gyre_workflow.run_mode}")
        print(f"GYRE Threads: {parsed_config.gyre_workflow.num_gyre_threads}")
        print(f"GYRE Parallel: {parsed_config.gyre_workflow.enable_parallel}")
        print(f"GYRE Max Concurrent: {parsed_config.gyre_workflow.max_concurrent_gyre_runs}")
        print(f"GYRE Inlist Template Name: {parsed_config.gyre_workflow.gyre_inlist_template_path}")
        print(f"Run RSP Workflow: {parsed_config.rsp_workflow.run_rsp_workflow}")
        print(f"RSP Inlist Template Path: {parsed_config.rsp_workflow.rsp_inlist_template_path}")
        print(f"RSP MESA Output Base Dir: {parsed_config.rsp_workflow.rsp_mesa_output_base_dir}")
        print(f"Generate Plots (internal): {parsed_config.plotting_settings.generate_plots}")
        print("\n--- Test complete ---")
    except Exception as e:
        logger.exception(f"Error during config_parser test: {e}")
        sys.exit(1)