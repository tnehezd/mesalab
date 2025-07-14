# mesalab/io/config_parser.py

import os
import argparse
import yaml
import sys
import logging
from datetime import datetime

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
        argparse.Namespace: A nested Namespace object containing the final resolved configuration.
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

    # --- NEW: CLI arguments for MESASDK_ROOT and GYRE_DIR ---
    parser.add_argument('--mesasdk-root', type=str,
                        help='Override MESASDK_ROOT path. (Environment variable MESASDK_ROOT or config.yaml).')
    parser.add_argument('--gyre-dir', type=str,
                        help='Override GYRE_DIR path. (Environment variable GYRE_DIR or config.yaml).')
    # --- END NEW ---

    # Blue Loop Analysis Settings (can be overridden by CLI)
    parser.add_argument("--analyze-blue-loop", action="store_true",
                        help="Override blue_loop_analysis.analyze_blue_loop. Perform blue loop analysis.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'],
                        help="Override blue_loop_analysis.blue_loop_output_type. Blue loop output type for detail files.")

    # Plotting Settings (can be overridden by CLI)
    parser.add_argument("--generate-heatmaps", action="store_true",
                        help="Override plotting_settings.generate_heatmaps. Generate heatmaps from cross-grid data.")
    parser.add_argument("--generate-hr-diagrams", type=str, choices=['none', 'all', 'drop_zams'],
                        help='Override plotting_settings.generate_hr_diagrams. Control HR diagram generation.')
    parser.add_argument("--generate-blue-loop-plots-with-bc", action="store_true",
                        help="Override plotting_settings.generate_blue_loop_plots_with_bc. Generate blue loop plots including bolometric corrections.")

    # GYRE Workflow Settings (can be overridden by CLI)
    parser.add_argument('--run-gyre-workflow', action='store_true',
                        help='Override gyre_workflow.run_gyre_workflow. Execute the full GYRE workflow.')
    parser.add_argument('--gyre-config-path', type=str,
                        help='Override gyre_workflow.gyre_config_path. Path to the GYRE-specific configuration file.')
    parser.add_argument('--filtered-profiles-csv-name', type=str,
                        help='Override gyre_workflow.filtered_profiles_csv_name. Custom name for the GYRE input CSV.')

    # Parse arguments from the command line.
    cli_args, unknown_args = parser.parse_known_args()

    # Immediately set logging based on CLI debug flag. This is crucial for early debugging.
    if cli_args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
        logger.debug("Debug logging enabled via CLI in config_parser.")

    # 2. Define default configuration values with the desired nested structure
    default_config = {
        'general_settings': {
            'input_dir': None, # This will be explicitly validated later as required
            'output_dir': './mesalab_output',
            'inlist_name': 'inlist_project',
            'force_reanalysis': False,
            'debug': False,
            'mesasdk_root': None, # --- NEW DEFAULT ---
            'gyre_dir': None     # --- NEW DEFAULT ---
        },
        'blue_loop_analysis': {
            'analyze_blue_loop': True,
            'blue_loop_output_type': 'all'
        },
        'plotting_settings': {
            'generate_heatmaps': False,
            'generate_hr_diagrams': 'none', # Default HR diagram generation
            'generate_blue_loop_plots_with_bc': False
        },
        'gyre_workflow': {
            'run_gyre_workflow': True,
            'gyre_config_path': 'gyre_config.in',
            'filtered_profiles_csv_name': 'sorted_mass_Z_min_max.csv'
        }
    }

    # 3. Load user-provided YAML configuration
    user_yaml_config = {}
    config_file_path = cli_args.config # Get the YAML path from CLI arguments
    if os.path.exists(config_file_path):
        try:
            with open(config_file_path, 'r') as f:
                user_yaml_config = yaml.safe_load(f)
            logger.info(f"Loaded configuration from '{config_file_path}'.")
        except yaml.YAMLError as e:
            logger.critical(f"Error parsing YAML configuration file '{config_file_path}': {e}")
            sys.exit(1)
    else:
        logger.warning(f"Configuration file '{config_file_path}' not found. Using default settings and CLI arguments.")

    # 4. Merge default, YAML, and CLI settings
    # Deep merge function to handle nested dictionaries
    def deep_merge(target, source):
        for key, value in source.items():
            if key in target and isinstance(target[key], dict) and isinstance(value, dict):
                target[key] = deep_merge(target[key], value)
            else:
                target[key] = value
        return target

    # Start with default settings
    final_config_dict = default_config.copy() # Use a copy to avoid modifying the original default_config

    # Apply YAML settings over defaults
    final_config_dict = deep_merge(final_config_dict, user_yaml_config)

    # --- NEW: Apply Environment Variables as an intermediate layer ---
    # Values from YAML config or CLI will override these later.
    env_mesasdk_root = os.getenv('MESASDK_ROOT')
    if env_mesasdk_root:
        final_config_dict['general_settings']['mesasdk_root'] = env_mesasdk_root
        logger.debug(f"MESASDK_ROOT set from environment variable: {env_mesasdk_root}")
    
    env_gyre_dir = os.getenv('GYRE_DIR')
    if env_gyre_dir:
        final_config_dict['general_settings']['gyre_dir'] = env_gyre_dir
        logger.debug(f"GYRE_DIR set from environment variable: {env_gyre_dir}")
    # --- END NEW ---

    # Apply CLI arguments, overriding YAML, Environment Variables, and defaults
    for arg_action in parser._actions:
        arg_name = arg_action.dest
        cli_value = getattr(cli_args, arg_name, None)

        if arg_name == 'config':
            continue
        
        # Determine if CLI argument was explicitly provided by the user
        is_boolean_flag = isinstance(arg_action, (argparse._StoreTrueAction, argparse._StoreFalseAction))
        cli_set_explicitly = False
        if is_boolean_flag:
            # For booleans, if cli_value is different from the arg's *default* value, it was set.
            # (Note: This default is argparse's default, not our `default_config`'s).
            if cli_value is not None: # cli_value would be True/False if set, None if not in cli.
                 cli_set_explicitly = (cli_value != arg_action.default)
        else:
            # For non-booleans, if cli_value is not None and is different from argparse's default, it was set.
            if cli_value is not None and cli_value != arg_action.default:
                cli_set_explicitly = True


        if cli_set_explicitly:
            if arg_name in ['input_dir', 'output_dir', 'inlist_name', 'force_reanalysis', 'debug',
                            'mesasdk_root', 'gyre_dir']: # --- NEW CLI MAPPINGS ---
                final_config_dict['general_settings'][arg_name] = cli_value
            elif arg_name in ['analyze_blue_loop', 'blue_loop_output_type']:
                final_config_dict['blue_loop_analysis'][arg_name] = cli_value
            elif arg_name in ['generate_heatmaps', 'generate_hr_diagrams', 'generate_blue_loop_plots_with_bc']:
                final_config_dict['plotting_settings'][arg_name] = cli_value
            elif arg_name in ['run_gyre_workflow', 'gyre_config_path', 'filtered_profiles_csv_name']:
                final_config_dict['gyre_workflow'][arg_name] = cli_value
            else:
                logger.debug(f"CLI argument '{arg_name}' with value '{cli_value}' was provided but not explicitly mapped to a config setting. It will be ignored.")

    # --- NEW LOGIC START ---
    # Dynamically set 'generate_plots' based on whether any specific plotting option is enabled.
    # This means 'generate_plots' is no longer a user-configurable option via CLI/YAML,
    # but an internal flag derived from other plotting settings.
    final_config_dict['plotting_settings']['generate_plots'] = (
        final_config_dict['plotting_settings']['generate_heatmaps'] or
        final_config_dict['plotting_settings']['generate_hr_diagrams'] != 'none' or
        final_config_dict['plotting_settings']['generate_blue_loop_plots_with_bc']
    )
    if final_config_dict['plotting_settings']['generate_plots']:
        logger.debug("Internal 'generate_plots' flag set to True as a specific plotting option is enabled.")
    # --- NEW LOGIC END ---

    # 5. Final validation for required arguments
    if final_config_dict['general_settings']['input_dir'] is None:
        logger.critical("ERROR: 'input_dir' must be specified either via command-line (--input-dir) or in the config file.")
        sys.exit(1)

    # 6. Convert the final dictionary to an argparse.Namespace for easy attribute access
    def dict_to_namespace(d):
        for k, v in d.items():
            if isinstance(v, dict):
                d[k] = dict_to_namespace(v)
        return argparse.Namespace(**d)

    final_namespace = dict_to_namespace(final_config_dict)

    # Ensure debug setting is applied to root logger and this module's logger
    if final_namespace.general_settings.debug:
        logging.root.setLevel(logging.DEBUG)
        logger.setLevel(logging.DEBUG)
        logger.debug("Debug mode enabled after full config merge.")
    else:
        # Default to WARNING if not debug, to keep output clean unless issues arise
        logging.root.setLevel(logging.WARNING) 
        logger.setLevel(logging.WARNING)

    logger.info(f"Final resolved configuration: {final_namespace}")
    return final_namespace

# This block is for testing the config_parser.py module itself
if __name__ == '__main__':
    # You can test this module by running:
    # python -m mesalab.io.config_parser --config your_config.yaml -i /path/to/override --debug
    # Or test with env vars:
    # MESASDK_ROOT=/tmp/sdk GYRE_DIR=/tmp/gyre python -m mesalab.io.config_parser --config your_config.yaml -i /path/to/input
    
    print("--- Testing config_parser.py ---")
    parsed_config = parsing_options()
    print("\n--- Parsed Config (Namespace) ---")
    print(parsed_config)
    print(f"Input Directory: {parsed_config.general_settings.input_dir}")
    print(f"MESASDK Root: {parsed_config.general_settings.mesasdk_root}")
    print(f"GYRE Directory: {parsed_config.general_settings.gyre_dir}")
    print(f"Analyze Blue Loop: {parsed_config.blue_loop_analysis.analyze_blue_loop}")
    print(f"Run GYRE Workflow: {parsed_config.gyre_workflow.run_gyre_workflow}")
    print(f"Generate Plots (internal): {parsed_config.plotting_settings.generate_plots}")
