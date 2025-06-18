import sys
import os
import argparse
import logging
import datetime
import yaml
import pandas as pd # Added for potential DataFrame usage later if not already present
import numpy as np  # Added for potential numpy usage later if not already present

# Set up base logging configuration before any other imports that might log
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')

# Attempt to import GYRE related modules
try:
    from mesalab.gyre import gyre_modules
except ImportError:
    logging.warning("mesalab.gyre.gyre_modules not found in cli.py. Using a dummy placeholder.")
    # Create a dummy object if gyre_modules cannot be imported
    class DummyGyreModules:
        def __init__(self):
            logging.warning("Dummy gyre_modules will be used. GYRE workflow will be skipped.")
        def run_gyre_workflow(self, *args, **kwargs):
            logging.info("GYRE workflow skipped due to missing gyre_modules.")
            return pd.DataFrame() # Return empty DataFrame or suitable dummy
    gyre_modules = DummyGyreModules()

# Attempt to import MIST Bolometric Correction Grid related modules
try:
    # This assumes 'bcgrid' is a module you have or plan to use for BCs
    from mesalab.plotting.bc_grid import BCGrid
    BC_GRID_AVAILABLE = True
except ImportError:
    logging.warning("MIST Bolometric Correction Grid not imported. Some visualizations might not be available.")
    BC_GRID_AVAILABLE = False
    class DummyBCGrid:
        def __init__(self):
            pass
        def apply_bolometric_corrections(self, df):
            logging.warning("Bolometric corrections skipped: BCGrid not available.")
            return df # Return the original DataFrame unmodified
    BCGrid = DummyBCGrid


# Import other core modules
from mesalab.analyzis import mesa_analyzer
from mesalab.plotting import mesa_plotter


def setup_logging(output_dir, debug_mode):
    """
    Sets up the logging configuration, including a file handler.
    """
    # Ensure the output directory exists for the log file
    os.makedirs(output_dir, exist_ok=True)

    log_filename = datetime.datetime.now().strftime("mesagrid_run_%Y%m%d_%H%M%S.log")
    log_filepath = os.path.join(output_dir, log_filename)

    # Remove previous handlers to avoid duplicate logs if called multiple times
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)

    # Set base level for the root logger
    if debug_mode:
        logging.root.setLevel(logging.DEBUG)
    else:
        logging.root.setLevel(logging.INFO)

    # Console handler
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setFormatter(logging.Formatter('%(levelname)s: %(message)s'))
    logging.root.addHandler(console_handler)

    # File handler
    file_handler = logging.FileHandler(log_filepath)
    file_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(filename)s:%(lineno)d - %(message)s'))
    logging.root.addHandler(file_handler)

    logging.info("Logging set up. Log file: %s", log_filepath)


def main():
    """
    Main function to parse arguments, configure analysis, and execute workflows.
    """
    parser = argparse.ArgumentParser(description="MESA Grid Analysis Lab (mesalab)")
    parser.add_argument('--config', type=str, help='Path to the YAML configuration file.')
    parser.add_argument('--debug', action='store_true', help='Enable debug logging.')

    cli_args = parser.parse_args()

    # Default configuration
    config = {
        'general_settings': {
            'input_dir': './',
            'output_dir': './output',
            'inlist_name': 'inlist_project',
            'force_reanalysis': False,
            'debug': False # Default to False, can be overridden by CLI or YAML
        },
        'blue_loop_analysis': {
            'analyze_blue_loop': True,
            'blue_loop_output_type': 'all' # 'all' or 'summary'
        },
        'plotting_settings': {
            'generate_heatmaps': True,
            'generate_plots': True,
            'generate_hr_diagrams': 'drop_zams', # 'full', 'drop_zams', 'none'
            'generate_blue_loop_plots_with_bc': False # Requires BCGrid to be available
        },
        'gyre_workflow': {
            'run_gyre_workflow': False,
            'gyre_config_path': 'gyre_config.in',
            'gyre_input_csv_name': 'sorted_mass_Z_min_max.csv'
        }
    }

    # Load configuration from YAML if provided
    if cli_args.config:
        try:
            with open(cli_args.config, 'r') as f:
                yaml_config = yaml.safe_load(f)
            # Merge YAML config into default config
            for key, value in yaml_config.items():
                if key in config and isinstance(config[key], dict) and isinstance(value, dict):
                    config[key].update(value)
                else:
                    config[key] = value
            logging.info(f"Loaded configuration from: {cli_args.config}")
        except FileNotFoundError:
            logging.error(f"Configuration file not found: {cli_args.config}. Using default settings.")
        except yaml.YAMLError as e:
            logging.error(f"Error parsing YAML configuration file: {e}. Using default settings.")

    # Override debug setting if --debug flag is used
    if cli_args.debug:
        config['general_settings']['debug'] = True

    # Setup logging with the determined debug mode and output directory
    debug_mode = config['general_settings']['debug']
    output_dir = config['general_settings']['output_dir']
    setup_logging(output_dir, debug_mode)

    if debug_mode:
        logging.debug("Debug mode enabled.")
        logging.debug(f"Full configuration dictionary: {config}")

    # Convert config dictionary to a Namespace object for easier access,
    # mimicking argparse.Namespace attributes.
    # This also flattens the dictionary for simplicity in passing to functions.
    config_for_analyzer = argparse.Namespace()
    for section_key, section_dict in config.items():
        for item_key, item_value in section_dict.items():
            setattr(config_for_analyzer, item_key, item_value)

    if debug_mode:
        logging.debug(f"Config object for analyzer: {config_for_analyzer}")

    logging.info("\n--- Starting MESA Grid Analysis ---")

    # Define output subdirectories
    analysis_results_sub_dir = os.path.join(output_dir, "analysis_results")
    blue_loop_plots_dir = os.path.join(output_dir, "blue_loop_plots")
    heatmap_output_dir = os.path.join(output_dir, "heatmaps")
    hr_diagrams_output_dir = os.path.join(output_dir, "hr_diagrams")
    detail_files_output_dir = os.path.join(output_dir, "blue_loop_detail_files") # For detailed blue loop CSVs

    # Ensure output directories exist
    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    os.makedirs(blue_loop_plots_dir, exist_ok=True)
    os.makedirs(heatmap_output_dir, exist_ok=True)
    os.makedirs(hr_diagrams_output_dir, exist_ok=True)
    os.makedirs(detail_files_output_dir, exist_ok=True) # Ensure this directory exists

    # --- Call the MESA Analysis Workflow ---
    # This is the corrected call to the function in mesa_analyzer.py
    summary_df, combined_detail_data, full_history_data, gyre_input_csv_path = \
        mesa_analyzer.perform_mesa_analysis(
            config_for_analyzer,
            analysis_results_sub_dir,
            detail_files_output_dir, # Pass the detail_files_output_dir
            config_for_analyzer.gyre_input_csv_name # Pass the GYRE input CSV name
        )

    if summary_df.empty:
        logging.info("No summary data generated from MESA analysis. Skipping plotting and GYRE workflow.")
        sys.exit(0) # Exit gracefully if no data to process further

    # --- Plotting Workflow ---
    if config_for_analyzer.generate_plots:
        logging.info("\n--- Starting Plotting Workflow ---")
        if config_for_analyzer.generate_heatmaps:
            logging.info("Generating heatmaps...")
            mesa_plotter.handle_heatmap_generation(
                config_for_analyzer,
                summary_df, # summary_df_for_plotting, passed as a placeholder/if needed by handler
                heatmap_output_dir, # plots_sub_dir
                analysis_results_sub_dir, # analysis_results_sub_dir
                config_for_analyzer.input_dir # input_dir for model_name
            )
            logging.info("Heatmaps generated.")
        if config_for_analyzer.generate_hr_diagrams != 'none' and not full_history_data:
             logging.warning("Cannot generate HR diagrams: No full history data available. This might happen if 'force_reanalysis' was False and data was loaded, but full history was not retained for plotting.")
        elif config_for_analyzer.generate_hr_diagrams != 'none':
            logging.info("Generating HR Diagrams...")
            mesa_plotter.handle_hr_diagram_generation(
                config_for_analyzer,
                hr_diagrams_output_dir,
                full_history_data,
                config_for_analyzer.generate_hr_diagrams == 'drop_zams' # Pass True if 'drop_zams' requested
            )
            logging.info("HR Diagrams generated.")
            
        if config_for_analyzer.analyze_blue_loop and config_for_analyzer.generate_blue_loop_plots_with_bc: # Added check for generate_blue_loop_plots_with_bc
            logging.info("Generating Blue Loop CMD/HRD with Bolometric Corrections...")
            mesa_plotter.handle_blue_loop_bc_plotting(
                config_for_analyzer,
                combined_detail_data, # This is the DataFrame from mesa_analyzer
                blue_loop_plots_dir, # Output directory for these plots
                detail_files_output_dir # Directory where individual detail files might be if loading is needed
            )
            logging.info("Blue Loop CMD/HRD with Bolometric Corrections generated.")
            
        elif config_for_analyzer.analyze_blue_loop and not config_for_analyzer.generate_blue_loop_plots_with_bc: # If blue loop analysis is on but plotting is off
            logging.info("Blue loop analysis is enabled, but 'generate_blue_loop_plots_with_bc' is False. Skipping blue loop BC plots.")
        else: # If blue loop analysis is completely off
            logging.info("Blue loop analysis is disabled. Skipping all blue loop related plots.")        elif config_for_analyzer.analyze_blue_loop and combined_detail_data.empty:
            logging.info("No detailed blue loop data available for plotting. Skipping blue loop HR diagrams.")
        else:
            logging.info("Blue loop analysis is off. Skipping blue loop HR diagrams.")

    # --- GYRE Workflow ---
    if config_for_analyzer.run_gyre_workflow and gyre_modules: # Check if gyre_modules is not the Dummy
        if gyre_input_csv_path and os.path.exists(gyre_input_csv_path):
            logging.info("\n--- Starting GYRE Workflow ---")
            logging.info(f"Using GYRE configuration file: {config_for_analyzer.gyre_config_path}")
            gyre_modules.run_gyre_workflow(
                gyre_input_csv_path,
                config_for_analyzer.gyre_config_path,
                output_dir,
                config_for_analyzer.debug # Pass debug flag to GYRE workflow
            )
            logging.info("GYRE workflow completed.")
        else:
            logging.warning("GYRE input CSV not found. Skipping GYRE workflow.")
    elif config_for_analyzer.run_gyre_workflow and not gyre_modules:
        logging.warning("GYRE modules not available. Skipping GYRE workflow.")
    else:
        logging.info("GYRE workflow is disabled in configuration.")

    logging.info("\n--- MESA Grid Analysis Finished ---")

if __name__ == '__main__':
    main()
