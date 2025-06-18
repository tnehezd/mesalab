# cli.py - REVISED for correct plotting handler calls

import sys
import os
import logging
import datetime
import pandas as pd
import numpy as np

# Import config_parser module, which now handles ALL argument parsing and config loading
from mesalab.io import config_parser

# --- Logging Setup (Initial) ---
# This basic configuration ensures logs appear from the start.
# The level will be adjusted later by config_parser based on the 'debug' setting.
logging.basicConfig(
    level=logging.INFO, # Default level, will be overridden by config_parser based on config.general_settings.debug
    format='%(asctime)s - %(levelname)s: %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout) # Log to console
    ]
)
# Suppress matplotlib font warnings, if any (often verbose)
logging.getLogger('matplotlib').setLevel(logging.WARNING)
logger = logging.getLogger(__name__) # Logger for cli.py itself

# --- Module Imports with Fallback (Corrected paths based on your files) ---
try:
    # Correct import for mesa_analyzer (which is perform_mesa_analysis from mesa_analyzer.py)
    from mesalab.analyzis.mesa_analyzer import perform_mesa_analysis as mesa_analyzer

    # Correct imports for plotting HANDLER functions from mesa_plotter.py (your plot_handlers.py)
    # These handlers will then call the specific plotting functions (like generate_heatmaps_and_time_diff_csv)
    from mesalab.plotting.mesa_plotter import handle_heatmap_generation
    from mesalab.plotting.mesa_plotter import handle_hr_diagram_generation
    from mesalab.plotting.mesa_plotter import handle_blue_loop_bc_plotting

    from mesalab.gyretools import gyre_modules # Assuming this import is correctly set up now
except ImportError as e:
    logger.error(f"Failed to import core MESA/GYRE modules. Some functionalities might be unavailable: {e}")
    logger.warning("If you encounter 'module not found' errors later, ensure your PYTHONPATH is configured correctly (e.g., by running 'pip install -e .' in your project root) or dependencies are installed.")
    logger.warning("Using dummy modules for missing MESA/GYRE components.")

    # Define dummy modules to prevent NameError later if imports fail
    class DummyModule:
        def __getattr__(self, name):
            def dummy_func(*args, **kwargs):
                logger.warning(f"Function '{name}' from a missing module was called. Skipping operation.")
                return None # Return None or appropriate dummy value
            return dummy_func
            
    # Assign dummy objects if actual imports failed to prevent NameError later
    mesa_analyzer = DummyModule()
    handle_heatmap_generation = DummyModule()
    handle_hr_diagram_generation = DummyModule()
    handle_blue_loop_bc_plotting = DummyModule()
    gyre_modules = DummyModule()
    logger.warning("Using dummy modules for missing MESA/GYRE components.")


# --- Main Application Logic ---
def main():
    logger.debug(f"Starting main application logic. Raw CLI arguments: {sys.argv[1:]}")
    
    # Call config_parser.parsing_options(). This function now handles all argument parsing,
    # merging with YAML, and setting defaults, returning a nested Namespace object.
    config = config_parser.parsing_options()

    # --- Final Logging Setup (based on parsed config) ---
    # The config_parser sets the root logger level. We ensure cli.py's logger and
    # any other child loggers also reflect the final resolved 'debug' setting.
    if config.general_settings.debug:
        logging.getLogger().setLevel(logging.DEBUG) # Set cli.py's logger to DEBUG
        logger.debug("Debug logging confirmed and enabled by final configuration.")
    else:
        logging.getLogger().setLevel(logging.INFO) # Ensure cli.py's logger is INFO if not debug
        logger.info("Debug mode is OFF. Logging level for cli.py is INFO.") # Clarify INFO level

    logger.info(f"Final resolved configuration being used by cli.py: {config}")

    # --- Output Directory Setup ---
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    # Access output_dir from general_settings
    output_base_dir = os.path.abspath(config.general_settings.output_dir)
    session_output_dir = os.path.join(output_base_dir, f"mesa_analysis_{timestamp}")
    analysis_results_sub_dir = os.path.join(session_output_dir, 'analysis_results')
    detail_files_output_dir = os.path.join(session_output_dir, 'detail_files')
    plots_output_dir = os.path.join(session_output_dir, 'plots')
    gyre_output_dir = os.path.join(session_output_dir, 'gyre_output') # GYRE specific output dir

    # Create all necessary output directories
    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    os.makedirs(detail_files_output_dir, exist_ok=True)
    os.makedirs(plots_output_dir, exist_ok=True)
    os.makedirs(gyre_output_dir, exist_ok=True)
    logger.info(f"All outputs for this session will be saved in: '{session_output_dir}'")

    # --- MESA Analysis Workflow ---
    logger.info("\n--- Starting MESA Analysis Workflow ---")
    try:
        # Call the perform_mesa_analysis function directly
        summary_df, combined_detail_data, full_history_data, gyre_input_csv_path = \
            mesa_analyzer( # No '.perform_mesa_analysis' here, call directly
                args=config, # Pass the entire config object as 'args'
                analysis_results_sub_dir=analysis_results_sub_dir,
                detail_files_output_dir=detail_files_output_dir,
                gyre_input_csv_name=config.gyre_workflow.filtered_profiles_csv_name
            )
        if summary_df.empty:
            logger.info("No summary data generated from MESA analysis. Skipping subsequent steps.")
            sys.exit(0)
        logger.info("MESA Analysis workflow completed successfully.")
    except Exception as e:
        logger.critical(f"Critical error during MESA Analysis workflow: {e}", exc_info=True)
        sys.exit(1)


    # --- Plotting Workflow ---
    # Access generate_plots from plotting_settings
    if config.plotting_settings.generate_plots:
        logger.info("\n--- Starting Plotting Workflow ---")
        try:
            # Call the heatmap handler
            if config.plotting_settings.generate_heatmaps:
                handle_heatmap_generation(
                    args=config,
                    summary_df_for_plotting=summary_df, # The summary_df is still passed, though not used by handler's current logic
                    plots_sub_dir=plots_output_dir,
                    analysis_results_sub_dir=analysis_results_sub_dir,
                    input_dir=config.general_settings.input_dir # Need input_dir for model_name
                )
            
            # Call the HR diagrams handler
            if config.plotting_settings.generate_hr_diagrams != 'none':
                handle_hr_diagram_generation(
                    args=config, # Pass the entire config object as 'args'
                    plots_sub_dir=plots_output_dir,
                    full_history_data_for_plotting=full_history_data,
                    drop_zams=(config.plotting_settings.generate_hr_diagrams == 'drop_zams') # Pass boolean
                )
            
            # Call the blue loop plotting handler
            if config.blue_loop_analysis.analyze_blue_loop: # Use blue_loop_analysis setting here
                handle_blue_loop_bc_plotting(
                    args=config, # Pass the entire config object as 'args'
                    combined_detail_data_for_plotting=combined_detail_data,
                    blue_loop_plots_bc_sub_dir=plots_output_dir, # Plots go to plots_output_dir
                    detail_files_output_dir=detail_files_output_dir
                )
            logger.info("Plotting workflow completed successfully.")
        except Exception as e:
            logger.error(f"Error during plotting workflow: {e}", exc_info=True)
    else:
        logger.info("Plotting workflow is disabled in configuration (generate_plots=False).")


    # --- GYRE Workflow ---
    # Access run_gyre_workflow from gyre_workflow settings
    # The 'isinstance' check verifies if gyre_modules was successfully imported or is a DummyModule
    if config.gyre_workflow.run_gyre_workflow and not isinstance(gyre_modules, type(sys.modules.get('mesalab.gyretools.gyre_modules', object()))):
        logger.info("\n--- Starting GYRE Workflow ---")
        # Access gyre_config_path from gyre_workflow settings
        gyre_config_full_path = os.path.abspath(config.gyre_workflow.gyre_config_path)
        
        # Validate GYRE config file path
        if not os.path.exists(gyre_config_full_path):
            logger.error(f"GYRE configuration file not found at '{gyre_config_full_path}'. Skipping GYRE workflow.")
        # Validate filtered profiles CSV (required if blue loop analysis is enabled)
        # Access analyze_blue_loop from blue_loop_analysis
        # Access filtered_profiles_csv_name from gyre_workflow
        elif config.blue_loop_analysis.analyze_blue_loop and \
             (not gyre_input_csv_path or not os.path.exists(gyre_input_csv_path)):
            logger.warning(f"GYRE workflow enabled, but the filtered profiles CSV ('{config.gyre_workflow.filtered_profiles_csv_name}') "
                           f"was not generated or not found at '{gyre_input_csv_path}'. "
                           "This is required for GYRE to run on filtered profiles. Skipping GYRE workflow.")
        else:
            logger.info(f"Using GYRE specific settings from: '{gyre_config_full_path}'")
            try:
                gyre_modules.run_gyre_workflow(
                    gyre_config_path=gyre_config_full_path,
                    filtered_profiles_csv_path=gyre_input_csv_path, # Path to the CSV generated by mesa_analyzer
                    # Access input_dir from general_settings
                    global_mesa_base_dir=os.path.abspath(config.general_settings.input_dir), 
                    global_output_base_dir=gyre_output_dir,
                    # Access debug from general_settings
                    debug_mode=config.general_settings.debug
                )
                logger.info("GYRE workflow completed successfully.")
            except Exception as e:
                logger.critical(f"Critical error during GYRE workflow: {e}", exc_info=True)
    # This block is for when run_gyre_workflow is True, but gyre_modules import failed earlier
    elif config.gyre_workflow.run_gyre_workflow:
        logger.warning("GYRE workflow is enabled, but GYRE modules were not fully available (ImportError detected earlier). Skipping GYRE workflow.")
    else:
        logger.info("GYRE workflow is disabled in configuration (run_gyre_workflow=False).")

    logger.info("\n--- MESA Grid Analysis and Workflow Finished ---")

# --- Entry Point ---
if __name__ == '__main__':
    main()
