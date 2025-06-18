# cli.py

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

# --- Module Imports with Fallback ---
# This block attempts to import core modules. If any fail, it creates
# "DummyModule" placeholders to allow the script to run partially and
# provide warnings instead of crashing immediately.
try:
    from mesalab.analyzis import mesa_analyzer
    from mesalab.plotting.all_hrd_plotter import generate_all_hr_diagrams as plot_hr_diagrams
    from mesalab.plotting.heatmap_generator import generate_heatmaps_and_time_diff_csv as plot_heatmaps
    # The blue loop summary plot is handled by the blue_loop_cmd_plotter, but its handler is in mesa_plotter (plot_handlers.py)
    # So we import the handler function directly.
    from mesalab.plotting.plot_handlers import handle_blue_loop_bc_plotting as plot_blue_loop_summary # Renamed mesa_plotter.py to plot_handlers.py in my mind

    from mesalab.gyretools import gyre_modules
    logger.debug("Core MESA/GYRE modules imported successfully.")
except ImportError as e:
    logger.error(f"Failed to import core MESA/GYRE modules. Some functionalities might be unavailable: {e}")
    logger.warning("If you encounter 'module not found' errors later, ensure your PYTHONPATH is configured correctly (e.g., by running 'pip install -e .' in your project root) or dependencies are installed.")
    
    # Define dummy placeholders if imports fail, to allow the script to run partially
    class DummyModule:
        def __getattr__(self, name):
            def dummy_func(*args, **kwargs):
                logger.warning(f"Function '{name}' from a missing module was called. Skipping operation.")
                # Return a default empty DataFrame or None, based on typical return types
                if 'df' in name or 'data' in name or 'summary' in name or 'history' in name:
                    return pd.DataFrame()
                elif 'path' in name:
                    return "" # Empty string for path
                else:
                    return None
            return dummy_func
            
    # Assign dummy objects if actual imports failed to prevent NameError later
    if 'mesa_analyzer' not in sys.modules:
        mesa_analyzer = DummyModule()
    if 'plot_hr_diagrams' not in sys.modules:
        plot_hr_diagrams = DummyModule()
    if 'plot_heatmaps' not in sys.modules:
        plot_heatmaps = DummyModule()
    if 'plot_blue_loop_summary' not in sys.modules:
        plot_blue_loop_summary = DummyModule()
    if 'gyre_modules' not in sys.modules:
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
        summary_df, combined_detail_data, full_history_data, gyre_input_csv_path = \
            mesa_analyzer.perform_mesa_analysis(
                config, # Pass the *entire* config Namespace object to mesa_analyzer
                analysis_results_sub_dir,
                detail_files_output_dir,
                # Access filtered_profiles_csv_name from gyre_workflow settings
                config.gyre_workflow.filtered_profiles_csv_name 
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
            # Access generate_heatmaps from plotting_settings
            if config.plotting_settings.generate_heatmaps:
                plot_heatmaps.generate_mass_z_heatmaps(summary_df, plots_output_dir)
            
            # Access generate_hr_diagrams from plotting_settings
            if config.plotting_settings.generate_hr_diagrams != 'none':
                plot_hr_diagrams.generate_all_hr_diagrams(
                    full_history_data,
                    plots_output_dir,
                    hr_diagram_type=config.plotting_settings.generate_hr_diagrams
                )
            
            # Access analyze_blue_loop from blue_loop_analysis
            if config.blue_loop_analysis.analyze_blue_loop:
                plot_blue_loop_summary.plot_blue_loop_data(
                    summary_df,
                    plots_output_dir,
                    # Access blue_loop_output_type from blue_loop_analysis
                    blue_loop_output_type=config.blue_loop_analysis.blue_loop_output_type,
                    # Access generate_blue_loop_plots_with_bc from plotting_settings
                    generate_plots_with_bc=config.plotting_settings.generate_blue_loop_plots_with_bc
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
