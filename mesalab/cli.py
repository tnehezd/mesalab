# mesalab/cli.py - REVISED with "nice" print statements for workflow stages

import sys
import os
import logging
import datetime
import pandas as pd
import numpy as np
import pkg_resources # Import pkg_resources to get package version

# Import config_parser module, which now handles ALL argument parsing and config loading
from mesalab.io import config_parser
from mesalab import config_paths


# --- Logging Setup (Initial) ---
logging.basicConfig(
    level=logging.ERROR, # Set this to ERROR to only show errors by default
    format='%(asctime)s - %(levelname)s: %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout) # Log to console
    ]
)
# Suppress matplotlib font warnings, if any (often verbose)
logging.getLogger('matplotlib').setLevel(logging.WARNING)
logger = logging.getLogger(__name__) # Logger for cli.py itself

# --- Module Imports with Fallback (Corrected paths based on your files) ---

# Flag to track if GYRE modules were successfully imported
_GYRE_MODULES_LOADED = False

try:
    logger.debug("Attempting to import core mesalab modules...")
    
    from mesalab.analyzis.mesa_analyzer import perform_mesa_analysis as mesa_analyzer
    from mesalab.plotting.mesa_plotter import handle_heatmap_generation
    from mesalab.plotting.mesa_plotter import handle_hr_diagram_generation
    from mesalab.plotting.mesa_plotter import handle_blue_loop_bc_plotting

    # Corrected import: import the specific function directly
    from mesalab.gyretools.gyre_modules import run_gyre_workflow
    _GYRE_MODULES_LOADED = True
    logger.debug("Successfully imported all core mesalab modules, including GYRE.")

except ImportError as e:
    logger.error(f"Failed to import core MESA/GYRE modules due to ImportError: {e}", exc_info=True)
    print(f"FATAL ERROR: Failed to import critical mesalab/GYRE modules at startup. Please check installation and dependencies.", file=sys.stderr)
    print(f"Error details: {e}", file=sys.stderr)
    print("Full traceback (printed to stderr):", file=sys.stderr)
    import traceback
    traceback.print_exc(file=sys.stderr)
    sys.exit(1)

except Exception as e:
    logger.critical(f"CRITICAL UNEXPECTED ERROR during initial module imports: {e}", exc_info=True)
    print(f"FATAL ERROR: An unexpected critical error occurred during initial module imports: {e}", file=sys.stderr)
    print("Full traceback (printed to stderr):", file=sys.stderr)
    import traceback
    traceback.print_exc(file=sys.stderr)
    sys.exit(1)


class DummyModule:
    def __getattr__(self, name):
        def dummy_func(*args, **kwargs):
            logger.warning(f"Function '{name}' from a missing/unavailable module was called. Skipping operation.")
            return None
        return dummy_func
        
if not _GYRE_MODULES_LOADED:
    # If GYRE modules failed to load, replace run_gyre_workflow with a dummy
    # This prevents NameError if GYRE is called later.
    run_gyre_workflow = DummyModule().dummy_func # Assign the specific dummy function
    logger.warning("GYRE modules could not be loaded. GYRE workflow will be skipped.")
else:
    pass


# --- Main Application Logic ---
def main():
    """
    Main entry point for the MESA/GYRE analysis and plotting pipeline.
    This function parses configuration, sets up environment variables,
    manages output directories, and orchestrates the MESA analysis,
    plotting, and GYRE pulsation analysis workflows based on the configuration.
    """
    
    # --- START OF RUN (using print for clear visibility) ---
    try:
        mesalab_version = pkg_resources.get_distribution('mesalab').version
    except pkg_resources.DistributionNotFound:
        mesalab_version = "N/A (not installed as package)"

    print(f"\n{'#'*80}")
    print(f"### {'mesalab CLI - Starting Analysis Workflow':^72} ###")
    print(f"### {'Version: ' + mesalab_version:^72} ###")
    print(f"{'#'*80}\n")

    # Parse command line arguments and load config (twice? config_parser.parsing_options() is called twice, remove one)
    config = config_parser.parsing_options()


    config_paths.set_environment_variables_for_executables(config)

    logger.debug(f"Starting main application logic. Raw CLI arguments: {sys.argv[1:]}")
    
    # config = config_parser.parsing_options() # This line is redundant, parsing_options() was called above

    # --- Final Logging Setup (based on parsed config) ---
    if config.general_settings.debug:
        logging.getLogger().setLevel(logging.DEBUG)
        logger.debug("Debug logging confirmed and enabled by final configuration.")
    else:
        # If not debug, set the root logger level back to INFO or higher for normal operation,
        # unless it's explicitly set to ERROR for `cli.py` itself as per initial setup.
        # It's usually better to let sub-modules control their own logging.
        # Here, we set the root logger to INFO for more general messages.
        logging.getLogger().setLevel(logging.INFO) 
        logger.info("Debug mode is OFF. General logging level is INFO.") # This message will now show
    
    # For cli.py itself, we can keep it at ERROR if we only want critical messages from here.
    # Otherwise, it should also be INFO or DEBUG. Let's keep it at INFO for this example.
    logger.setLevel(logging.INFO) 
    logger.info(f"Final resolved configuration being used by cli.py: {config}")

    # --- Output Directory Setup ---
    # Ensure all output directories are absolute paths for clarity and reliability
    output_base_dir = os.path.abspath(config.general_settings.output_dir)
    session_output_dir = output_base_dir # This can be made more dynamic if needed for multiple sessions
    analysis_results_sub_dir = os.path.join(session_output_dir, 'analysis_results')
    detail_files_output_dir = os.path.join(session_output_dir, 'detail_files')
    plots_output_dir = os.path.join(session_output_dir, 'plots')
    # The gyre_output_dir here might be slightly redundant as gyre_modules.py handles its own sub-dir
    # relative to global_output_base_dir. It's fine to keep it for consistency if desired.
    gyre_output_dir_placeholder = os.path.join(session_output_dir, 'gyre_output') 

    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    os.makedirs(detail_files_output_dir, exist_ok=True)
    os.makedirs(plots_output_dir, exist_ok=True)
    os.makedirs(gyre_output_dir_placeholder, exist_ok=True) # Create this as a parent for GYRE output
    logger.info(f"All primary outputs for this session will be saved in: '{session_output_dir}'")

    # Initialize gyre_input_csv_path to None or a default if not generated by analysis
    gyre_input_csv_path = None

    # --- MESA Analysis Workflow ---
    print(f"\n{'='*70}")
    print(f"  Starting MESA Analysis Workflow...")
    print(f"{'='*70}\n")
    try:
        summary_df, combined_detail_data, full_history_data, gyre_input_csv_path = \
            mesa_analyzer(
                args=config, # mesa_analyzer still takes 'args', which is your config object
                analysis_results_sub_dir=analysis_results_sub_dir,
                detail_files_output_dir=detail_files_output_dir,
                gyre_input_csv_name=config.gyre_workflow.filtered_profiles_csv_name
            )
        if summary_df.empty:
            logger.info("No summary data generated from MESA analysis. Skipping subsequent steps.")
            # sys.exit(0) # Consider if this should be exit(0) or just a return to allow other workflows
            return # Exit main() gracefully if no data
        print(f"\n{'='*70}")
        print(f"  MESA Analysis Workflow Completed Successfully.")
        print(f"{'='*70}\n")
    except Exception as e:
        logger.critical(f"Critical error during MESA Analysis workflow: {e}", exc_info=True)
        sys.exit(1)


    # --- Plotting Workflow ---
    if config.plotting_settings.generate_plots:
        print(f"\n{'='*70}")
        print(f"  Starting Plotting Workflow...")
        print(f"{'='*70}\n")
        try:
            if config.plotting_settings.generate_heatmaps:
                handle_heatmap_generation(
                    args=config,
                    summary_df_for_plotting=summary_df,
                    plots_sub_dir=plots_output_dir,
                    analysis_results_sub_dir=analysis_results_sub_dir,
                    input_dir=config.general_settings.input_dir
                )
            
            if config.plotting_settings.generate_hr_diagrams != 'none':
                handle_hr_diagram_generation(
                    args=config,
                    plots_sub_dir=plots_output_dir,
                    full_history_data_for_plotting=full_history_data,
                    drop_zams=(config.plotting_settings.generate_hr_diagrams == 'drop_zams')
                )
            
            if config.blue_loop_analysis.analyze_blue_loop:
                handle_blue_loop_bc_plotting(
                    args=config,
                    combined_detail_data_for_plotting=combined_detail_data,
                    blue_loop_plots_bc_sub_dir=plots_output_dir,
                    detail_files_output_dir=detail_files_output_dir
                )
            print(f"\n{'='*70}")
            print(f"  Plotting Workflow Completed Successfully.")
            print(f"{'='*70}\n")
        except Exception as e:
            logger.error(f"Error during plotting workflow: {e}", exc_info=True)
    else:
        print(f"\n{'='*70}")
        print(f"  Plotting workflow is disabled in configuration (generate_plots=False).")
        print(f"{'='*70}\n")


    # --- GYRE Workflow ---
    if config.gyre_workflow.run_gyre_workflow:
        if _GYRE_MODULES_LOADED:
            print(f"\n{'='*70}")
            print(f"  Starting GYRE Workflow...")
            print(f"{'='*70}\n")
            
            # The 'gyre_config_full_path' variable is no longer needed here.
            # The run_gyre_workflow function directly uses the template name from config.
            
            # Check if filtered profiles are needed but not available
            if config.gyre_workflow.run_mode.upper() == 'FILTERED_PROFILES' and \
                (not gyre_input_csv_path or not os.path.exists(gyre_input_csv_path)):
                logger.warning(f"GYRE workflow enabled in 'FILTERED_PROFILES' mode, but the filtered profiles CSV ('{config.gyre_workflow.filtered_profiles_csv_name}') "
                               f"was not generated or not found at '{gyre_input_csv_path}'. "
                               "This is required for GYRE to run on filtered profiles. Skipping GYRE workflow.")
                print(f"\n{'='*70}")
                print(f"  GYRE Workflow Skipped: Required input CSV not found.")
                print(f"{'='*70}\n")
            else:
                try:
                    # Pass the entire config object directly to run_gyre_workflow
                    # gyre_modules.run_gyre_workflow will extract its needed parameters from this config
                    run_gyre_workflow(
                        config_data=config, # Pass the entire config object
                        filtered_profiles_csv_path=gyre_input_csv_path, # This is still needed for FILTERED_PROFILES mode
                        debug_mode=config.general_settings.debug
                    )
                    print(f"\n{'='*70}")
                    print(f"  GYRE Workflow Completed Successfully.")
                    print(f"{'='*70}\n")
                except Exception as e:
                    logger.critical(f"Critical error during GYRE workflow: {e}", exc_info=True)
                    print(f"\n{'='*70}")
                    print(f"  GYRE Workflow Encountered an Error.")
                    print(f"{'='*70}\n")
        else:
            print(f"\n{'='*70}")
            print(f"  GYRE workflow is enabled in configuration, but GYRE modules failed to load at startup. Skipping GYRE workflow.")
            print(f"{'='*70}\n")
    else:
        print(f"\n{'='*70}")
        print(f"  GYRE workflow is disabled in configuration (run_gyre_workflow=False).")
        print(f"{'='*70}\n")

    # --- END OF RUN (using print for clear visibility) ---
    print(f"\n{'#'*80}")
    print(f"### {'mesalab Workflow Finished Successfully!':^72} ###")
    print(f"{'#'*80}\n")

# --- Entry Point ---
if __name__ == '__main__':
    main()