# mesalab/cli.py

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


# --- Logging Setup (Initial - Manual Configuration for cli.py) ---
# Get the root logger
root_logger = logging.getLogger()
# Remove any existing handlers to prevent duplicate output (e.g., from implicit basicConfig calls)
# This loop is crucial to prevent duplicate messages if basicConfig was called elsewhere implicitly
for handler in root_logger.handlers[:]:
    if isinstance(handler, logging.StreamHandler) and handler.stream == sys.stdout:
        root_logger.removeHandler(handler)

# Add a StreamHandler to output to console (only one now)
console_handler = logging.StreamHandler(sys.stdout)
formatter = logging.Formatter('%(asctime)s - %(levelname)s: %(message)s')
console_handler.setFormatter(formatter)
root_logger.addHandler(console_handler)

# Suppress matplotlib font warnings, if any (often verbose)
logging.getLogger('matplotlib').setLevel(logging.WARNING)

# Logger for cli.py itself (distinct from root_logger for finer control)
cli_logger = logging.getLogger(__name__)
cli_logger.setLevel(logging.WARNING) # Default for cli_logger itself, adjusted later


# --- Module Imports with Fallback (Corrected paths based on your files) ---

# Flag to track if GYRE modules were successfully imported
_GYRE_MODULES_LOADED = False

try:
    cli_logger.debug("Attempting to import core mesalab modules...")
    
    from mesalab.analyzis.mesa_analyzer import perform_mesa_analysis as mesa_analyzer
    from mesalab.plotting.mesa_plotter import handle_heatmap_generation
    from mesalab.plotting.mesa_plotter import handle_hr_diagram_generation
    from mesalab.plotting.mesa_plotter import handle_blue_loop_bc_plotting

    # Corrected import: import the specific function directly
    from mesalab.gyretools.gyre_modules import run_gyre_workflow
    _GYRE_MODULES_LOADED = True
    cli_logger.debug("Successfully imported all core mesalab modules, including GYRE.")

except ImportError as e:
    cli_logger.error(f"Failed to import core MESA/GYRE modules due to ImportError: {e}", exc_info=True)
    print(f"FATAL ERROR: Failed to import critical mesalab/GYRE modules at startup. Please check installation and dependencies.", file=sys.stderr)
    print(f"Error details: {e}", file=sys.stderr)
    print("Full traceback (printed to stderr):", file=sys.stderr)
    import traceback
    traceback.print_exc(file=sys.stderr)
    sys.exit(1)

except Exception as e:
    cli_logger.critical(f"CRITICAL UNEXPECTED ERROR during initial module imports: {e}", exc_info=True)
    print(f"FATAL ERROR: An unexpected critical error occurred during initial module imports: {e}", file=sys.stderr)
    print("Full traceback (printed to stderr):", file=sys.stderr)
    import traceback
    traceback.print_exc(file=sys.stderr)
    sys.exit(1)


class DummyModule:
    def __getattr__(self, name):
        def dummy_func(*args, **kwargs):
            cli_logger.warning(f"Function '{name}' from a missing/unavailable module was called. Skipping operation.")
            return None
        return dummy_func
        
if not _GYRE_MODULES_LOADED:
    # If GYRE modules failed to load, replace run_gyre_workflow with a dummy
    # This prevents NameError if GYRE is called later.
    run_gyre_workflow = DummyModule().dummy_func # Assign the specific dummy function
    cli_logger.warning("GYRE modules could not be loaded. GYRE workflow will be skipped.")
    # No need to set overall_workflow_success here, as main() will handle it if GYRE workflow is enabled and skipped.
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

    overall_workflow_success = True 

    try:
        mesalab_version = pkg_resources.get_distribution('mesalab').version
    except pkg_resources.DistributionNotFound:
        mesalab_version = "N/A (not installed as package)"

    print(f"\n{'#'*80}")
    print(f"### {'mesalab CLI - Starting Analysis Workflow':^72} ###")
    print(f"### {'Version: ' + mesalab_version:^72} ###")
    print(f"{'#'*80}\n")

    # Parse command line arguments and load config
    config = config_parser.parsing_options()

    config_paths.set_environment_variables_for_executables(config)

    cli_logger.debug(f"Starting main application logic. Raw CLI arguments: {sys.argv[1:]}")
    
    # --- Final Logging Setup (based on parsed config) ---
    if config.general_settings.debug:
        root_logger.setLevel(logging.DEBUG) # Set root logger to DEBUG
        cli_logger.setLevel(logging.DEBUG)  # Set cli_logger to DEBUG
        cli_logger.debug("Debug logging confirmed and enabled by final configuration.")
    else:
        # If not debug, set the root logger level to INFO for balanced output
        root_logger.setLevel(logging.INFO) # Set root logger to INFO (for general pipeline messages)
        cli_logger.setLevel(logging.WARNING) # cli_logger specific output can be less verbose
        # cli_logger.info("Debug mode is OFF. General logging level is INFO.") # Removed, as it won't show and is verbose
    
    # This message is now debug-level, so it only shows if debug is true
    cli_logger.debug(f"Final resolved configuration being used by cli.py: {config}")


    # --- Output Directory Setup ---
    # Ensure all output directories are absolute paths for clarity and reliability
    output_base_dir = os.path.abspath(config.general_settings.output_dir)
    session_output_dir = output_base_dir # This can be made more dynamic if needed for multiple sessions
    analysis_results_sub_dir = os.path.join(session_output_dir, 'analysis_results')
    detail_files_output_dir = os.path.join(session_output_dir, 'detail_files')
    plots_output_dir = os.path.join(session_output_dir, 'plots')
    # The gyre_output_dir here might be slightly redundant as gyre_modules.py handles its own sub-dir
    # relative to global_output_base_dir. It's fine to keep it for consistency if desired.

    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    os.makedirs(detail_files_output_dir, exist_ok=True)
    os.makedirs(plots_output_dir, exist_ok=True)
    cli_logger.info(f"All primary outputs for this session will be saved in: '{session_output_dir}'")

    # Initialize gyre_input_csv_path to None or a default if not generated by analysis
    gyre_input_csv_path = None

    # --- MESA Analysis Workflow ---
    print(f"\n{'='*70}")
    print(f"    Starting MESA Analysis Workflow...")
    print(f"{'='*70}\n")
    try:
        # Determine if gyre_input_csv_name should be passed to mesa_analyzer
        # It should only be passed if GYRE workflow is enabled and in 'FILTERED_PROFILES' mode
        gyre_cfg_exists = hasattr(config, 'gyre_workflow') and config.gyre_workflow is not None
        gyre_workflow_enabled_for_analysis_csv = False
        gyre_csv_name_to_pass = None

        if gyre_cfg_exists and config.gyre_workflow.get('run_gyre_workflow', False):
            if config.gyre_workflow.get('run_mode', '').upper() == 'FILTERED_PROFILES':
                gyre_csv_name_to_pass = config.gyre_workflow.get('filtered_profiles_csv_name', None)
                if gyre_csv_name_to_pass: # Only enable if name is actually provided
                    gyre_workflow_enabled_for_analysis_csv = True
                else:
                    cli_logger.warning("GYRE workflow is enabled in FILTERED_PROFILES mode, but 'filtered_profiles_csv_name' is missing in config. Will not generate GYRE input CSV from analysis.")


        summary_df, combined_detail_data, full_history_data, gyre_input_csv_path = \
            mesa_analyzer(
                args=config, # mesa_analyzer still takes 'args', which is your config object
                analysis_results_sub_dir=analysis_results_sub_dir,
                detail_files_output_dir=detail_files_output_dir,
                # Pass gyre_csv_name_to_pass ONLY if GYRE workflow is correctly configured to use it for CSV generation
                gyre_input_csv_name=gyre_csv_name_to_pass if gyre_workflow_enabled_for_analysis_csv else None
            )
        if summary_df.empty:
            cli_logger.info("No summary data generated from MESA analysis. Skipping subsequent steps.")
            overall_workflow_success = False # No data generated is a non-success
            # sys.exit(0) # Consider if this should be exit(0) or just a return to allow other workflows
            return # Exit main() gracefully if no data
        print(f"\n{'='*70}")
        print(f"    MESA Analysis Workflow Completed Successfully.")
        print(f"{'='*70}\n")
    except Exception as e:
        cli_logger.critical(f"Critical error during MESA Analysis workflow: {e}", exc_info=True)
        overall_workflow_success = False # MESA analysis failure is critical
        sys.exit(1)


    # --- Plotting Workflow ---
    # Check if 'generate_plots' exists and is True, or if any specific plotting flag is True
    # If plotting_settings section itself is missing, assume no plotting
    plotting_enabled = getattr(config, 'plotting_settings', None) and \
                       (config.plotting_settings.get('generate_plots', False) or \
                        config.plotting_settings.get('generate_heatmaps', False) or \
                        config.plotting_settings.get('generate_hr_diagrams', 'none') != 'none' or \
                        config.plotting_settings.get('generate_blue_loop_plots_with_bc', False))

    if plotting_enabled:
        print(f"\n{'='*70}")
        print(f"    Starting Plotting Workflow...")
        print(f"{'='*70}\n")
        try:
            if config.plotting_settings.get('generate_heatmaps', False):
                handle_heatmap_generation(
                    args=config,
                    summary_df_for_plotting=summary_df,
                    plots_sub_dir=plots_output_dir,
                    analysis_results_sub_dir=analysis_results_sub_dir,
                    input_dir=config.general_settings.input_dir
                )
            
            if config.plotting_settings.get('generate_hr_diagrams', 'none') != 'none':
                handle_hr_diagram_generation(
                    args=config,
                    plots_sub_dir=plots_output_dir,
                    full_history_data_for_plotting=full_history_data,
                    drop_zams=(config.plotting_settings.generate_hr_diagrams == 'drop_zams')
                )
            
            # Only run blue loop plotting if blue_loop_analysis is enabled AND plotting for it is enabled
            if getattr(config, 'blue_loop_analysis', None) and \
               config.blue_loop_analysis.get('analyze_blue_loop', False) and \
               config.plotting_settings.get('generate_blue_loop_plots_with_bc', False):
                handle_blue_loop_bc_plotting(
                    args=config,
                    combined_detail_data_for_plotting=combined_detail_data,
                    blue_loop_plots_bc_sub_dir=plots_output_dir,
                    detail_files_output_dir=detail_files_output_dir
                )
            print(f"\n{'='*70}")
            print(f"    Plotting Workflow Completed Successfully.")
            print(f"{'='*70}\n")
        except Exception as e:
            cli_logger.error(f"Error during plotting workflow: {e}", exc_info=True)
            # Plotting error is not critical enough to stop the whole process as unsuccessful
            # but you can change this if you want plotting failures to mark the run as failed.
            # overall_workflow_success = False 
            # sys.exit(1) 
    else:
        print(f"\n{'='*70}")
        print(f"    Plotting workflow is disabled in configuration.")
        print(f"{'='*70}\n")


    # --- GYRE Workflow ---
    # Use .get() with a default to safely check if 'gyre_workflow' section exists and is enabled
    if config.gyre_workflow.get('run_gyre_workflow', False): 
        if _GYRE_MODULES_LOADED:
            print(f"\n{'='*70}")
            print(f"    Starting GYRE Workflow...")
            print(f"{'='*70}\n")
            
            # Check if filtered profiles are needed but not available
            if config.gyre_workflow.get('run_mode', '').upper() == 'FILTERED_PROFILES' and \
                (not gyre_input_csv_path or not os.path.exists(gyre_input_csv_path)):
                cli_logger.warning(f"GYRE workflow enabled in 'FILTERED_PROFILES' mode, but the filtered profiles CSV ('{config.gyre_workflow.get('filtered_profiles_csv_name', 'N/A')}') "
                                   f"was not generated or not found at '{gyre_input_csv_path}'. "
                                   f"This is required for GYRE to run on filtered profiles. Skipping GYRE workflow.")
                print(f"\n{'='*70}")
                print(f"    GYRE Workflow Skipped: Required input CSV not found.")
                print(f"{'='*70}\n")
                overall_workflow_success = False # GYRE skipped means not fully successful
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
                    print(f"    GYRE Workflow Completed Successfully.")
                    print(f"{'='*70}\n")
                except Exception as e:
                    cli_logger.critical(f"Critical error during GYRE workflow: {e}", exc_info=True)
                    print(f"\n{'='*70}")
                    print(f"    GYRE Workflow Encountered an Error.")
                    print(f"{'='*70}\n")
                    overall_workflow_success = False # GYRE runtime error means not fully successful
                    # sys.exit(1) # Consider adding sys.exit(1) here if a GYRE runtime error should terminate immediately
        else:
            print(f"\n{'='*70}")
            print(f"    GYRE workflow is enabled in configuration, but GYRE modules failed to load at startup. Skipping GYRE workflow.")
            print(f"{'='*70}\n")
            overall_workflow_success = False # GYRE module load failure means not fully successful
    else:
        print(f"\n{'='*70}")
        print(f"    GYRE workflow is disabled in configuration (run_gyre_workflow=False).")
        print(f"{'='*70}\n")

    # --- END OF RUN (using print for clear visibility) ---
    print(f"\n{'#'*80}")
    if overall_workflow_success:
        print(f"### {'mesalab Workflow Finished Successfully!':^72} ###")
    else:
        print(f"### {'mesalab Workflow Completed with Errors/Skipped Steps!':^72} ###")
    print(f"{'#'*80}\n")

# --- Entry Point ---
if __name__ == '__main__':
    main()