# mesalab/cli.py

import sys
import os
import logging
import datetime
import pandas as pd
import numpy as np
import pkg_resources # Import pkg_resources to get package version

# Import config_parser module, which handles ALL argument parsing and config loading
from mesalab.io import config_parser
from mesalab.io import config_paths


# --- Logging Setup (Initial - Manual Configuration for cli.py) ---
# Get the root logger
root_logger = logging.getLogger()

# Crucially, set the root logger to DEBUG immediately.
# This ensures that all loggers that inherit from root (like those in data_reader, grid_analyzer)
# will be able to emit DEBUG messages from the very beginning of the script's execution,
# even before the full config is parsed.
root_logger.setLevel(logging.DEBUG)

# Remove any existing handlers to prevent duplicate output (e.g., from implicit basicConfig calls)
# This loop is crucial to prevent duplicate messages if basicConfig was called elsewhere implicitly.
for handler in root_logger.handlers[:]:
    if isinstance(handler, logging.StreamHandler) and handler.stream == sys.stdout:
        root_logger.removeHandler(handler)

# Add a StreamHandler to output to console (only one now)
console_handler = logging.StreamHandler(sys.stdout)
formatter = logging.Formatter('%(asctime)s - %(levelname)s: %(message)s')
console_handler.setFormatter(formatter)
root_logger.addHandler(console_handler)

# Suppress verbose warnings from external libraries like matplotlib and numba
logging.getLogger('matplotlib').setLevel(logging.WARNING)
logging.getLogger('numba').setLevel(logging.WARNING)


# Logger for cli.py itself (distinct from root_logger for finer control)
cli_logger = logging.getLogger(__name__)
# Set cli_logger to DEBUG initially as well to capture early debug messages specific to cli.py
cli_logger.setLevel(logging.DEBUG)


# --- Module Imports with Fallback ---

# Flag to track if GYRE modules were successfully imported
_GYRE_MODULES_LOADED = False

try:
    cli_logger.debug("Attempting to import core mesalab modules...")
    
    # Import main analysis, plotting, and GYRE workflow functions
    from mesalab.analyzis.mesa_analyzer import perform_mesa_analysis as mesa_analyzer
    from mesalab.plotting.mesa_plotter import handle_heatmap_generation
    from mesalab.plotting.mesa_plotter import handle_hr_diagram_generation
    from mesalab.plotting.mesa_plotter import handle_blue_loop_bc_plotting

    # Import the specific GYRE workflow function directly
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
    sys.exit(1) # Exit if critical modules cannot be imported

except Exception as e:
    cli_logger.critical(f"CRITICAL UNEXPECTED ERROR during initial module imports: {e}", exc_info=True)
    print(f"FATAL ERROR: An unexpected critical error occurred during initial module imports: {e}", file=sys.stderr)
    print("Full traceback (printed to stderr):", file=sys.stderr)
    import traceback
    traceback.print_exc(file=sys.stderr)
    sys.exit(1) # Exit for any other unexpected error during imports


# --- Import RSP workflow module with fallback ---
_RSP_MODULES_LOADED = False
try:
    cli_logger.debug("Attempting to import RSP workflow module...")
    from mesalab.rsptools.rsp_runner import run_mesa_rsp_workflow # The RSP runner function
    _RSP_MODULES_LOADED = True
    cli_logger.debug("Successfully imported RSP workflow module.")
except ImportError as e:
    cli_logger.error(f"Failed to import RSP workflow module due to ImportError: {e}", exc_info=True)
    cli_logger.warning("RSP workflow modules could not be loaded. RSP workflow will be skipped if enabled.")
    # Define a dummy function for run_mesa_rsp_workflow to prevent NameError
    def run_mesa_rsp_workflow(inlist_paths, mesa_dir, max_workers):
        cli_logger.error("Attempted to call run_mesa_rsp_workflow, but the module failed to load. Skipping RSP runs.")
        return {'successful': [], 'failed': inlist_paths, 'timeout': [], 'error': [{'inlist': p, 'error': 'Module not loaded'} for p in inlist_paths]}
except Exception as e:
    cli_logger.critical(f"CRITICAL UNEXPECTED ERROR during RSP module import: {e}", exc_info=True)
    cli_logger.warning("RSP workflow modules could not be loaded due to unexpected error. RSP workflow will be skipped if enabled.")
    def run_mesa_rsp_workflow(inlist_paths, mesa_dir, max_workers):
        cli_logger.error("Attempted to call run_mesa_rsp_workflow, but an unexpected error occurred during import. Skipping RSP runs.")
        return {'successful': [], 'failed': inlist_paths, 'timeout': [], 'error': [{'inlist': p, 'error': 'Unexpected module import error'} for p in inlist_paths]}


class DummyModule:
    """A dummy class to provide dummy functions for modules that fail to load."""
    def __getattr__(self, name):
        def dummy_func(*args, **kwargs):
            cli_logger.warning(f"Function '{name}' from a missing/unavailable module was called. Skipping operation.")
            # For functions expected to return a status (like run_gyre_workflow), return a non-zero code for failure.
            return 1 
        return dummy_func
        
if not _GYRE_MODULES_LOADED:
    # If GYRE modules failed to load, replace run_gyre_workflow with a dummy
    # This prevents NameError if GYRE is called later and ensures it returns a failure status.
    run_gyre_workflow = DummyModule().dummy_func # Assign the specific dummy function
    cli_logger.warning("GYRE modules could not be loaded. GYRE workflow will be skipped and marked as failed if enabled.")
else:
    # If GYRE modules loaded, do nothing (pass)
    pass


# --- Main Application Logic ---
def main():
    """
    Main entry point for the MESA/GYRE/RSP analysis and plotting pipeline.
    This function parses configuration, sets up environment variables,
    manages output directories, and orchestrates the MESA analysis,
    plotting, and pulsation analysis workflows based on the configuration.
    """
    
    # --- START OF RUN (using print for clear visibility) ---

    overall_workflow_success = True # Flag to track overall success of the entire mesalab workflow

    try:
        # Get mesalab package version for display
        mesalab_version = pkg_resources.get_distribution('mesalab').version
    except pkg_resources.DistributionNotFound:
        mesalab_version = "N/A (not installed as package)"

    # Print initial banner to console
    print(f"\n{'='*80}")
    print(f"{'mesalab CLI - Starting Analysis Workflow':^80}")
    print(f"{'Version: ' + mesalab_version:^80}")
    print(f"{'='*80}\n")

    # Parse command line arguments and load configuration from YAML
    config = config_parser.parsing_options()

    # Set up environment variables required for external executables (like GYRE)
    # IMPORTANT: This call must happen *before* attempting to resolve mesa_star_dir,
    # as it populates config.general_settings.mesasdk_root using $MESA_DIR if available.
    config_paths.set_environment_variables_for_executables(config)

    cli_logger.debug(f"Starting main application logic. Raw CLI arguments: {sys.argv[1:]}")
    
    # --- Final Logging Setup (based on parsed config) ---
    # Now, we only adjust the logging level if 'debug' is explicitly FALSE in the config.
    # The default is already DEBUG from the top of the script.
    if not config.general_settings.get('debug', False): # Use .get() for safety
        # If debug is false, downgrade the root logger and cli_logger to INFO/WARNING
        root_logger.setLevel(logging.INFO) # Set root logger to INFO for general pipeline messages
        cli_logger.setLevel(logging.WARNING) # cli_logger specific output can be less verbose
        cli_logger.info("Debug mode is OFF. General logging level is INFO.") 
    else:
        cli_logger.debug("Debug logging confirmed and enabled by final configuration.")
        
    cli_logger.debug(f"Final resolved configuration being used by cli.py: {config}")


    # --- Output Directory Setup ---
    # Ensure all output directories are absolute paths for clarity and reliability
    output_base_dir = os.path.abspath(config.general_settings.output_dir)
    session_output_dir = output_base_dir # This can be made more dynamic if needed for multiple sessions
    analysis_results_sub_dir = os.path.join(session_output_dir, 'analysis_results')
    detail_files_output_dir = os.path.join(session_output_dir, 'detail_files')
    plots_output_dir = os.path.join(session_output_dir, 'plots')

    # Define and create the dedicated RSP output directory ONLY if RSP workflow is enabled
    rsp_output_dir = None # Initialize to None
    mesa_star_dir = None # Initialize MESA star directory path for RSP workflow

    if hasattr(config, 'rsp_workflow') and config.rsp_workflow.get('run_rsp_workflow', False):
        rsp_output_dir = os.path.join(output_base_dir, 'mesa_rsp_runs')
        os.makedirs(rsp_output_dir, exist_ok=True)
        cli_logger.info(f"Dedicated RSP output directory created: '{rsp_output_dir}'")
        
        # --- Attempt to get or auto-detect mesa_star_dir for RSP workflow ---
        # First, try to get it explicitly from config.yaml
        mesa_star_dir = config.general_settings.get('mesa_star_dir') 
        
        if not mesa_star_dir:
            cli_logger.info("'mesa_star_dir' not explicitly set in config. Attempting to auto-detect from MESASDK_ROOT.")
            
            # MESASDK_ROOT should have been populated by config_paths.set_environment_variables_for_executables()
            # either from config.yaml or $MESA_DIR environment variable.
            mesasdk_root_path = config.general_settings.get('mesasdk_root') 
            
            if mesasdk_root_path and os.path.isdir(mesasdk_root_path):
                potential_mesa_dirs = []
                for item in os.listdir(mesasdk_root_path):
                    full_path = os.path.join(mesasdk_root_path, item)
                    # Look for directories starting with 'mesa-r' and containing a 'star' subdir
                    if os.path.isdir(full_path) and item.startswith('mesa-r'):
                        test_star_path = os.path.join(full_path, 'star')
                        if os.path.isdir(test_star_path):
                            potential_mesa_dirs.append(test_star_path)
                
                if len(potential_mesa_dirs) == 1:
                    mesa_star_dir = potential_mesa_dirs[0]
                    cli_logger.info(f"Auto-detected 'mesa_star_dir': {mesa_star_dir}")
                elif len(potential_mesa_dirs) > 1:
                    cli_logger.critical(f"ERROR: Multiple MESA 'star' directories found within MESASDK_ROOT ('{mesasdk_root_path}'). "
                                        f"Cannot auto-detect. Please set 'mesa_star_dir' explicitly in config.yaml from these options: {potential_mesa_dirs}")
                    overall_workflow_success = False
                    sys.exit(1)
                else: # No 'mesa-r*/star' directories found
                    cli_logger.critical(f"ERROR: 'mesa_star_dir' is not defined in config.yaml and no MESA 'star' directories found within MESASDK_ROOT ('{mesasdk_root_path}') for auto-detection. "
                                        f"This is required for the RSP workflow. Please set 'mesa_star_dir' explicitly in config.yaml.")
                    overall_workflow_success = False
                    sys.exit(1)
            else: # MESASDK_ROOT is not set or invalid (even after checking $MESA_DIR)
                cli_logger.critical(f"ERROR: 'mesa_star_dir' is not defined in config.yaml and auto-detection from 'mesasdk_root' failed. "
                                    f"'mesasdk_root' is either not set or points to an invalid directory: '{mesasdk_root_path}'. "
                                    f"This is required for the RSP workflow.")
                overall_workflow_success = False
                sys.exit(1)
        else: # mesa_star_dir WAS explicitly set in config
            if not os.path.isdir(mesa_star_dir):
                cli_logger.critical(f"ERROR: The explicitly provided 'mesa_star_dir' does not exist or is not a directory: '{mesa_star_dir}'. This is required for the RSP workflow.")
                overall_workflow_success = False
                sys.exit(1)
            cli_logger.debug(f"MESA 'star' directory configured from config.yaml: '{mesa_star_dir}'")


    # Create general output subdirectories
    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    os.makedirs(detail_files_output_dir, exist_ok=True)
    os.makedirs(plots_output_dir, exist_ok=True)
    cli_logger.info(f"All primary outputs for this session will be saved in: '{session_output_dir}'")

    # Initialize variables that will be populated by mesa_analyzer
    gyre_input_csv_path = None
    generated_rsp_inlists_paths = [] # This will hold the list of generated RSP inlist paths
    generated_cross_csv_paths = []


    # --- MESA Analysis Workflow ---
    print(f"\n{'='*70}")
    print(f"       Starting MESA Analysis Workflow...")
    print(f"{'='*70}\n")
    try:
        # Determine if gyre_input_csv_name should be passed to mesa_analyzer.
        # It should only be passed if GYRE workflow is enabled and in 'FILTERED_PROFILES' mode.
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

        # Perform MESA analysis, which includes scanning runs and (optionally) generating RSP inlists
        # generated_rsp_inlists_paths will be populated here if RSP inlist generation is enabled in mesa_analyzer.
        summary_df, combined_detail_data, full_history_data, gyre_input_csv_path, generated_rsp_inlists_paths, generated_cross_csv_paths = \
            mesa_analyzer(
                args=config, # mesa_analyzer still takes 'args', which is your config object
                analysis_results_sub_dir=analysis_results_sub_dir,
                detail_files_output_dir=detail_files_output_dir,
                # Pass gyre_csv_name_to_pass ONLY if GYRE workflow is correctly configured to use it for CSV generation
                gyre_input_csv_name=gyre_csv_name_to_pass if gyre_workflow_enabled_for_analysis_csv else None,
                rsp_mesa_output_base_dir=rsp_output_dir # Passed here to mesa_analyzer for inlist generation
            )
        # Check if MESA analysis produced any data
        if summary_df.empty:
            cli_logger.info("No summary data generated from MESA analysis. Skipping subsequent steps.")
            overall_workflow_success = False # Mark overall workflow as unsuccessful
            sys.exit(1) # Critical early exit if no data is generated for further processing

        print(f"\n{'='*70}")
        print(f"       MESA Analysis Workflow Completed Successfully.")
        print(f"{'='*70}\n")

    except Exception as e:
        cli_logger.critical(f"Critical error during MESA Analysis workflow: {e}", exc_info=True)
        overall_workflow_success = False # Mark overall workflow as unsuccessful
        sys.exit(1) # Critical exit for MESA analysis failures


    # --- MESA RSP Workflow ---
    # Check if RSP workflow is enabled in configuration AND if RSP modules loaded AND if inlists were generated
    if hasattr(config, 'rsp_workflow') and config.rsp_workflow.get('run_rsp_workflow', False):
        if not _RSP_MODULES_LOADED:
            cli_logger.warning("RSP workflow is enabled in config, but RSP modules failed to load at startup. Skipping RSP workflow.")
            print(f"\n{'='*70}")
            print(f"       RSP workflow disabled: modules failed to load.")
            print(f"{'='*70}\n")
            overall_workflow_success = False # Mark as failure if a chosen workflow can't even run
        elif not generated_rsp_inlists_paths:
            cli_logger.warning("RSP workflow is enabled in config, but no RSP inlist files were generated by MESA analysis. Skipping RSP runs.")
            print(f"\n{'='*70}")
            print(f"       MESA RSP workflow is enabled, but no inlist files were generated. Skipping.")
            print(f"{'='*70}\n")
        elif not mesa_star_dir:
            # This case should ideally be caught earlier by the sys.exit(1) if `mesa_star_dir` is missing
            # or auto-detection failed. But it's here as an additional safeguard.
            cli_logger.critical("RSP workflow is enabled, but 'mesa_star_dir' is not set. Cannot run RSP simulations. Skipping.")
            print(f"\n{'='*70}")
            print(f"       MESA RSP workflow skipped: 'mesa_star_dir' is missing.")
            print(f"{'='*70}\n")
            overall_workflow_success = False # Critical failure if the path is missing
        else:
            # All conditions met: RSP workflow is enabled, modules loaded, inlists generated, MESA path set.
            print(f"\n{'='*70}")
            print(f"       Starting MESA RSP Workflow...")
            print(f"{'='*70}\n")
            
            try:
                rsp_workflow_results = run_mesa_rsp_workflow(
                    inlist_paths=generated_rsp_inlists_paths,
                    config_data=config, # <--- EGYETLEN VÁLTOZTATÁS EZ A SOR!
                    max_workers=os.cpu_count() # Use all available CPU cores for parallelization
                )

                # Check the results of the RSP workflow
                if rsp_workflow_results['failed'] or rsp_workflow_results['timeout'] or rsp_workflow_results['error']:
                    cli_logger.error("MESA RSP workflow completed with some failures, timeouts, or errors. Check previous logs for details.")
                    overall_workflow_success = False # Mark overall workflow as unsuccessful
                    print(f"\n{'='*70}")
                    print(f"       MESA RSP Workflow Completed with Errors/Timeouts.")
                    print(f"{'='*70}\n")
                else:
                    cli_logger.info("MESA RSP workflow completed successfully for all runs.")
                    print(f"\n{'='*70}")
                    print(f"       MESA RSP Workflow Completed Successfully.")
                    print(f"{'='*70}\n")

            except Exception as e:
                cli_logger.critical(f"Critical unexpected error during MESA RSP workflow: {e}", exc_info=True)
                overall_workflow_success = False # Mark overall workflow as unsuccessful
                print(f"\n{'='*70}")
                print(f"       MESA RSP Workflow Encountered a Critical Python Error.")
                print(f"{'='*70}\n")
    else:
        # RSP workflow is completely disabled in configuration
        print(f"\n{'='*70}")
        print(f"       MESA RSP workflow is disabled in configuration.")
        print(f"{'='*70}\n")


    # --- Plotting Workflow ---
    # Check if 'generate_plots' exists and is True, or if any specific plotting flag is True.
    # If plotting_settings section itself is missing, assume no plotting.
    plotting_enabled = getattr(config, 'plotting_settings', None) and \
                               (config.plotting_settings.get('generate_plots', False) or \
                                config.plotting_settings.get('generate_heatmaps', False) or \
                                config.plotting_settings.get('generate_hr_diagrams', 'none') != 'none' or \
                                config.plotting_settings.get('generate_blue_loop_plots_with_bc', False))

    if plotting_enabled:
        print(f"\n{'='*70}")
        print(f"       Starting Plotting Workflow...")
        print(f"{'='*70}\n")
        try:
            # Handle heatmap generation if enabled
            if config.plotting_settings.get('generate_heatmaps', False):
                handle_heatmap_generation(
                    args=config,
                    summary_df_for_plotting=summary_df,
                    plots_sub_dir=plots_output_dir,
                    analysis_results_sub_dir=analysis_results_sub_dir,
                    input_dir=config.general_settings.input_dir
                )
            
            # Handle HR diagram generation if enabled
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
            print(f"       Plotting Workflow Completed Successfully.")
            print(f"{'='*70}\n")
        except Exception as e:
            cli_logger.error(f"Error during plotting workflow: {e}", exc_info=True)
            # Plotting errors do NOT mark the overall workflow as unsuccessful by default.
            # Change 'overall_workflow_success = False' here if you want plotting failures to cause a full workflow failure.
    else:
        print(f"\n{'='*70}")
        print(f"       Plotting workflow is disabled in configuration.")
        print(f"{'='*70}\n")


    # --- GYRE Workflow ---
    # Use .get() with a default to safely check if 'gyre_workflow' section exists and is enabled
    if config.gyre_workflow.get('run_gyre_workflow', False): 
        # Variable to store the return status of the GYRE workflow (0 for success, non-zero for failure)
        gyre_workflow_return_status = 0

        if _GYRE_MODULES_LOADED:
            print(f"\n{'='*70}")
            print(f"       Starting GYRE Workflow...")
            print(f"{'='*70}\n")
            
            # Check if filtered profiles are needed but not available for GYRE
            if config.gyre_workflow.get('run_mode', '').upper() == 'FILTERED_PROFILES' and \
               (not gyre_input_csv_path or not os.path.exists(gyre_input_csv_path)):
                cli_logger.warning(f"GYRE workflow enabled in 'FILTERED_PROFILES' mode, but the filtered profiles CSV ('{config.gyre_workflow.get('filtered_profiles_csv_name', 'N/A')}') "
                                   f"was not generated or not found at '{gyre_input_csv_path}'. "
                                   f"This is required for GYRE to run on filtered profiles. Skipping GYRE workflow.")
                print(f"\n{'='*70}")
                print(f"       GYRE Workflow Skipped: Required input CSV not found.")
                print(f"{'='*70}\n")
                gyre_workflow_return_status = 1 # Mark GYRE as failed if skipped due to missing input
            else:
                try:
                    # Capture the integer return value from run_gyre_workflow
                    gyre_workflow_return_status = run_gyre_workflow(
                        config_data=config, # Pass the entire config object
                        filtered_profiles_csv_path=gyre_input_csv_path, # This is still needed for FILTERED_PROFILES mode
                        debug_mode=config.general_settings.debug
                    )

                    # Based on the return status, print the appropriate message for the GYRE section
                    if gyre_workflow_return_status == 0:
                        print(f"\n{'='*70}")
                        print(f"       GYRE Workflow Completed Successfully.")
                        print(f"{'='*70}\n")
                    else:
                        # run_gyre_workflow already logs/prints specific GYRE run failures,
                        # so here we just confirm the overall GYRE workflow encountered an error.
                        print(f"\n{'='*70}") # Re-print separator for clarity after GYRE's own messages
                        print(f"       GYRE Workflow Encountered an Error.")
                        print(f"{'='*70}\n")
                        # overall_workflow_success will be set based on gyre_workflow_return_status below

                except Exception as e:
                    # This block catches unexpected Python exceptions *during* the call to run_gyre_workflow itself,
                    # not the GYRE executable's return code (which is handled inside run_gyre_workflow).
                    cli_logger.critical(f"Critical error during GYRE workflow: {e}", exc_info=True)
                    print(f"\n{'='*70}")
                    print(f"       GYRE Workflow Encountered a Critical Python Error.")
                    print(f"{'='*70}\n")
                    gyre_workflow_return_status = 1 # Ensure this also contributes to GYRE failure status
        else: # _GYRE_MODULES_LOADED is False (meaning GYRE imports failed at startup)
            print(f"\n{'='*70}")
            print(f"       GYRE workflow is enabled in configuration, but GYRE modules failed to load at startup. Skipping GYRE workflow.")
            print(f"{'='*70}\n")
            gyre_workflow_return_status = 1 # Mark GYRE workflow as failed if its modules couldn't load

        # After the GYRE section, update the overall workflow success flag
        if gyre_workflow_return_status != 0:
            overall_workflow_success = False
    else: # run_gyre_workflow is False in config
        print(f"\n{'='*70}")
        print(f"       GYRE workflow is disabled in configuration (run_gyre_workflow=False).")
        print(f"{'='*70}\n")

    # --- END OF RUN (using print for clear visibility) ---
    print(f"\n{'='*80}")
    if overall_workflow_success:
        print(f"║        {'mesalab Workflow Finished Successfully!':^72}        ║")
    else:
        print(f"║        {'mesalab Workflow Completed with Errors/Skipped Steps!':^72}        ║")
    print(f"{'='*80}\n")

    # Exit the script with a non-zero code if the overall workflow was not successful
    if not overall_workflow_success:
        sys.exit(1)

# --- Entry Point ---
if __name__ == '__main__':
    main()