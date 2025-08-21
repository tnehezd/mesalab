import sys
import os
import logging
import pkg_resources

# Import config_parser module, which handles ALL argument parsing and config loading.
# It's imported first because the entire script's behavior depends on the config.
from mesalab.io import config_parser

# It's better to import the config_paths module here and use its functions directly.
from mesalab.io import config_paths

# --- Logging Setup (Initial - Manual Configuration) ---
# Get the root logger. All other loggers will inherit from it.
root_logger = logging.getLogger()

# Crucially, set the root logger to DEBUG immediately.
# This ensures that all loggers that inherit from root can emit DEBUG messages from the very beginning.
root_logger.setLevel(logging.DEBUG)

# Remove any existing handlers to prevent duplicate output (e.g., from implicit basicConfig calls).
for handler in root_logger.handlers[:]:
    if isinstance(handler, logging.StreamHandler) and handler.stream == sys.stdout:
        root_logger.removeHandler(handler)

# Add a StreamHandler to output to console.
console_handler = logging.StreamHandler(sys.stdout)
formatter = logging.Formatter('%(asctime)s - %(levelname)s: %(message)s')
console_handler.setFormatter(formatter)
root_logger.addHandler(console_handler)

# Suppress verbose warnings from external libraries like matplotlib and numba.
logging.getLogger('matplotlib').setLevel(logging.WARNING)
logging.getLogger('numba').setLevel(logging.WARNING)

# Logger for cli.py itself (distinct from root_logger for finer control).
cli_logger = logging.getLogger(__name__)
# Set cli_logger to DEBUG initially to capture early debug messages specific to cli.py.
cli_logger.setLevel(logging.WARNING)


class DummyModule:
    """A dummy class to provide dummy functions for modules that fail to load."""
    def __getattr__(self, name):
        def dummy_func(*args, **kwargs):
            cli_logger.warning(f"Function '{name}' from a missing/unavailable module was called. Skipping operation.")
            # For functions expected to return a status, return a failure code.
            return 1
        return dummy_func


# --- Main Application Logic ---
def main():
    """
    Main entry point for the MESA/GYRE/RSP analysis and plotting pipeline.
    This function orchestrates the entire workflow based on the configuration.
    """

    overall_workflow_success = True

    try:
        # Get mesalab package version for display
        mesalab_version = pkg_resources.get_distribution('mesalab').version
    except pkg_resources.DistributionNotFound:
        mesalab_version = "N/A (not installed as package)"

    # Print initial banner to console.
    print(f"\n{'='*80}")
    print(f"{'mesalab CLI - Starting Analysis Workflow':^80}")
    print(f"{'Version: ' + mesalab_version:^80}")
    print(f"{'='*80}\n")

    # The most critical step: Parse command line arguments and load configuration from YAML.
    # The entire program flow depends on this object.
    try:
        config = config_parser.parsing_options()
    except Exception as e:
        cli_logger.critical(f"FATAL: Failed to parse configuration at startup: {e}", exc_info=True)
        sys.exit(1)

    # --- Conditional Module Imports based on Configuration ---
    # We only import the heavier modules if they are actually needed.
    is_mesa_dependent_workflow_enabled = (
        config.gyre_workflow.get('run_gyre_workflow', False) or
        config.rsp_workflow.get('run_rsp_workflow', False) or
        config.plotting_settings.get('generate_blue_loop_plots_with_bc', False) or
        config.plotting_settings.get('generate_hr_diagrams', 'none') != 'none' or
        config.plotting_settings.get('generate_heatmaps', False)
    )

    # These flags will determine if we can call the respective modules later.
    _GYRE_MODULES_LOADED = False
    _RSP_MODULES_LOADED = False
    
    # We will import all dependent modules here if any of the conditions are met.
    if is_mesa_dependent_workflow_enabled:
        try:
            cli_logger.debug("Attempting to import core mesalab modules...\n\n")
            from mesalab.analyzis.mesa_analyzer import perform_mesa_analysis as mesa_analyzer
            from mesalab.plotting.mesa_plotter import handle_heatmap_generation
            from mesalab.plotting.mesa_plotter import handle_hr_diagram_generation
            from mesalab.plotting.mesa_plotter import handle_blue_loop_bc_plotting
            from mesalab.gyretools.gyre_modules import run_gyre_workflow
            from mesalab.rsptools.rsp_runner import run_mesa_rsp_workflow
            
            _GYRE_MODULES_LOADED = True
            _RSP_MODULES_LOADED = True
            cli_logger.debug("Successfully imported all core mesalab modules.")
        except ImportError as e:
            cli_logger.error(f"Failed to import core MESA/GYRE/RSP modules: {e}", exc_info=True)
            print(f"FATAL ERROR: Failed to import critical mesalab/GYRE modules. Please check installation and dependencies.", file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            cli_logger.critical(f"CRITICAL UNEXPECTED ERROR during initial module imports: {e}", exc_info=True)
            print(f"FATAL ERROR: An unexpected critical error occurred during initial module imports: {e}", file=sys.stderr)
            sys.exit(1)
    else:
        cli_logger.info("No MESA/GYRE/RSP workflows enabled. Skipping module imports.")
        # Define dummy functions to prevent NameError if they are called later.
        def mesa_analyzer(*args, **kwargs): cli_logger.warning("MESA analysis is disabled in config. Skipping.")
        def handle_heatmap_generation(*args, **kwargs): cli_logger.warning("Plotting is disabled in config. Skipping.")
        def handle_hr_diagram_generation(*args, **kwargs): cli_logger.warning("Plotting is disabled in config. Skipping.")
        def handle_blue_loop_bc_plotting(*args, **kwargs): cli_logger.warning("Plotting is disabled in config. Skipping.")
        def run_gyre_workflow(*args, **kwargs): cli_logger.warning("GYRE workflow is disabled in config. Skipping.")
        def run_mesa_rsp_workflow(*args, **kwargs): cli_logger.warning("RSP workflow is disabled in config. Skipping.")


    # Set up environment variables required for external executables (like GYRE)
    # This call is now correctly placed after config is loaded, but before any
    # MESA/GYRE specific operations are attempted.
    config_paths.set_environment_variables_for_executables(config)

    cli_logger.debug(f"Starting main application logic. Raw CLI arguments: {sys.argv[1:]}")
    
    # --- Final Logging Setup (based on parsed config) ---
    if not config.general_settings.get('debug', False):
        root_logger.setLevel(logging.WARNING)
        cli_logger.setLevel(logging.WARNING)
        cli_logger.info("Debug mode is OFF. General logging level is INFO.")
    else:
        cli_logger.debug("Debug logging confirmed and enabled by final configuration.")
        
    cli_logger.debug(f"Final resolved configuration being used by cli.py: {config}")

    # --- Output Directory Setup ---
    # Ensure all output directories are absolute paths for clarity and reliability
    output_base_dir = os.path.abspath(config.general_settings.output_dir)
    session_output_dir = output_base_dir
    analysis_results_sub_dir = os.path.join(session_output_dir, 'analysis_results')
    detail_files_output_dir = os.path.join(session_output_dir, 'detail_files')
    plots_output_dir = os.path.join(session_output_dir, 'plots')
    
    rsp_output_subdir = None
    mesa_star_dir = None

    rsp_output_subdir = None
    if config.rsp_workflow.get('run_rsp_workflow', False):
        # Retrieve the subdirectory name from the YAML config, or use a default if it's not specified.
        rsp_output_name = config.rsp_workflow.get('rsp_output_subdir', 'rsp_outputs')
        rsp_output_subdir = os.path.join(output_base_dir, rsp_output_name)
        os.makedirs(rsp_output_subdir, exist_ok=True)
        cli_logger.info(f"Dedicated RSP output directory created: '{rsp_output_subdir}'")
        
    # No more `sys.exit(1)` here for missing paths. That logic is now safely handled
    # within `config_paths.set_environment_variables_for_executables`.
    mesa_star_dir = config.general_settings.get('mesa_star_dir')
        
    # Create general output subdirectories
    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    os.makedirs(detail_files_output_dir, exist_ok=True)
    os.makedirs(plots_output_dir, exist_ok=True)
    cli_logger.info(f"All primary outputs for this session will be saved in: '{session_output_dir}'")

    # Initialize variables that will be populated by mesa_analyzer
    gyre_input_csv_path = None
    generated_rsp_inlists_paths = []
    generated_cross_csv_paths = []

    # --- MESA Analysis Workflow ---
    print(f"\n{'='*70}")
    print(f"        Starting MESA Analysis Workflow...")
    print(f"{'='*70}\n")
    try:
        gyre_cfg_exists = hasattr(config, 'gyre_workflow') and config.gyre_workflow is not None
        gyre_workflow_enabled_for_analysis_csv = False
        gyre_csv_name_to_pass = None

        if gyre_cfg_exists and config.gyre_workflow.get('run_gyre_workflow', False):
            if config.gyre_workflow.get('run_mode', '').upper() == 'FILTERED_PROFILES':
                gyre_csv_name_to_pass = config.gyre_workflow.get('filtered_profiles_csv_name', None)
                if gyre_csv_name_to_pass:
                    gyre_workflow_enabled_for_analysis_csv = True
                else:
                    cli_logger.warning("GYRE workflow is enabled in FILTERED_PROFILES mode, but 'filtered_profiles_csv_name' is missing in config. Will not generate GYRE input CSV from analysis.")

        summary_df, combined_detail_data, full_history_data, gyre_input_csv_path, generated_rsp_inlists_paths, generated_cross_csv_paths = \
            mesa_analyzer(
                args=config,
                analysis_results_sub_dir=analysis_results_sub_dir,
                detail_files_output_dir=detail_files_output_dir,
                gyre_input_csv_name=gyre_csv_name_to_pass if gyre_workflow_enabled_for_analysis_csv else None,
                rsp_output_subdir=rsp_output_subdir
            )

        if summary_df.empty:
            cli_logger.info("No summary data generated from MESA analysis. Skipping subsequent steps.")
            overall_workflow_success = False
            sys.exit(1)

        print(f"\n{'='*70}")
        print(f"        MESA Analysis Workflow Completed Successfully.")
        print(f"{'='*70}\n")
    except Exception as e:
        cli_logger.critical(f"Critical error during MESA Analysis workflow: {e}", exc_info=True)
        overall_workflow_success = False
        sys.exit(1)

    # --- MESA RSP Workflow ---
    if config.rsp_workflow.get('run_rsp_workflow', False):
        if not _RSP_MODULES_LOADED:
            cli_logger.warning("RSP workflow is enabled in config, but RSP modules failed to load at startup. Skipping RSP workflow.")
            overall_workflow_success = False
        elif not generated_rsp_inlists_paths:
            cli_logger.warning("RSP workflow is enabled in config, but no RSP inlist files were generated by MESA analysis. Skipping RSP runs.")
        elif not mesa_star_dir:
            cli_logger.critical("RSP workflow is enabled, but 'mesa_star_dir' is not set. Cannot run RSP simulations. Skipping.")
            overall_workflow_success = False
        else:
            print(f"\n{'='*70}")
            print(f"        Starting MESA RSP Workflow...")
            print(f"{'='*70}\n")
            try:
                rsp_workflow_results = run_mesa_rsp_workflow(
                    inlist_paths=generated_rsp_inlists_paths,
                    config_data=config,
                    rsp_output_subdir=rsp_output_subdir
                )
                if rsp_workflow_results['failed'] or rsp_workflow_results['timeout'] or rsp_workflow_results['error']:
                    cli_logger.error("MESA RSP workflow completed with some failures, timeouts, or errors. Check previous logs for details.")
                    overall_workflow_success = False
                else:
                    cli_logger.info("MESA RSP workflow completed successfully for all runs.")
            except Exception as e:
                cli_logger.critical(f"Critical unexpected error during MESA RSP workflow: {e}", exc_info=True)
                overall_workflow_success = False
            
            print(f"\n{'='*70}")
            print(f"        MESA RSP Workflow Completed.")
            print(f"{'='*70}\n")
    else:
        print(f"\n{'='*70}")
        print(f"        MESA RSP workflow is disabled in configuration.")
        print(f"{'='*70}\n")

    # --- Plotting Workflow ---
    plotting_enabled = getattr(config, 'plotting_settings', None) and \
        (config.plotting_settings.get('generate_plots', False) or
         config.plotting_settings.get('generate_heatmaps', False) or
         config.plotting_settings.get('generate_hr_diagrams', 'none') != 'none' or
         config.plotting_settings.get('generate_blue_loop_plots_with_bc', False))

    if plotting_enabled:
        print(f"\n{'='*70}")
        print(f"        Starting Plotting Workflow...")
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
            print(f"        Plotting Workflow Completed Successfully.")
            print(f"{'='*70}\n")
        except Exception as e:
            cli_logger.error(f"Error during plotting workflow: {e}", exc_info=True)
    else:
        print(f"\n{'='*70}")
        print(f"        Plotting workflow is disabled in configuration.")
        print(f"{'='*70}\n")

    # --- GYRE Workflow ---
    if config.gyre_workflow.get('run_gyre_workflow', False):
        gyre_workflow_return_status = 0
        if not _GYRE_MODULES_LOADED:
            cli_logger.warning("GYRE workflow is enabled in config, but GYRE modules failed to load at startup. Skipping GYRE workflow.")
            gyre_workflow_return_status = 1
        elif config.gyre_workflow.get('run_mode', '').upper() == 'FILTERED_PROFILES' and \
             (not gyre_input_csv_path or not os.path.exists(gyre_input_csv_path)):
            cli_logger.warning(f"GYRE workflow enabled in 'FILTERED_PROFILES' mode, but the required CSV file was not generated or not found at '{gyre_input_csv_path}'. Skipping GYRE workflow.")
            gyre_workflow_return_status = 1
        else:
            print(f"\n{'='*70}")
            print(f"        Starting GYRE Workflow...")
            print(f"{'='*70}\n")
            try:
                # Create a full path for the GYRE output subdirectory
                gyre_output_path = os.path.join(output_base_dir, config.gyre_workflow.get('gyre_output_subdir', 'gyre_outputs'))
                gyre_workflow_return_status = run_gyre_workflow(
                    config_data=config,
                    filtered_profiles_csv_path=gyre_input_csv_path,
                    debug_mode=config.general_settings.debug,
                    gyre_output_subdir=gyre_output_path
                )
            except Exception as e:
                cli_logger.critical(f"Critical error during GYRE workflow: {e}", exc_info=True)
                gyre_workflow_return_status = 1

        if gyre_workflow_return_status != 0:
            overall_workflow_success = False

        print(f"\n{'='*70}")
        if gyre_workflow_return_status == 0:
            print(f"        GYRE Workflow Completed Successfully.")
        else:
            print(f"        GYRE Workflow Encountered an Error.")
        print(f"{'='*70}\n")
    else:
        print(f"\n{'='*70}")
        print(f"        GYRE workflow is disabled in configuration.")
        print(f"{'='*70}\n")

    # --- END OF RUN ---
    print(f"\n{'='*80}")
    if overall_workflow_success:
        print(f"║        {'mesalab Workflow Finished Successfully!':^62}        ║")
    else:
        print(f"║        {'mesalab Workflow Completed with Errors/Skipped Steps!':^62}        ║")
    print(f"{'='*80}\n")

    if not overall_workflow_success:
        sys.exit(1)

# --- Entry Point ---
if __name__ == '__main__':
    main()