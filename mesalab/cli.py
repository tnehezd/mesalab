import os
import argparse
import pandas as pd
import numpy as np
import re
from tqdm import tqdm
import yaml
import logging
import sys
from datetime import datetime

# --- GLOBAL LOGGING CONFIGURATION ---
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logging.getLogger('matplotlib').setLevel(logging.ERROR)
# -----------------------------------------------------

# Import functions from specialized modules
from .io.config_parser import parsing_options
from .analyzis.data_reader import extract_params_from_inlist, scan_mesa_runs, get_data_from_history_file
from .bluelooptools.blue_loop_analyzer import analyze_blue_loop_and_instability
from .io.output_manager import create_output_directories, get_analysis_file_paths
from .analyzis.mesa_analyzer import perform_mesa_analysis
from .allplots.plot_handlers import handle_heatmap_generation, handle_blue_loop_bc_plotting, handle_hr_diagram_generation

# >>> NEW/MODIFIED IMPORT FOR GYRE_MODULES
from .gyretools.gyre_modules import run_single_gyre_model, run_gyre_workflow, 

def setup_logging_to_file(output_dir: str, debug_mode: bool = False):
    """
    Sets up logging to a file and the console.
    This overrides the global basicConfig settings.
    """
    log_file_path = os.path.join(output_dir, f"mesagrid_run_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log")
    
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)

    log_level = logging.DEBUG if debug_mode else logging.INFO

    logging.basicConfig(
        level=log_level,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(log_file_path),
            logging.StreamHandler(sys.stdout)
        ]
    )
    logging.info(f"Logging set up. Log file: {log_file_path}")
    if debug_mode:
        logging.debug("Debug mode enabled.")


def main():
    """ Main function: Coordinates the analysis and plotting workflow. """

    args = parsing_options()

    os.makedirs(args.output_dir, exist_ok=True)
    setup_logging_to_file(args.output_dir, args.debug)

    logging.info(f"MESA Grid Analysis started. Input directory: {args.input_dir}, Output directory: {args.output_dir}")

    analysis_results_sub_dir, plots_sub_dir, blue_loop_plots_bc_sub_dir, detail_files_output_dir = \
        create_output_directories(args.output_dir, args.analyze_blue_loop, args.generate_plots, args.generate_blue_loop_plots_with_bc)

    summary_csv_path, cross_csv_path = get_analysis_file_paths(analysis_results_sub_dir)

    logging.debug(f"--- RE-ANALYSIS DEBUG ---")
    logging.debug(f"    force_reanalysis (from arguments): {args.force_reanalysis}")
    logging.debug(f"    summary_csv_path: {summary_csv_path}")
    logging.debug(f"    Does summary_csv_path exist? {os.path.exists(summary_csv_path)}")
    logging.debug(f"    cross_csv_path: {cross_csv_path}")
    logging.debug(f"    Does cross_csv_path exist? {os.path.exists(cross_csv_path)}")

    summary_df_for_plotting, combined_detail_data_for_plotting, full_history_data_for_plotting = \
        perform_mesa_analysis(args, analysis_results_sub_dir, detail_files_output_dir)

    analysis_performed_or_data_loaded = not summary_df_for_plotting.empty
    
    if not analysis_performed_or_data_loaded:
        logging.info("No analysis performed or data loaded. Exiting.")
        return

    # --- GYRE Workflow Integration (Input Generation & Simulation) ---
    # This block now covers both generating the input CSV AND running GYRE,
    # all managed by gyre_modules.py's run_gyre_workflow function.
    if args.run_gyre_workflow: # This requires a new argparse flag, e.g., --run-gyre-workflow
        logging.info("Starting integrated GYRE workflow (input generation and simulation).")
        
        # Determine the path to gyre_config.in
        # It's crucial that gyre_config.in contains all necessary paths
        # for MESA profiles (mesa_dir) and GYRE executable (gyre_dir).
        # We'll pass this config file path to gyre_modules.run_gyre_workflow.
        
        # Get gyre_config_path from arguments, default to 'gyre_config.in'
        gyre_config_file_path = getattr(args, 'gyre_config_path', 'gyre_config.in') 
        
        # If the path is not absolute, try to find it relative to output_dir first, then current dir.
        if not os.path.isabs(gyre_config_file_path):
             proposed_path_in_output = os.path.join(args.output_dir, gyre_config_file_path)
             if os.path.exists(proposed_path_in_output):
                 gyre_config_file_path = proposed_path_in_output
             elif os.path.exists(gyre_config_file_path): # Check current directory
                 pass # Use the default or specified relative path
             else:
                 logging.error(f"GYRE configuration file '{gyre_config_file_path}' not found at '{proposed_path_in_output}' or current directory. Please provide a valid path.")
                 return # Or raise an exception, depending on desired strictness

        try:
            # Here, we call the run_gyre_workflow function from gyre_modules.
            # This function is now expected to handle:
            # 1. Reading gyre_config.in
            # 2. Identifying profiles (potentially using the logic moved from gyre_in_generator)
            # 3. Generating inlist files for each profile
            # 4. Executing GYRE for each profile
            gyre_modules.run_gyre_workflow(config_file=gyre_config_file_path)
        except Exception as e:
            logging.error(f"Error during integrated GYRE workflow: {e}")
            logging.exception("Integrated GYRE workflow exception details:")
            
    # --- Heatmap Generation ---
    handle_heatmap_generation(
        args,
        summary_df_for_plotting,
        plots_sub_dir,
        analysis_results_sub_dir,
        args.input_dir
    )

    # --- HR Diagram Plotting Section ---
    if args.generate_hr_diagrams != 'none':
        logging.info("Attempting to generate HR diagrams.")
        should_drop_zams_for_hrd = (args.generate_hr_diagrams == 'drop_zams')
        logging.debug(f"    --> HR Diagram Generation: should_drop_zams_for_hrd set to: {should_drop_zams_for_hrd}")

        handle_hr_diagram_generation(
            args,
            plots_sub_dir,
            full_history_data_for_plotting,
            drop_zams=should_drop_zams_for_hrd
        )
    else:
        logging.info("HR diagram generation skipped based on --generate-hr-diagrams setting.")

    # --- Blue Loop Specific Plots with BCs Plotting Section ---
    handle_blue_loop_bc_plotting(
        args,
        combined_detail_data_for_plotting,
        blue_loop_plots_bc_sub_dir,
        detail_files_output_dir
    )

    logging.info("MESA Grid Analysis and workflow completed.")

if __name__ == "__main__":
    main()