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

# Import specific functions from the GYRE input generator module
from .gyretools.gyre_in_generator import find_nearest_profiles, generate_gyre_in_file, run_gyre_input_generation

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

    # --- GYRE Input File Generation ---
    if args.generate_gyre_in:
        logging.info("Starting GYRE input file generation process.")
        
        gyre_args = argparse.Namespace()
        gyre_args.gyre_mesa_base_dir = args.gyre_mesa_base_dir 
        gyre_args.gyre_output_dir = args.gyre_output_dir   
        gyre_args.gyre_model_name = args.gyre_model_name   

        # Determine which source to use for model ranges for GYRE
        df_for_gyre_input = None
        blue_loop_csv_for_gyre = None

        # Always try to use in-memory data first IF analysis was performed AND it has required columns
        if args.analyze_blue_loop and analysis_performed_or_data_loaded:
            required_cols_for_gyre_df = [
                'initial_mass', 'initial_Z', 'first_model_number', 'last_model_number', 'run_dir_path'
            ]
            if all(col in summary_df_for_plotting.columns for col in required_cols_for_gyre_df):
                df_for_gyre_input = summary_df_for_plotting[required_cols_for_gyre_df].copy()
                df_for_gyre_input.rename(columns={
                    'initial_mass': 'mass', 
                    'initial_Z': 'Z',       
                    'first_model_number': 'min_model_number',
                    'last_model_number': 'max_model_number'
                }, inplace=True)
                logging.info("Passing in-memory analysis results to GYRE input generator.")
            else:
                logging.warning("In-memory summary DataFrame lacks required columns for GYRE (first_model_number, last_model_number, run_dir_path). Will attempt to load from saved CSV.")
                # Fallback to CSV path if in-memory data is incomplete
                blue_loop_csv_for_gyre = os.path.join(analysis_results_sub_dir, "sorted_mass_Z_min_max.csv")
                logging.info(f"Falling back to GYRE data from CSV: {blue_loop_csv_for_gyre}")
        else:
            # If blue loop analysis was not enabled (args.analyze_blue_loop is False),
            # or no data was loaded for some reason, we must rely on the CSV.
            # First, check if the user explicitly provided a --gyre-blue-loop-csv
            if args.gyre_blue_loop_csv:
                blue_loop_csv_for_gyre = args.gyre_blue_loop_csv
                logging.info(f"Blue loop analysis not active. Using user-provided --gyre-blue-loop-csv: {blue_loop_csv_for_gyre}")
            else: # If user didn't provide, try the one saved by mesa_analyzer in the output directory
                blue_loop_csv_for_gyre = os.path.join(analysis_results_sub_dir, "sorted_mass_Z_min_max.csv")
                logging.info(f"Blue loop analysis not active and no --gyre-blue-loop-csv provided. Attempting to load GYRE data from default CSV: {blue_loop_csv_for_gyre}")


        gyre_args.df_model_ranges = df_for_gyre_input
        gyre_args.gyre_blue_loop_csv = blue_loop_csv_for_gyre

        try:
            run_gyre_input_generation(gyre_args)
        except Exception as e:
            logging.error(f"Error during GYRE input file generation: {e}")
            logging.exception("GYRE input generation exception details:")
            
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
