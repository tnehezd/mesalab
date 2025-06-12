import os
import argparse # Needed for argparse.Namespace type hint and for creating gyre_args
import pandas as pd
import numpy as np
import re
from tqdm import tqdm # Not directly used in cli.py, but often part of shared dev environment
import yaml # Not directly used in cli.py, but part of parsing_options
import logging
import sys
from datetime import datetime # For logging file naming

# --- GLOBAL LOGGING CONFIGURATION ---
# This configuration is default and can be overridden based on args.debug.
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logging.getLogger('matplotlib').setLevel(logging.ERROR) # Suppress non-error messages from Matplotlib
# -----------------------------------------------------

# Import functions from specialized modules
from .config_parser import parsing_options
from .data_reader import extract_params_from_inlist, scan_mesa_runs, get_data_from_history_file
from .blue_loop_analyzer import analyze_blue_loop_and_instability
from .output_manager import create_output_directories, get_analysis_file_paths
from .mesa_analyzer import perform_mesa_analysis
from .plot_handlers import handle_heatmap_generation, handle_blue_loop_bc_plotting, handle_hr_diagram_generation

# Import specific functions from the GYRE input generator module
from .gyre_in_generator import find_nearest_profiles, generate_gyre_in_file, run_gyre_input_generation

def setup_logging_to_file(output_dir: str, debug_mode: bool = False):
    """
    Sets up logging to a file and the console.
    This overrides the global basicConfig settings.
    """
    log_file_path = os.path.join(output_dir, f"mesagrid_run_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log")
    
    # Remove existing handlers to prevent duplication
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

    # Parse all arguments from CLI and configuration file
    args = parsing_options()

    # Set up logging to the output directory
    # config_parser has already checked for output_dir existence, but we ensure the folder here
    os.makedirs(args.output_dir, exist_ok=True)
    setup_logging_to_file(args.output_dir, args.debug)


    logging.info(f"MESA Grid Analysis started. Input directory: {args.input_dir}, Output directory: {args.output_dir}")

    # Create output subdirectories
    # The create_output_directories function already handles these folders and returns their paths
    analysis_results_sub_dir, plots_sub_dir, blue_loop_plots_bc_sub_dir, detail_files_output_dir = \
        create_output_directories(args.output_dir, args.analyze_blue_loop, args.generate_plots, args.generate_blue_loop_plots_with_bc)

    # Get paths for analysis summary files
    summary_csv_path, cross_csv_path = get_analysis_file_paths(analysis_results_sub_dir)

    logging.debug(f"--- RE-ANALYSIS DEBUG ---")
    logging.debug(f"    force_reanalysis (from arguments): {args.force_reanalysis}")
    logging.debug(f"    summary_csv_path: {summary_csv_path}")
    logging.debug(f"    Does summary_csv_path exist? {os.path.exists(summary_csv_path)}")
    logging.debug(f"    cross_csv_path: {cross_csv_path}")
    logging.debug(f"    Does cross_csv_path exist? {os.path.exists(cross_csv_path)}")

    # Call the main analysis function, which returns three DataFrames
    summary_df_for_plotting, combined_detail_data_for_plotting, full_history_data_for_plotting = \
        perform_mesa_analysis(args, analysis_results_sub_dir, detail_files_output_dir)

    # Determine if analysis or data loading occurred.
    # If summary_df_for_plotting is empty, it means no runs were found or processed.
    analysis_performed_or_data_loaded = not summary_df_for_plotting.empty
    
    if not analysis_performed_or_data_loaded:
        logging.info("No analysis performed or data loaded. Exiting.")
        return # Exit if there's no data to work with

    # --- GYRE Input File Generation ---
    if args.generate_gyre_in:
        logging.info("Starting GYRE input file generation process.")
        
        # Create a new Namespace object to pass to run_gyre_input_generation.
        # This allows passing specific arguments, including the in-memory DataFrame.
        gyre_args = argparse.Namespace()
        
        # Populate GYRE-specific arguments from the main args object.
        # These values are already populated with defaults or values from CLI/config.
        gyre_args.gyre_mesa_base_dir = args.gyre_mesa_base_dir 
        gyre_args.gyre_output_dir = args.gyre_output_dir   
        gyre_args.gyre_model_name = args.gyre_model_name   
        gyre_args.gyre_blue_loop_csv = args.gyre_blue_loop_csv 
        
        # Conditional logic to pass the in-memory DataFrame if analysis was performed
        if args.analyze_blue_loop and analysis_performed_or_data_loaded:
            # Check for required columns and rename them if necessary for gyre_in_generator consistency.
            # mesa_analyzer returns 'first_model_number' and 'last_model_number'.
            # gyre_in_generator expects 'min_model_number' and 'max_model_number'.
            # Also pass the 'run_dir_path' column.
            required_cols_for_gyre_df = [
                'initial_mass', 'initial_Z', 'first_model_number', 'last_model_number', 'run_dir_path'
            ]
            if all(col in summary_df_for_plotting.columns for col in required_cols_for_gyre_df):
                
                df_to_pass_to_gyre = summary_df_for_plotting[required_cols_for_gyre_df].copy()
                df_to_pass_to_gyre.rename(columns={
                    'initial_mass': 'mass', 
                    'initial_Z': 'Z',       
                    'first_model_number': 'min_model_number',
                    'last_model_number': 'max_model_number'
                }, inplace=True)
                
                gyre_args.df_model_ranges = df_to_pass_to_gy # Pass the in-memory DataFrame
                logging.info("Passing in-memory analysis results to GYRE input generator.")
            else:
                logging.warning("In-memory summary DataFrame does not contain required columns (first_model_number, last_model_number, run_dir_path). GYRE generator will attempt to load from CSV.")
                gyre_args.df_model_ranges = None # Ensure it's None to trigger CSV loading in gyre_in_generator
        else:
            logging.info("Analysis was not performed or no data is available. GYRE input generator will attempt to load from CSV if --gyre-blue-loop-csv was provided.")
            gyre_args.df_model_ranges = None # Ensure it's None to trigger CSV loading in gyre_in_generator

        try:
            # Call the GYRE input generation function
            run_gyre_input_generation(gyre_args)
        except Exception as e:
            logging.error(f"Error during GYRE input file generation: {e}")
            logging.exception("GYRE input generation exception details:") # Log full traceback
            
    # --- Heatmap Generation ---
    # This part already works correctly in the previous version.
    # It assumes handle_heatmap_generation checks args.generate_heatmaps.
    handle_heatmap_generation(
        args,
        summary_df_for_plotting,
        plots_sub_dir,
        analysis_results_sub_dir,
        args.input_dir # Important to pass args.input_dir, not local input_dir if other modules need it
    )

    # --- HR Diagram Plotting Section (now uses full_history_data_for_plotting) ---
    if args.generate_hr_diagrams != 'none': # Check if HR diagrams should be generated
        logging.info("Attempting to generate HR diagrams.")
        
        # Determine boolean for dropping ZAMS based on argument
        should_drop_zams_for_hrd = (args.generate_hr_diagrams == 'drop_zams')
        logging.debug(f"    --> HR Diagram Generation: should_drop_zams_for_hrd set to: {should_drop_zams_for_hrd}")

        handle_hr_diagram_generation(
            args,
            plots_sub_dir, # This is the base 'plots' directory
            full_history_data_for_plotting, # Pass the full history data
            drop_zams=should_drop_zams_for_hrd # Pass the boolean flag
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