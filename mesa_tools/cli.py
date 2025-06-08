import os
import argparse
import pandas as pd
import numpy as np
import re
from tqdm import tqdm
import yaml
import logging
import sys

# --- GLOBAL LOGGING CONFIGURATION ---
logging.basicConfig(level=logging.DEBUG, format='%(levelname)s: %(message)s')
logging.getLogger('matplotlib').setLevel(logging.ERROR) # Suppress Matplotlib's non-error messages
# -----------------------------------------------------

# Import functions from your specialized modules
from .config_parser import parsing_options
from .data_reader import extract_params_from_inlist, scan_mesa_runs, get_data_from_history_file
from .blue_loop_analyzer import analyze_blue_loop_and_instability
from .output_manager import create_output_directories, get_analysis_file_paths
from .mesa_analyzer import perform_mesa_analysis

# Import the new plot_handlers module and its functions
from .plot_handlers import handle_heatmap_generation, handle_blue_loop_bc_plotting, handle_hr_diagram_generation

def main():
    """ Main function: Orchestrates the analysis and plotting workflow. """

    args = parsing_options()

    input_dir = args.input_dir
    output_dir = args.output_dir
    analyze_blue_loop = args.analyze_blue_loop
    # inlist_name = args.inlist_name # This variable is not used in main
    # should_generate_heatmaps = args.generate_heatmaps # Handled inside handle_heatmap_generation
    force_reanalysis = args.force_reanalysis
    # blue_loop_output_type = args.blue_loop_output_type # Handled inside handle_heatmap_generation
    should_generate_plots = args.generate_plots # This flag is still used for general plots, though HR is separate now
    # should_generate_blue_loop_plots_with_bc = args.generate_blue_loop_plots_with_bc # Handled inside handle_blue_loop_bc_plotting

    analysis_results_sub_dir, plots_sub_dir, blue_loop_plots_bc_sub_dir, detail_files_output_dir = \
        create_output_directories(output_dir, analyze_blue_loop, should_generate_plots, args.generate_blue_loop_plots_with_bc)

    # Call the function from output_manager to get file paths
    summary_csv_path, cross_csv_path = get_analysis_file_paths(analysis_results_sub_dir)

    # --- CRITICAL DEBUGGING FOR REANALYSIS LOGIC ---
    logging.debug(f"--- REANALYSIS DEBUG ---")
    logging.debug(f"    force_reanalysis (from args): {force_reanalysis}")
    logging.debug(f"    summary_csv_path: {summary_csv_path}")
    logging.debug(f"    Does summary_csv_path exist? {os.path.exists(summary_csv_path)}")
    logging.debug(f"    cross_csv_path: {cross_csv_path}")
    logging.debug(f"    Does cross_csv_path exist? {os.path.exists(cross_csv_path)}")

    # --- Calling the analysis function which now returns three values ---
    # MODIFIED: Expecting three return values
    summary_df_for_plotting, combined_detail_data_for_plotting, full_history_data_for_plotting = \
        perform_mesa_analysis(args, analysis_results_sub_dir, detail_files_output_dir)

    # Determine if reanalysis was actually performed based on the returned summary_df
    # If summary_df_for_plotting is empty, it means no runs were found or processed.
    analysis_performed_or_data_loaded = not summary_df_for_plotting.empty
    
    if not analysis_performed_or_data_loaded:
        logging.info("No analysis performed or data loaded. Exiting.")
        return # Exit if no data to work with


    # --- Heatmap generation ---
    handle_heatmap_generation(
        args,
        summary_df_for_plotting,
        plots_sub_dir,
        analysis_results_sub_dir,
        input_dir
    )

    # --- Plotting section for HR diagrams (now uses full_history_data_for_plotting) ---
    if args.generate_hr_diagrams:
        logging.info("Attempting to generate HR diagrams.")
        handle_hr_diagram_generation(
            args,
            plots_sub_dir, # This is the base 'plots' directory
            full_history_data_for_plotting # NEW: Pass the full history data
        )

    # --- Plotting section for blue loop specific plots with BCs ---
    handle_blue_loop_bc_plotting(
        args,
        combined_detail_data_for_plotting,
        blue_loop_plots_bc_sub_dir,
        detail_files_output_dir
    )


if __name__ == "__main__":
    main()
