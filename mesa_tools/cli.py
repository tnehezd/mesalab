import os
import argparse
import pandas as pd
import numpy as np
import re
from tqdm import tqdm
import yaml
import logging
import sys

# --- GLOBAL LOGGING CONFIGURATION (KEEP THIS HERE) ---
logging.basicConfig(level=logging.DEBUG, format='%(levelname)s: %(message)s')
logging.getLogger('matplotlib').setLevel(logging.ERROR) # Suppress Matplotlib's non-error messages
# -----------------------------------------------------

# Import functions from your specialized modules
from .config_parser import parsing_options
from .data_reader import extract_params_from_inlist, scan_mesa_runs, get_data_from_history_file
from .blue_loop_analyzer import analyze_blue_loop_and_instability
from .heatmap_generator import generate_heatmaps_and_time_diff_csv
from .blue_loop_cmd_plotter import generate_blue_loop_plots_with_bc, load_and_group_data
from .output_manager import create_output_directories, get_analysis_file_paths
from .mesa_analyzer import perform_mesa_analysis


def main():
    """ Main function ... Orchestrates the analysis and plotting workflow. """

    args = parsing_options()

    input_dir = args.input_dir
    output_dir = args.output_dir
    analyze_blue_loop = args.analyze_blue_loop
    inlist_name = args.inlist_name
    should_generate_heatmaps = args.generate_heatmaps
    force_reanalysis = args.force_reanalysis
    blue_loop_output_type = args.blue_loop_output_type
    should_generate_plots = args.generate_plots
    should_generate_blue_loop_plots_with_bc = args.generate_blue_loop_plots_with_bc

    analysis_results_sub_dir, plots_sub_dir, blue_loop_plots_bc_sub_dir, detail_files_output_dir = \
        create_output_directories(output_dir, analyze_blue_loop, should_generate_plots, should_generate_blue_loop_plots_with_bc)

    # Call the function from output_manager to get file paths
    summary_csv_path, cross_csv_path = get_analysis_file_paths(analysis_results_sub_dir)

    # --- CRITICAL DEBUGGING FOR REANALYSIS LOGIC ---
    logging.debug(f"--- REANALYSIS DEBUG ---")
    logging.debug(f"   force_reanalysis (from args): {force_reanalysis}")
    logging.debug(f"   summary_csv_path: {summary_csv_path}")
    logging.debug(f"   Does summary_csv_path exist? {os.path.exists(summary_csv_path)}")
    logging.debug(f"   cross_csv_path: {cross_csv_path}")
    logging.debug(f"   Does cross_csv_path exist? {os.path.exists(cross_csv_path)}")

    # The reanalysis_needed flag now primarily informs perform_mesa_analysis
    # We still need to know if data was *actually* reanalyzed or loaded, for downstream plotting.
    # A simple way to check this is to see if the returned summary_df is empty.
    
    # --- Calling the new analysis function ---
    summary_df_for_plotting, combined_detail_data_for_plotting = \
        perform_mesa_analysis(args, analysis_results_sub_dir, detail_files_output_dir)

    # Determine if reanalysis was actually performed based on the returned summary_df
    # If summary_df_for_plotting is empty, it means no runs were found or processed.
    analysis_performed_or_data_loaded = not summary_df_for_plotting.empty
    
    if not analysis_performed_or_data_loaded:
        logging.info("No analysis performed or data loaded. Exiting.")
        return # Exit if no data to work with


    # --- Heatmap generation (uses the returned summary_df) ---
    if should_generate_heatmaps:
        try:
            if 'blue_loop_crossing_count' in summary_df_for_plotting.columns:
                cross_data_matrix_loaded = summary_df_for_plotting['blue_loop_crossing_count'].unstack(level='initial_mass')
                
                unique_zs_for_heatmap = sorted(list(set(summary_df_for_plotting.index.get_level_values('initial_Z'))))
                unique_masses_for_heatmap = sorted(list(set(summary_df_for_plotting.index.get_level_values('initial_mass'))))

                generate_heatmaps_and_time_diff_csv(
                    cross_data_df=cross_data_matrix_loaded,
                    summary_csv_path=summary_csv_path, # Path to the potentially existing file
                    unique_zs=unique_zs_for_heatmap,
                    unique_masses=unique_masses_for_heatmap,
                    plots_output_dir=plots_sub_dir,
                    analysis_results_output_dir=analysis_results_sub_dir,
                    model_name=os.path.basename(input_dir),
                    blue_loop_output_type=blue_loop_output_type,
                    analyze_blue_loop=analyze_blue_loop
                )
                logging.info("Heatmaps generated successfully.")
            else:
                logging.warning("Summary CSV lacks 'blue_loop_crossing_count'; skipping heatmap generation.")
        except Exception as e:
            logging.error(f"Error generating heatmaps: {e}.")

    # Plotting section for general HRD plots (needs 'generate_general_hrd_plots' to be uncommented/imported if it exists)
    if should_generate_plots:
        logging.warning("Attempted to generate general HRD plots, but 'generate_general_hrd_plots' is not currently imported or defined.")
        logging.warning("Please ensure 'generate_general_hrd_plots' is available if you wish to use the '--generate-plots' flag.")

    # --- Plotting section for blue loop specific plots with BCs ---
    if should_generate_blue_loop_plots_with_bc:
        try:
            # If combined_detail_data_for_plotting is empty here, it means
            # either analyze_blue_loop was False during analysis, or no detail data
            # was produced/loaded by perform_mesa_analysis.
            # In this case, try to load it now if it wasn't reanalyzed.
            if combined_detail_data_for_plotting.empty and not force_reanalysis:
                 logging.info(f"Attempting to load detail files directly from {detail_files_output_dir} for plotting...")
                 # load_and_group_data function will now combine detail files from disk
                 combined_detail_data_for_plotting = load_and_group_data(detail_files_output_dir)

            if not combined_detail_data_for_plotting.empty:
                generate_blue_loop_plots_with_bc(
                    combined_df_all_data=combined_detail_data_for_plotting,
                    output_dir=blue_loop_plots_bc_sub_dir,
                    output_type_label="all_blue_loop_data"
                )
                logging.info("Blue loop specific plots with BCs generated successfully.")
            else:
                logging.warning("No blue loop detail data available for BC plots. Cannot generate plots.")
        except Exception as e:
            logging.error(f"Error generating blue loop specific plots with BCs: {e}")
            logging.error("Please ensure your 'blue_loop_cmd_plotter.py' is updated to expect 'combined_df_all_data' as its first parameter.")


if __name__ == "__main__":
    main()