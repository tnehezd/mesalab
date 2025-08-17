# mesalab/plotting/mesa_plotter.py

import os
import logging
import pandas as pd
import numpy as np
import re

# Import the actual plot generation functions from their respective modules
from .heatmap_generator import generate_heatmaps_and_time_diff_csv
from ..bluelooptools.blue_loop_cmd_plotter import generate_blue_loop_plots_with_bc, load_and_group_data
from .all_hrd_plotter import generate_all_hr_diagrams

def handle_heatmap_generation(args, summary_df_for_plotting, plots_sub_dir, analysis_results_sub_dir, input_dir):
    """
    Generate heatmaps showing how often models cross the instability strip.

    This function processes the YAML or CSV summary files from a previous analysis
    to create Z-M (metallicity-mass) heatmaps of instability strip crossing counts.
    It now handles cases where the grid is split by Y value, generating a separate
    heatmap for each Y value found. It saves the figures in a subdirectory and labels
    them with the model grid name and the initial Y value.

    Args:
        args (argparse.Namespace): Runtime arguments (e.g., plot options).
        summary_df_for_plotting (pd.DataFrame): Optional preloaded summary dataframe (can be None).
        plots_sub_dir (str): Output directory for saving heatmap images.
        analysis_results_sub_dir (str): Directory with analysis summary files (e.g., YAML/CSV).
        input_dir (str): Name of the input model grid (used in plot titles/filenames).

    Returns:
        None
    """
    if not args.plotting_settings.generate_heatmaps:
        logging.debug("Heatmap generation not requested. Skipping.")
        return

    logging.info("Attempting to generate heatmaps...")
    
    # Use a regex pattern to find all cross-grid CSV files, including those split by Y.
    file_pattern = re.compile(r'crossing_count_grid_Y(\d+\.\d+)\.csv')
    
    found_files = [f for f in os.listdir(analysis_results_sub_dir) if file_pattern.match(f)]

    if not found_files:
        logging.warning(
            f"No cross-grid CSV files found in '{analysis_results_sub_dir}' "
            "matching the pattern 'crossing_count_grid_Y*.csv'. "
            "Cannot generate heatmaps. Please ensure analysis has been run successfully."
        )
        return

    for cross_csv_filename in found_files:
        try:
            # Extract the Y value from the filename
            y_val = file_pattern.match(cross_csv_filename).group(1)
            
            cross_csv_path = os.path.join(analysis_results_sub_dir, cross_csv_filename)
            logging.info(f"Generating heatmap for Y = {y_val} from file '{cross_csv_filename}'...")

            cross_data_matrix_loaded = pd.read_csv(cross_csv_path, index_col=0)

            # Ensure index and columns are numeric for sorting
            cross_data_matrix_loaded.columns = pd.to_numeric(cross_data_matrix_loaded.columns, errors='coerce')
            cross_data_matrix_loaded.index = pd.to_numeric(cross_data_matrix_loaded.index, errors='coerce')

            # Drop any NaN columns/indices that might have resulted from conversion
            cross_data_matrix_loaded.dropna(axis=0, how='all', inplace=True)
            cross_data_matrix_loaded.dropna(axis=1, how='all', inplace=True)

            if cross_data_matrix_loaded.empty:
                logging.warning(f"Cross data DataFrame is empty for Y={y_val}. Skipping heatmap generation.")
                continue

            # Ensure the columns and index are sorted for consistent plotting
            unique_zs_for_heatmap = sorted([z for z in cross_data_matrix_loaded.index.unique() if not pd.isna(z)])
            unique_masses_for_heatmap = sorted([m for m in cross_data_matrix_loaded.columns.unique() if not pd.isna(m)])

            # Reindex to ensure consistent order, and fill potential missing grid points with NaN
            cross_data_df_final = cross_data_matrix_loaded.reindex(index=unique_zs_for_heatmap, columns=unique_masses_for_heatmap)

            generate_heatmaps_and_time_diff_csv(
                cross_data_df=cross_data_df_final,
                summary_csv_path=os.path.join(analysis_results_sub_dir, "summary_results.csv"),
                unique_zs=unique_zs_for_heatmap,
                unique_masses=unique_masses_for_heatmap,
                plots_output_dir=plots_sub_dir,
                analysis_results_output_dir=analysis_results_sub_dir,
                model_name=f"{os.path.basename(input_dir)}_Y{y_val}",  # Add Y value to model name for filename
                blue_loop_output_type=args.blue_loop_analysis.blue_loop_output_type,
                analyze_blue_loop=args.blue_loop_analysis.analyze_blue_loop
            )
            logging.info(f"Successfully generated heatmap for Y={y_val}.")

        except Exception as e:
            logging.error(f"Error generating heatmap for file '{cross_csv_filename}': {e}.")

def handle_blue_loop_bc_plotting(args, combined_detail_data_for_plotting, blue_loop_plots_bc_sub_dir, detail_files_output_dir):
    """
    Plot blue loop tracks in the color–magnitude diagram with bolometric corrections.

    Reads preprocessed detail CSV files and bolometric correction data (e.g., for Gaia bands),
    then plots the evolutionary tracks during the blue loop phase on CMDs.
    Used for visualizing where and how models populate the instability strip.

    Args:
        args (argparse.Namespace): Runtime arguments (e.g., filters, flags).
        combined_detail_data_for_plotting (pd.DataFrame): Optional preloaded detail data.
        blue_loop_plots_bc_sub_dir (str): Directory to save output CMD plots.
        detail_files_output_dir (str): Directory where the input detail CSVs are located.

    Returns:
        None
    """
    if not args.plotting_settings.generate_blue_loop_plots_with_bc:
        logging.debug("Blue loop specific plots with BCs not requested. Skipping.")
        return

    logging.info("Attempting to generate blue loop specific plots with BCs...")
    try:
        if combined_detail_data_for_plotting.empty and not args.general_settings.force_reanalysis:
            logging.info(f"Detail data not in memory; attempting to load from {detail_files_output_dir} for plotting...")
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

# --- HR diagram generation handler ---
def handle_hr_diagram_generation(args, plots_sub_dir, full_history_data_for_plotting, drop_zams):
    """
    Generate Hertzsprung–Russell diagrams from full stellar evolutionary tracks.

    This function loops through available history data by metallicity, and plots
    the full evolutionary path (log Teff vs log L). Optionally filters out
    pre-ZAMS data points for clarity.

    Args:
        args (argparse.Namespace): Command-line arguments including force overwrite, limits.
        plots_sub_dir (str): Output directory for saving HR diagrams.
        full_history_data_for_plotting (list): List of DataFrames, each representing
                                            a full history for a single MESA run.
        drop_zams (bool): Whether to skip points before the zero-age main sequence.

    Returns:
        None
    """
    if not args.plotting_settings.generate_hr_diagrams or args.plotting_settings.generate_hr_diagrams.lower() == 'none':
        logging.debug("HR diagram generation not requested. Skipping.")
        return

    logging.info("Attempting to generate HR diagrams using pre-loaded full history data...")
    try:
        if not full_history_data_for_plotting:
            logging.warning("No full history data available for HR diagram generation. Skipping.")
            return

        model_name = os.path.basename(args.general_settings.input_dir)

        logT_blue_edge = [3.76, 3.83]
        logL_blue_edge = [4.5, 2.4]
        logT_red_edge = [3.65, 3.77]
        logL_red_edge = [4.5, 2.4]
        
        logging.info(f"Preparing to generate HR diagrams with drop_zams={drop_zams}.")

        generate_all_hr_diagrams(
            all_history_data_flat=full_history_data_for_plotting,
            model_name=model_name,
            output_dir=plots_sub_dir,
            logT_blue_edge=logT_blue_edge,
            logL_blue_edge=logL_blue_edge,
            logT_red_edge=logT_red_edge,
            logL_red_edge=logL_red_edge,
            drop_zams=drop_zams
        )
        logging.info("HR diagrams generated successfully.")
    except Exception as e:
        logging.error(f"Error generating HR diagrams: {e}")