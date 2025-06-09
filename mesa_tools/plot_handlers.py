import os
import logging
import pandas as pd
import numpy as np

# Import the actual plot generation functions from their respective modules
from .heatmap_generator import generate_heatmaps_and_time_diff_csv
from .blue_loop_cmd_plotter import generate_blue_loop_plots_with_bc, load_and_group_data
from .all_hrd_plotter import generate_all_hr_diagrams # Import the HR diagram generator
from .grid_analyzer import analyze_mesa_grid_directory 

def handle_heatmap_generation(args, summary_df_for_plotting, plots_sub_dir, analysis_results_sub_dir, input_dir):
    """
    Handles the generation of heatmaps based on data from MESA runs.
    Now directly reads from 'crossing_count_grid.csv' to ensure 0-crossing data is included.

    Args:
        args (argparse.Namespace): The command-line arguments object, containing flags like
                                   `generate_heatmaps`, `blue_loop_output_type`, `analyze_blue_loop`.
        summary_df_for_plotting (pd.DataFrame): The summary DataFrame of analyzed MESA runs.
                                                 (This is still passed but no longer used for heatmap data source).
        plots_sub_dir (str): The directory where plot images will be saved.
        analysis_results_sub_dir (str): The directory where analysis results (e.g., CSVs) will be saved.
        input_dir (str): The input directory, used to determine the model name for plot titles/filenames.
    """
    if not args.generate_heatmaps:
        logging.debug("Heatmap generation not requested. Skipping.")
        return

    logging.info("Attempting to generate heatmaps...")
    try:
        # --- MODIFICATION START ---
        # Load the cross_data_matrix directly from the saved CSV.
        # This CSV is generated from summary_df_raw (which includes 0 crossings).
        cross_csv_path = os.path.join(analysis_results_sub_dir, "crossing_count_grid.csv")
        
        if not os.path.exists(cross_csv_path):
            logging.warning(f"'{cross_csv_path}' not found. Cannot generate heatmaps. Please ensure analysis has been run successfully.")
            return

        cross_data_matrix_loaded = pd.read_csv(cross_csv_path, index_col=0) # Read the CSV
        
        # Ensure index and columns are numeric strings for reindexing if they are not already
        cross_data_matrix_loaded.columns = pd.to_numeric(cross_data_matrix_loaded.columns, errors='coerce')
        cross_data_matrix_loaded.index = pd.to_numeric(cross_data_matrix_loaded.index, errors='coerce')

        # Drop any NaN columns/indices that might have resulted from conversion
        cross_data_matrix_loaded.dropna(axis=0, how='all', inplace=True)
        cross_data_matrix_loaded.dropna(axis=1, how='all', inplace=True)

        # Ensure the columns and index are sorted for consistent plotting
        unique_zs_for_heatmap = sorted([z for z in cross_data_matrix_loaded.index.unique() if not pd.isna(z)])
        unique_masses_for_heatmap = sorted([m for m in cross_data_matrix_loaded.columns.unique() if not pd.isna(m)])

        # Reindex to ensure consistent order, and fill potential missing grid points with NaN
        cross_data_df_final = cross_data_matrix_loaded.reindex(index=unique_zs_for_heatmap, columns=unique_masses_for_heatmap)
        # --- MODIFICATION END ---

        if cross_data_df_final.empty:
            logging.warning("Cross data DataFrame is empty after loading from CSV. Cannot generate heatmaps.")
            return

        generate_heatmaps_and_time_diff_csv(
            cross_data_df=cross_data_df_final, # Pass the directly loaded and processed DataFrame
            summary_csv_path=os.path.join(analysis_results_sub_dir, "summary_results.csv"),
            unique_zs=unique_zs_for_heatmap,
            unique_masses=unique_masses_for_heatmap,
            plots_output_dir=plots_sub_dir,
            analysis_results_output_dir=analysis_results_sub_dir,
            model_name=os.path.basename(input_dir),
            blue_loop_output_type=args.blue_loop_output_type,
            analyze_blue_loop=args.analyze_blue_loop
        )
        logging.info("Heatmaps generated successfully.")
    except Exception as e:
        logging.error(f"Error generating heatmaps: {e}.")


def handle_blue_loop_bc_plotting(args, combined_detail_data_for_plotting, blue_loop_plots_bc_sub_dir, detail_files_output_dir):
    """
    Handles the generation of blue loop specific plots with bolometric corrections (BCs).

    Args:
        args (argparse.Namespace): The command-line arguments object, containing flags like
                                   `generate_blue_loop_plots_with_bc` and `force_reanalysis`.
        combined_detail_data_for_plotting (pd.DataFrame): Detailed MESA run data, potentially combined from multiple runs.
                                                           This might be empty initially if not reanalyzed.
        blue_loop_plots_bc_sub_dir (str): The directory where blue loop BC plots will be saved.
        detail_files_output_dir (str): The directory where individual detail data files are stored,
                                       used if data needs to be loaded from disk.
    """
    if not args.generate_blue_loop_plots_with_bc:
        logging.debug("Blue loop specific plots with BCs not requested. Skipping.")
        return

    logging.info("Attempting to generate blue loop specific plots with BCs...")
    try:
        if combined_detail_data_for_plotting.empty and not args.force_reanalysis:
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
def handle_hr_diagram_generation(args, plots_sub_dir, full_history_data_for_plotting):
    """
    Handles the generation of Hertzsprung-Russell (HR) diagrams using pre-loaded MESA run data.

    Args:
        args (argparse.Namespace): The command-line arguments object, containing
                                   `generate_hr_diagrams` and potentially
                                   `mesa_output_subdir`, `inlist_filename`, `inlist_alternatives`.
        plots_sub_dir (str): The directory where HR diagram images will be saved.
        full_history_data_for_plotting (dict): A dictionary where keys are metallicities (Z)
                                               and values are lists of full, untrimmed
                                               history DataFrames for each MESA run.
    """
    if not args.generate_hr_diagrams:
        logging.debug("HR diagram generation not requested. Skipping.")
        return

    logging.info("Attempting to generate HR diagrams using pre-loaded full history data...")
    try:
        if not full_history_data_for_plotting:
            logging.warning("No full history data available for HR diagram generation. Skipping.")
            return

        model_name = os.path.basename(args.input_dir) 

        logT_blue_edge = [3.76, 3.83]
        logL_blue_edge = [4.5, 2.4]
        logT_red_edge = [3.65, 3.77]
        logL_red_edge = [4.5, 2.4]

        generate_all_hr_diagrams(
            data_by_metallicity=full_history_data_for_plotting,
            model_name=model_name,
            output_dir=plots_sub_dir,
            logT_blue_edge=logT_blue_edge,
            logL_blue_edge=logL_blue_edge,
            logT_red_edge=logT_red_edge,
            logL_red_edge=logL_red_edge
        )
        logging.info("HR diagrams generated successfully.")
    except Exception as e:
        logging.error(f"Error generating HR diagrams: {e}")

