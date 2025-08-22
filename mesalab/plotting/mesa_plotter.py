# mesalab/plotting/mesa_plotter.py

import os
import logging
import pandas as pd
import numpy as np

# Import the actual plot generation functions from their respective modules
from .heatmap_generator import generate_heatmaps_and_time_diff_csv
from ..bluelooptools.blue_loop_cmd_plotter import generate_blue_loop_plots_with_bc, load_and_group_data
from .all_hrd_plotter import generate_all_hr_diagrams # Import the HR diagram generator
# Removed analyze_mesa_grid_directory as it's not used within this file.
# from ..analyzis.grid_analyzer import analyze_mesa_grid_directory

def handle_heatmap_generation(args, summary_df_for_plotting, plots_sub_dir, analysis_results_sub_dir, input_dir):
    """
    Generate heatmaps showing how often models cross the instability strip.

    This function processes the YAML or CSV summary files from a previous analysis
    to create Z-M (metallicity-mass) heatmaps of instability strip crossing counts.
    It saves the figures in a subdirectory and labels them with the model grid name.

    Args:
        args (argparse.Namespace): Runtime arguments (e.g., plot options).
        summary_df_for_plotting (pd.DataFrame): Optional preloaded summary dataframe (can be None).
        plots_sub_dir (str): Output directory for saving heatmap images.
        analysis_results_sub_dir (str): Directory with analysis summary files (e.g., YAML/CSV).
        input_dir (str): Name of the input model grid (used in plot titles/filenames).

    Returns:
        None

    Example:
        >>> from mesalab.plotting import mesa_plotter
        >>> summary_dir = "results/summary"
        >>> plot_dir = "results/plots"
        >>> mesa_plotter.handle_heatmap_generation(args, None, plot_dir, summary_dir, input_dir="grid_z001")
        Heatmaps saved to results/plots showing crossing frequencies by Z and M.
    """
    if not args.plotting_settings.generate_heatmaps:
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
            blue_loop_output_type=args.blue_loop_analysis.blue_loop_output_type,
            analyze_blue_loop=args.blue_loop_analysis.analyze_blue_loop
        )
        logging.info("Heatmaps generated successfully.")
    except Exception as e:
        logging.error(f"Error generating heatmaps: {e}.")


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

    Example:
        >>> from mesalab.plotting import mesa_plotter
        >>> detail_dir = "processed/detail_data"
        >>> cmd_plot_dir = "results/plots/cmd"
        >>> mesa_plotter.handle_blue_loop_bc_plotting(args, None, cmd_plot_dir, detail_dir)
        Plots saved for each Z showing blue loop positions in Gaia CMD.
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
                                                (Updated from dict to list to match all_hrd_plotter.py)
        drop_zams (bool): Whether to skip points before the zero-age main sequence.

    Returns:
        None

    Example:
        >>> from mesalab.plotting import mesa_plotter
        >>> data = [df_z004_m1, df_z004_m2, df_z014_m1] # Example: list of DataFrames
        >>> output_dir = "output/plots"
        >>> mesa_plotter.handle_hr_diagram_generation(args, output_dir, data, drop_zams=True)
        HR diagrams saved to output/plots for each metallicity.
    """
    # Note: cli.py passes a boolean for drop_zams, so args.plotting_settings.generate_hr_diagrams
    # will be 'true', 'false', or 'drop_zams'. The check here needs to handle that.
    if not args.plotting_settings.generate_hr_diagrams or (isinstance(args.plotting_settings.generate_hr_diagrams, str) and args.plotting_settings.generate_hr_diagrams.lower() == 'none'):
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
            all_history_data_flat=full_history_data_for_plotting, # <--- THIS IS THE CORRECTED LINE!
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