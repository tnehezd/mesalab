# mesalab/plotting/heatmap_generator.py

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
import math
from matplotlib.colors import ListedColormap, BoundaryNorm
import logging # Import the logging module
import sys

# --- Logging Setup for this module ---
# This ensures that if the module is run directly, it has a basic logging setup.
# When run via cli.py, the root logger configured in cli.py will take precedence.
logging.basicConfig(
    level=logging.INFO, # Default for this module if run standalone; cli.py will override
    format='%(asctime)s - %(levelname)s - %(name)s: %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout)
    ]
)
logger = logging.getLogger(__name__) # Logger for this specific module

def generate_heatmaps_and_time_diff_csv(cross_data_df, summary_csv_path, unique_zs, unique_masses,
                                         plots_output_dir, analysis_results_output_dir,
                                         model_name="MESA Grid Analysis", # Kept for potential use in plot titles
                                         blue_loop_output_type='all', analyze_blue_loop=False):
    """
    Generates a heatmap showing the number of instability strip (IS) crossings 
    as a function of initial stellar mass and metallicity, and optionally writes 
    a CSV with blue loop and instability phase durations.

    This function is typically used after the blue loop analysis step in the pipeline.
    The heatmap is a visual representation of the `cross_data_df` grid, with different
    colors representing the number of IS crossings for each unique Mass-Metallicity pair.
    Missing data points (NaNs) are explicitly handled and colored light grey.

    Args:
        cross_data_df (pd.DataFrame): Grid-like DataFrame with metallicities as index and masses as columns, 
                                      each value representing the number of IS crossings (0–5).
        summary_csv_path (str): Path to the summary CSV file containing time information.
        unique_zs (list): List of sorted unique metallicities (Z values).
        unique_masses (list): List of sorted unique initial masses.
        plots_output_dir (str): Directory to save the generated heatmap image.
        analysis_results_output_dir (str): Directory to save the output time difference CSV.
        model_name (str): Optional name of the model grid (used in plot title).
        blue_loop_output_type (str): Either 'all' or 'summary'; controls detail level of summary input.
        analyze_blue_loop (bool): If True, the function generates a CSV with phase durations.
    
    Returns:
        None

    Example:
        >>> import pandas as pd
        >>> import numpy as np
        >>> import os
        >>> from mesalab.plotting import heatmap_generator
        >>> 
        >>> plots_dir = 'output/plots'
        >>> results_dir = 'output/analysis_results'
        >>> os.makedirs(plots_dir, exist_ok=True)
        >>> os.makedirs(results_dir, exist_ok=True)
        >>>
        >>> # Create a dummy `cross_data_df`
        >>> cross_df = pd.DataFrame({
        ...     0.8: [0, 1, 2],
        ...     0.9: [1, np.nan, 3],
        ...     1.0: [2, 3, 5]
        ... }, index=[0.005, 0.008, 0.012])
        >>>
        >>> # Create a dummy summary CSV file with time data
        >>> summary_df = pd.DataFrame({
        ...     'initial_mass': [0.8, 0.8, 0.8, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0],
        ...     'initial_Z': [0.005, 0.008, 0.012, 0.005, 0.008, 0.012, 0.005, 0.008, 0.012],
        ...     'blue_loop_crossing_count': [0, 1, 2, 1, 0, 3, 2, 3, 5],
        ...     'blue_loop_start_age': [np.nan, 10.0, 15.0, 12.0, np.nan, 18.0, 20.0, 25.0, 30.0],
        ...     'blue_loop_end_age': [np.nan, 12.0, 17.0, 15.0, np.nan, 21.0, 22.0, 28.0, 33.0],
        ...     'instability_start_age': [np.nan, 10.5, 15.5, 12.5, np.nan, 18.5, 20.5, 25.5, 30.5],
        ...     'instability_end_age': [np.nan, 11.5, 16.5, 13.5, np.nan, 19.5, 21.5, 26.5, 31.5],
        ...     'calculated_blue_loop_duration': [np.nan, 2.0, 2.0, 3.0, np.nan, 3.0, 2.0, 3.0, 3.0],
        ...     'calculated_instability_duration': [np.nan, 1.0, 1.0, 1.0, np.nan, 1.0, 1.0, 1.0, 1.0]
        ... })
        >>> summary_csv_path = os.path.join(results_dir, 'summary.csv')
        >>> summary_df.to_csv(summary_csv_path, index=False)
        >>>
        >>> # Call the function with the dummy data and paths
        >>> print("Running example...")
        >>> generate_heatmaps_and_time_diff_csv(
        ...     cross_df,
        ...     summary_csv_path,
        ...     unique_zs=[0.005, 0.008, 0.012],
        ...     unique_masses=[0.8, 0.9, 1.0],
        ...     plots_output_dir=plots_dir,
        ...     analysis_results_output_dir=results_dir,
        ...     analyze_blue_loop=True
        ... )

    """

    if cross_data_df.empty:
        logger.warning("cross_data_df is empty. Cannot generate heatmaps.")
        return

    # Ensure index and columns are float for proper numerical operations and plotting
    cross_data_df.columns = pd.to_numeric(cross_data_df.columns, errors='coerce')
    cross_data_df.index = pd.to_numeric(cross_data_df.index, errors='coerce')

    # Drop any NaN columns/indices that might have resulted from conversion
    cross_data_df.dropna(axis=0, how='all', inplace=True)
    cross_data_df.dropna(axis=1, how='all', inplace=True)

    # Recalculate unique_zs_sorted and unique_masses_sorted from the potentially cleaned DataFrame
    unique_zs_sorted = sorted([z for z in cross_data_df.index.unique() if not pd.isna(z)])
    unique_masses_sorted = sorted([m for m in cross_data_df.columns.unique() if not pd.isna(m)])

    # Reindex the DataFrame to ensure it uses the sorted unique_zs and unique_masses
    cross_data_df_reindexed = cross_data_df.reindex(index=unique_zs_sorted, columns=unique_masses_sorted).astype(float)

    # Explicitly convert any remaining non-numeric/empty string values to NaN
    # This ensures that cmap.set_bad() correctly identifies and colors missing data.
    for col in cross_data_df_reindexed.columns:
        cross_data_df_reindexed[col] = pd.to_numeric(cross_data_df_reindexed[col], errors='coerce')

    logger.debug(f"cross_data_df_reindexed shape: {cross_data_df_reindexed.shape}")
    logger.debug(f"cross_data_df_reindexed has NaN values: {cross_data_df_reindexed.isnull().any().any()}")
    print(f"\n{'='*70}\n  Full Instability Strip Crossings Matrix (for Heatmap):\n{'='*70}\n"
        f"{cross_data_df_reindexed.to_string()}\n"
        f"{'='*70}\n")
                                             
    # --- Heatmap generation ---
    # Convert DataFrame to numpy array for imshow
    data_for_heatmap = cross_data_df_reindexed.to_numpy()

    # Define custom colors for the heatmap as requested:
    # 0 values are the darkest blue of viridis, NaN values are lightgrey, 1-5 use viridis.
    color_skipped = "lightgrey" # For NaN values (analysis skipped/error)

    # Get viridis colors for the actual crossing counts (0 to 5)
    # We get 6 distinct colors from the viridis colormap,
    # where the first color (index 0) will be the darkest blue for 0 crossings.
    viridis_full_range = plt.cm.viridis(np.linspace(0, 1, 6)) # 6 colors from dark to light viridis

    # Create the custom colormap using the viridis colors
    cmap = ListedColormap(viridis_full_range)

    # Set the color for bad (NaN) values to lightgrey
    cmap.set_bad(color=color_skipped) 

    # Set the color scale bounds for the custom colormap
    # -0.5 to 0.5 for 0, 0.5 to 1.5 for 1, etc.
    # This ensures that 0 maps to the first color, 1 to the second, etc.
    bounds = [-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5]
    norm = BoundaryNorm(bounds, cmap.N)

    # Create the heatmap
    plt.figure(figsize=(20, 12))
    plt.imshow(data_for_heatmap, aspect='auto', origin='lower', cmap=cmap, norm=norm)

    # Colorbar settings - showing ticks for 0, 1, 2, 3, 4, 5
    cbar = plt.colorbar(ticks=[0, 1, 2, 3, 4, 5])
    cbar.set_label("Number of IS Crossings", fontsize=14)
    cbar.ax.set_yticklabels(["0", "1", "2", "3", "4", "5"])
    
    # Axis settings
    plt.xticks(np.arange(len(unique_masses_sorted)), [f'{m:.1f}' for m in unique_masses_sorted], rotation=90, fontsize=12)
    metallicity_tick_indices = np.arange(0, len(unique_zs_sorted), 5)
    plt.yticks(metallicity_tick_indices, [f'{unique_zs_sorted[i]:.4f}' for i in metallicity_tick_indices], fontsize=12)
    plt.xlabel("Mass [M$_\odot$]", fontsize=14)
    plt.ylabel("Metallicity (Z)", fontsize=14)
    plt.title(f"Heatmap: Mass vs. Metallicity ({model_name})", fontsize=16)

    # Use a generic filename for the heatmap now
    heatmap_filename = "mesa_grid_blue_loop_heatmap.png"
    plt.tight_layout()
    plt.savefig(os.path.join(plots_output_dir, heatmap_filename), dpi=300)
    plt.close()
    logger.info(f"Generated heatmap: {heatmap_filename}")

    # Time differences logic (if analyze_blue_loop is True)
    if os.path.exists(summary_csv_path) and analyze_blue_loop:
        try:
            summary_df = pd.read_csv(summary_csv_path)

            # Ensure columns are numeric for calculation
            summary_df['blue_loop_start_age'] = pd.to_numeric(summary_df['blue_loop_start_age'], errors='coerce')
            summary_df['blue_loop_end_age'] = pd.to_numeric(summary_df['blue_loop_end_age'], errors='coerce')
            summary_df['instability_start_age'] = pd.to_numeric(summary_df['instability_start_age'], errors='coerce')
            summary_df['instability_end_age'] = pd.to_numeric(summary_df['instability_end_age'], errors='coerce')
            # Ensure crossing count is also numeric
            summary_df['blue_loop_crossing_count'] = pd.to_numeric(summary_df['blue_loop_crossing_count'], errors='coerce')

            # Round durations
            summary_df['calculated_blue_loop_duration'] = summary_df['calculated_blue_loop_duration'].apply(lambda x: round(x, 4) if pd.notna(x) else np.nan)
            summary_df['calculated_instability_duration'] = summary_df['calculated_instability_duration'].apply(lambda x: round(x, 4) if pd.notna(x) else np.nan)

            # --- Filtering Logic for Time Differences CSV ---
            # Only include rows where a valid blue loop was detected (crossing count > 0 and no NaNs in key ages)
            initial_rows = len(summary_df)
            filtered_df = summary_df[
                (summary_df['blue_loop_crossing_count'].notna()) &
                (summary_df['blue_loop_crossing_count'] > 0) &
                (summary_df['blue_loop_start_age'].notna()) &
                (summary_df['blue_loop_end_age'].notna())
            ].copy() # Use .copy() to avoid SettingWithCopyWarning

            if initial_rows > 0 and len(filtered_df) < initial_rows:
                logger.info(f"Filtered out {initial_rows - len(filtered_df)} rows from time_differences CSV where no valid blue loop was detected.")
            # --- END Filtering Logic ---

            # Renamed output file for consistency
            time_diff_csv_path = os.path.join(analysis_results_output_dir, "mesa_grid_time_differences.csv")

            output_cols = [
                'initial_mass', 'initial_Z',
                'blue_loop_start_age', 'blue_loop_end_age', 'calculated_blue_loop_duration',
                'instability_start_age', 'instability_end_age', 'calculated_instability_duration'
            ]

            # Filter for columns that actually exist in the DataFrame before selecting
            # Use the filtered_df here!
            output_cols_existing = [col for col in output_cols if col in filtered_df.columns]

            if not output_cols_existing:
                logger.warning(f"No relevant time difference columns found in filtered data. Skipping generation of time differences CSV.")
            else:
                # Save the filtered DataFrame
                filtered_df[output_cols_existing].to_csv(time_diff_csv_path, index=False)
                logger.info(f"Time differences CSV generated: {time_diff_csv_path}")

        except Exception as e:
            logger.error(f"Error generating time differences CSV: {e}", exc_info=True) # Added exc_info=True for traceback
    else:
        logger.info("Summary CSV not found or blue loop analysis not enabled. Skipping time differences CSV generation.")
