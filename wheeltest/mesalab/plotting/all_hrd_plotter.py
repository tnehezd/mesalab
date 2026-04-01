# mesalab/plotting/all_hrd_plotter.py

import numpy as np
import matplotlib.pyplot as plt
import math
import time
from matplotlib import colors
import os
import logging
import pandas as pd # Ensure pandas is imported as it's used for DataFrames

# Configure logging for better feedback during execution
logging.basicConfig(level=logging.WARNING, format='%(levelname)s: %(message:s)')

def generate_all_hr_diagrams(all_history_data_flat: list, model_name: str, output_dir: str,
                             logT_blue_edge: list, logL_blue_edge: list,
                             logT_red_edge: list, logL_red_edge: list,
                             drop_zams: bool = False):
    """
    Generates Hertzsprung-Russell (HR) diagrams for pre-loaded MESA run data,
    grouping plots by metallicity and saving each metallicity's plots
    as a single image.

    The pre-main sequence (pre-MS) phase can be excluded from the plots
    if 'drop_zams' is True.

    Plots are sorted by initial mass within each metallicity group; the subplot layout is
    fixed to 4 columns.

    If data is insufficient after trimming (and 'drop_zams' is True), the specific subplot
    for that run is skipped, and a warning is logged.

    Args:
        all_history_data_flat (list): A flat list of full, untrimmed history DataFrames
                                      for all MESA runs. Each DataFrame is expected
                                      to have 'initial_Z' and 'initial_mass' columns.
                                      (This is the 'flattened_full_history_data_for_plotting'
                                      from mesa_analyzer).
        model_name (str): The name of the MESA model, used for constructing
                          file paths and plot titles (e.g., 'nad_convos').
        output_dir (str): The directory where the generated HR diagram images
                          will be saved.
        logT_blue_edge (list): Logarithm of effective temperatures for the
                               blue edge of the instability strip.
        logL_blue_edge (list): Logarithm of luminosities for the
                               blue edge of the instability strip.
        logT_red_edge (list): Logarithm of effective temperatures for the
                               red edge of the instability strip.
        logL_red_edge (list): Logarithm of luminosities for the
                               red edge of the instability strip.
        drop_zams (bool, optional): If True, the pre-main sequence (pre-MS) phase
                                    is trimmed from the beginning of the track using
                                    the 'center_h1' drop criterion (or 'log_L' minimum as fallback).
                                    Defaults to False (i.e., full track is plotted).

    Returns:
        None

    Example:
        >>> import pandas as pd
        >>> import os
        >>> from mesalab.plotting import all_hrd_plotter
        >>> import numpy as np
        >>> # Define the output directory
        >>> output_dir = 'output/plots'
        >>> os.makedirs(output_dir, exist_ok=True)
        >>>
        >>> # Define dummy data for two different runs (M=1.0, Z=0.012 and M=1.5, Z=0.012)
        >>> df1 = pd.DataFrame({
        ...     'initial_mass': [1.0] * 20,
        ...     'initial_Z': [0.012] * 20,
        ...     'log_Teff': np.linspace(3.7, 3.8, 20),
        ...     'log_L': np.linspace(1.0, 1.5, 20),
        ...     'model_number': np.arange(20),
        ...     'center_h1': np.linspace(0.7, 0.6, 20)
        ... })
        >>> df2 = pd.DataFrame({
        ...     'initial_mass': [1.5] * 20,
        ...     'initial_Z': [0.012] * 20,
        ...     'log_Teff': np.linspace(3.8, 3.9, 20),
        ...     'log_L': np.linspace(1.5, 2.0, 20),
        ...     'model_number': np.arange(20),
        ...     'center_h1': np.linspace(0.7, 0.6, 20)
        ... })
        >>> # The function expects a flat list of DataFrames
        >>> all_data = [df1, df2]
        >>>
        >>> # Create dummy instability strip data
        >>> logT_blue = [3.8, 3.75, 3.7]
        >>> logL_blue = [1.5, 1.0, 0.5]
        >>> logT_red = [3.7, 3.65, 3.6]
        >>> logL_red = [1.5, 1.0, 0.5]
        >>>
        >>> # Call the function
        >>> all_hrd_plotter.generate_all_hr_diagrams(
        ...     all_history_data_flat=all_data,
        ...     model_name='dummy_model',
        ...     output_dir=output_dir,
        ...     logT_blue_edge=logT_blue,
        ...     logL_blue_edge=logL_blue,
        ...     logT_red_edge=logT_red,
        ...     logL_red_edge=logL_red,
        ...     drop_zams=True
        ... )

    """

    logging.info(f"Starting HR diagram generation for model '{model_name}'.")

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        logging.info(f"Created output directory: {output_dir}")

    # Group the flat list of DataFrames by metallicity
    # This recreates the dict structure that the rest of the function expects
    data_by_metallicity = {}
    for df in all_history_data_flat:
        if 'initial_Z' in df.columns and not df.empty:
            z_value = df['initial_Z'].iloc[0]
            if z_value not in data_by_metallicity:
                data_by_metallicity[z_value] = []
            data_by_metallicity[z_value].append(df)
        else:
            logging.warning("Skipping a DataFrame in HRD plotting due to missing 'initial_Z' or being empty.")

    # The data is now grouped by metallicity, so we just sort the keys
    sorted_metallicities = sorted(data_by_metallicity.keys())

    for z_value in sorted_metallicities:
        # current_z_dfs is a list of DataFrames for the current Z value
        current_z_dfs = data_by_metallicity[z_value]

        # Sort the DataFrames by initial_mass for consistent plotting order
        # Assuming 'initial_mass' column is present and valid for sorting
        try:
            current_z_dfs.sort(key=lambda df: df['initial_mass'].iloc[0])
            logging.info(f"Sorted {len(current_z_dfs)} runs by mass for Z={z_value:.4f}.")
        except KeyError:
            logging.warning(f" 'initial_mass' column not found for Z={z_value:.4f}. Skipping mass sort.")
        except IndexError:
            logging.warning(f"DataFrame for Z={z_value:.4f} is empty or 'initial_mass' column has no data. Skipping mass sort.")

        logging.info(f'➡ Processing HR diagrams for Z={z_value:.4f} with {len(current_z_dfs)} masses...')

        num_plots = len(current_z_dfs)
        
        # --- Fixed Column Layout: Always 4 Columns ---
        if num_plots == 0:
            logging.warning(f"No runs found for Z={z_value:.4f}. Skipping HR diagram generation for this metallicity.")
            continue
        
        cols = 4 # Fixed to 4 columns as requested
        rows = math.ceil(num_plots / cols)

        # --- Figure Size with 1:2 (Height:Width) Aspect Ratio ---
        # Define base dimensions for a single subplot to achieve 1:2 ratio
        base_subplot_height = 5  # Height in inches for each subplot
        base_subplot_width = base_subplot_height * 2 # Ensures 1:2 ratio, so 10 inches

        fig_width = cols * base_subplot_width
        fig_height = rows * base_subplot_height
        
        fig, axes = plt.subplots(rows, cols, figsize=(fig_width, fig_height), constrained_layout=True)
        
        # Flatten axes array for easy iteration, even if it's a single subplot (rows=1, cols=1)
        if num_plots == 1:
            axes = np.array([axes]) # Ensure it's an array for consistent indexing
        else:
            axes = axes.flatten()


        sc = None  # Default scatter plot reference for colorbar

        # Iterate directly over the DataFrames in current_z_dfs
        for i, df_full_history in enumerate(current_z_dfs):
            ax = axes[i] # Get the current subplot axis

            # Extract mass and Z from the DataFrame itself
            # Assuming 'initial_mass' and 'initial_Z' columns are present (added in mesa_analyzer)
            mass = df_full_history['initial_mass'].iloc[0] # Mass for title

            # --- Pre-MS Phase Trimming Logic (ZAMS detection) ---
            # Now conditional based on 'drop_zams' parameter
            df_post_prems = None # Initialize to None

            # Check for missing required columns or empty DataFrame
            if 'log_L' not in df_full_history.columns or df_full_history.empty:
                logging.warning(f"Missing 'log_L' or empty DataFrame for M={mass:.1f} (Z={z_value:.4f}). Skipping plot.")
                ax.set_visible(False)
                continue # Skip to the next run_info
            
            if drop_zams: # Csak akkor fut le a ZAMS levágás, ha drop_zams True
                if 'center_h1' in df_full_history.columns:
                    try:
                        initial_h1_val = df_full_history['center_h1'].iloc[0]
                        H1_DROP_THRESHOLD = 1e-4 # Adjustable threshold for H1 drop
                        zams_candidates = df_full_history[df_full_history['center_h1'] < (initial_h1_val - H1_DROP_THRESHOLD)]

                        if not zams_candidates.empty:
                            zams_idx = zams_candidates.index[0]
                            if zams_idx < len(df_full_history) - 1:
                                df_post_prems = df_full_history.iloc[zams_idx:].copy()
                                logging.info(f"Trimmed pre-MS using 'center_h1' drop criterion (threshold={H1_DROP_THRESHOLD}) for M={mass:.1f} (Z={z_value:.4f}).")
                            else:
                                logging.warning(f" 'center_h1' drop index is too close to end of data for M={mass:.1f} (Z={z_value:.4f}). Falling back to log_L minimum trimming.")
                                min_log_L_idx = df_full_history['log_L'].idxmin()
                                df_post_prems = df_full_history.iloc[min_log_L_idx:].copy()
                        else:
                            logging.warning(f" 'center_h1' did not drop below threshold (>{H1_DROP_THRESHOLD}) for M={mass:.1f} (Z={z_value:.4f}). Falling back to log_L minimum trimming.")
                            min_log_L_idx = df_full_history['log_L'].idxmin()
                            df_post_prems = df_full_history.iloc[min_log_L_idx:].copy()

                    except Exception as e:
                        logging.warning(f"Error during 'center_h1' trimming for M={mass:.1f} (Z={z_value:.4f}): {e}. Falling back to log_L minimum trimming.")
                        min_log_L_idx = df_full_history['log_L'].idxmin()
                        df_post_prems = df_full_history.iloc[min_log_L_idx:].copy()
                else:
                    logging.warning(f" 'center_h1' not found for M={mass:.1f} (Z={z_value:.4f}). Falling back to log_L minimum trimming.")
                    min_log_L_idx = df_full_history['log_L'].idxmin()
                    df_post_prems = df_full_history.iloc[min_log_L_idx:].copy()
            else:
                # If drop_zams is False, use the full history data
                df_post_prems = df_full_history.copy()
                logging.info(f"Pre-MS trimming skipped for M={mass:.1f} (Z={z_value:.4f}) as 'drop_zams' is False.")

            # --- Common plotting logic after trimming (or not trimming) ---
            log_Teff = df_post_prems['log_Teff'].values
            log_L = df_post_prems['log_L'].values
            model_number = np.array(df_post_prems['model_number'], dtype=float)

            if len(log_Teff) < 2: # Check for insufficient data even AFTER (potential) trimming
                logging.warning(f"Not enough data points after (potential) trimming for M={mass:.1f} (Z={z_value:.4f}) to plot HR diagram. Skipping plot.")
                ax.set_visible(False)
                continue # Skip to the next run_info

            # If we reach here, data is sufficient for plotting
            ax.set_title(f'{mass:.1f} M$_\odot$', fontsize=15)
            
            norm = colors.Normalize(vmin=np.min(model_number), vmax=np.max(model_number))
            cmap = plt.cm.viridis

            # Plot the evolutionary track
            sc = ax.scatter(log_Teff, log_L, c=model_number, cmap=cmap,
                            label=f'{mass:.1f} M$_\odot$', s=10, norm=norm, zorder=2)

            # Plot instability strip edges
            ax.plot(logT_blue_edge, logL_blue_edge, color='blue', linestyle='dashed', linewidth=1.5, zorder=1, label='Blue Edge')
            ax.plot(logT_red_edge, logL_red_edge, color='red', linestyle='dashed', linewidth=1.5, zorder=1, label='Red Edge')
            
            ax.invert_xaxis() # Standard HR diagram convention
            
        # Hide any unused subplots (those beyond the last processed index 'i')
        if num_plots > 0:
            start_idx_to_hide = i + 1
        else:
            start_idx_to_hide = 0
            
        for j in range(start_idx_to_hide, len(axes)):
            fig.delaxes(axes[j])

        # Set common Y-axis label only for the leftmost column
        for idx, ax_item in enumerate(axes):
            if ax_item.get_visible():
                if idx % cols == 0:
                    ax_item.set_ylabel(r'$\log L/L_\odot$', fontsize=15)
                else:
                    ax_item.set_yticklabels([])

        # Set common X-axis label only for the bottom row
        for idx, ax_item in enumerate(axes):
            if ax_item.get_visible():
                if idx >= (rows - 1) * cols:
                    ax_item.set_xlabel(r'$\log T_{\rm eff}$', fontsize=15)
                else:
                    ax_item.set_xticklabels([])

        fig.suptitle(f'Hertzsprung-Russell Diagram (Z = {z_value:.4f})', fontsize=20, y=1.02)

        if sc is not None:
            cbar = fig.colorbar(sc, ax=axes.ravel().tolist(), orientation='vertical', fraction=0.02, pad=0.02, label='Model Number (Evolutionary Stage)', aspect=50)
            cbar.set_label('Model Number (Evolutionary Stage)', fontsize=12)


        filename = os.path.join(output_dir, f'HR_diagram_{model_name}_z{z_value:.4f}.png')
        plt.savefig(filename, bbox_inches='tight', dpi=100)
        plt.close(fig)
        logging.info(f"✔ Saved HR diagram: {filename}")

    logging.info("✔  All HR diagram generation complete.")