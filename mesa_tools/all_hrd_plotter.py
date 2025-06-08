import numpy as np
import matplotlib.pyplot as plt
import pygyre as pg # Still imported, but no longer directly used for reading in generate_all_hr_diagrams
import math
import time
from matplotlib import colors
import os
import logging

# MODIFIED: Changed parameter from 'found_runs' to 'data_by_metallicity'
def generate_all_hr_diagrams(data_by_metallicity, model_name, output_dir,
                             logT_blue_edge, logL_blue_edge,
                             logT_red_edge, logL_red_edge):
    """
    Generates Hertzsprung-Russell (HR) diagrams for pre-loaded MESA run data,
    grouping plots by metallicity and saving each metallicity's plots
    as a single image.

    Args:
        data_by_metallicity (dict): A dictionary where keys are metallicities (Z)
                                    and values are lists of full, untrimmed
                                    history DataFrames for each MESA run.
                                    (This is the 'full_history_data_for_plotting' from mesa_analyzer).
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
    """
    logging.info(f"Starting HR diagram generation for model '{model_name}'.")

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        logging.info(f"Created output directory: {output_dir}")

    sorted_metallicities = sorted(data_by_metallicity.keys())

    for z_value in sorted_metallicities:
        current_z_dfs = data_by_metallicity[z_value] 
        logging.info(f'➡ Processing HR diagrams for Z={z_value:.4f} with {len(current_z_dfs)} masses...')

        num_plots = len(current_z_dfs)
        cols = 3
        rows = math.ceil(num_plots / cols)

        fig, axes = plt.subplots(rows, cols, figsize=(12, 14), constrained_layout=True)
        axes = axes.flatten()

        sc = None  # Default scatter plot reference for colorbar

        for i, df_full_history in enumerate(current_z_dfs):
            ax = axes[i] # Get the current subplot axis

            # NEW: Check if the DataFrame is empty before trying to access its elements
            if df_full_history.empty:
                logging.warning(f"No data in DataFrame for this run (Z={z_value:.4f}, plot index {i}). Skipping plot.")
                # Set title to indicate missing data
                ax.set_title(f'No Data (Z={z_value:.4f})', color='darkred', fontsize=9)
                ax.set_xticks([]) # Hide ticks
                ax.set_yticks([])
                continue # Skip to the next iteration of the loop

            # Extract mass from the DataFrame itself
            # Assuming 'initial_mass' column is present and has at least one row
            mass = df_full_history['initial_mass'].iloc[0] 

            log_Teff = df_full_history['log_Teff'].values
            log_L = df_full_history['log_L'].values
            model_number = np.array(df_full_history['model_number'], dtype=float)

            # REMOVED: The trimming logic based on np.argmax(log_Teff)
            # This ensures the full evolutionary track is plotted.
            # If specific pre-MS trimming is needed, a more robust method (e.g.,
            # using 'star_is_on_main_sequence' column) should be implemented here.

            if len(log_Teff) < 2: # Check for insufficient data even after no trimming
                logging.warning(f"Not enough data points for M={mass:.1f} (Z={z_value:.4f}) to plot HR diagram. Skipping.")
                ax.set_title(f'{mass:.1f} M$_\odot$\n(No Data)\n(Z={z_value:.4f})', color='darkred', fontsize=9)
                ax.set_xticks([])
                ax.set_yticks([])
                continue # Skip to the next run_info if data is insufficient

            norm = colors.Normalize(vmin=np.min(model_number), vmax=np.max(model_number))
            cmap = plt.cm.viridis

            # Plot the evolutionary track
            sc = ax.scatter(log_Teff, log_L, c=model_number, cmap=cmap,
                             label=f'{mass:.1f} M$_\odot$', s=10, norm=norm, zorder=2)

            # Plot instability strip edges
            ax.plot(logT_blue_edge, logL_blue_edge, color='blue', linestyle='dashed', linewidth=1.5, zorder=1, label='Blue Edge')
            ax.plot(logT_red_edge, logL_red_edge, color='red', linestyle='dashed', linewidth=1.5, zorder=1, label='Red Edge')

            ax.set_title(f'{mass:.1f} M$_\odot$', fontsize=10)
            ax.invert_xaxis() # Standard HR diagram convention
            
        # Hide any unused subplots
        for j in range(i + 1, len(axes)): # Use 'i + 1' to start from the next empty axis
            fig.delaxes(axes[j])

        # Set common Y-axis label only for the leftmost column
        for i, ax in enumerate(axes):
            if i % cols == 0:
                ax.set_ylabel(r'$\log L/L_\odot$', fontsize=12)
            else:
                ax.set_yticklabels([]) # Hide Y-axis labels for other columns

        # Set common X-axis label only for the bottom row
        for i, ax in enumerate(axes):
            if i >= (rows - 1) * cols:
                ax.set_xlabel(r'$\log T_{\rm eff}$', fontsize=12)
            else:
                ax.set_xticklabels([]) # Hide X-axis labels for upper rows

        fig.suptitle(f'Hertzsprung-Russell Diagram (Z = {z_value:.4f})', fontsize=16, y=1.02)

        if sc is not None:
            # Ensure colorbar is drawn correctly on all axes if there's any data
            cbar = fig.colorbar(sc, ax=axes.ravel().tolist(), orientation='vertical', fraction=0.04, pad=0.02, label='Model Number (Evolutionary Stage)')
        else:
            logging.warning(f"No scatter plot generated for Z={z_value}. Skipping colorbar for this subplot group.")


        filename = os.path.join(output_dir, f'HR_diagram_{model_name}_z{z_value:.4f}.png')
        plt.savefig(filename, bbox_inches='tight', dpi=300)
        plt.close(fig)
        logging.info(f"✔ Saved HR diagram: {filename}")

    logging.info("✅ All HR diagram generation complete.")
