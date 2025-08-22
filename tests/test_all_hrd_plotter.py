import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os
import math
import logging

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
        try:
            current_z_dfs.sort(key=lambda df: df['initial_mass'].iloc[0])
            logging.info(f"Sorted {len(current_z_dfs)} runs by mass for Z={z_value:.4f}.")
        except KeyError:
            logging.warning(f" 'initial_mass' column not found for Z={z_value:.4f}. Skipping mass sort.")
        except IndexError:
            logging.warning(f"DataFrame for Z={z_value:.4f} is empty or 'initial_mass' column has no data. Skipping mass sort.")

        logging.info(f'➡ Processing HR diagrams for Z={z_value:.4f} with {len(current_z_dfs)} masses...')

        num_plots = len(current_z_dfs)

        if num_plots == 0:
            logging.warning(f"No runs found for Z={z_value:.4f}. Skipping HR diagram generation for this metallicity.")
            continue

        cols = 4 # Fixed to 4 columns as requested
        rows = math.ceil(num_plots / cols)

        # Define base dimensions for a single subplot to achieve 1:2 ratio
        base_subplot_height = 5  # Height in inches for each subplot
        base_subplot_width = base_subplot_height * 2 # Ensures 1:2 ratio, so 10 inches

        fig_width = cols * base_subplot_width
        fig_height = rows * base_subplot_height

        # Create the subplots
        fig, axes = plt.subplots(rows, cols, figsize=(fig_width, fig_height), constrained_layout=True)

        # --- FIX: Ensure axes is always a flattened array for consistent iteration ---
        # The original code's `if num_plots == 1` block was the source of the bug
        # by trying to wrap an already-arrayed object, resulting in a nested array.
        # This fix correctly handles all cases by unconditionally flattening.
        axes = np.array(axes).flatten()

        sc = None  # Default scatter plot reference for colorbar

        # Iterate directly over the DataFrames in current_z_dfs
        for i, df_full_history in enumerate(current_z_dfs):
            # Check if there are more plots than axes (this is an edge case but good to handle)
            if i >= len(axes):
                logging.warning(f"More plots than available axes. Stopping at {i} for Z={z_value:.4f}.")
                break
                
            ax = axes[i] # Get the current subplot axis

            # Extract mass and Z from the DataFrame itself
            mass = df_full_history['initial_mass'].iloc[0] # Mass for title

            # --- Pre-MS Phase Trimming Logic (ZAMS detection) ---
            df_post_prems = None # Initialize to None

            # Check for missing required columns or empty DataFrame
            if 'log_L' not in df_full_history.columns or df_full_history.empty:
                logging.warning(f"Missing 'log_L' or empty DataFrame for M={mass:.1f} (Z={z_value:.4f}). Skipping plot.")
                ax.set_visible(False)
                continue # Skip to the next run_info

            if drop_zams: # Only trim if drop_zams is True
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
            ax.set_title(f'{mass:.1f} M$_{r}\odot$', fontsize=15)
            # ax.set_xlabel(r'log(T$_{\rm eff}$/K)', fontsize=12) # Already set on fig
            # ax.set_ylabel(r'log(L/L$_\odot$)', fontsize=12) # Already set on fig
            ax.invert_xaxis()
            ax.set_aspect(aspect='equal', adjustable='box')
            ax.grid(True)
            ax.set_facecolor('#f0f0f0')

            sc = ax.scatter(
                log_Teff,
                log_L,
                c=model_number,
                cmap='viridis',
                s=5,
                zorder=2,
            )

            ax.plot(
                logT_blue_edge,
                logL_blue_edge,
                linestyle='--',
                color='skyblue',
                linewidth=2,
                label='Instability Strip',
                zorder=1
            )
            ax.plot(
                logT_red_edge,
                logL_red_edge,
                linestyle='--',
                color='skyblue',
                linewidth=2,
                zorder=1
            )
            ax.fill_betweenx(
                logL_blue_edge,
                logT_blue_edge,
                logT_red_edge,
                color='skyblue',
                alpha=0.3,
                label='_nolegend_',
                zorder=0
            )

        # Turn off any unused subplots
        for i in range(num_plots, len(axes)):
            axes[i].set_visible(False)

        # Set common labels and titles for the figure
        fig.suptitle(f'{model_name.upper()} HR Diagrams (Z={z_value:.4f})', fontsize=20)
        fig.text(0.5, 0.04, r'log(T$_{\rm eff}$/K)', ha='center', fontsize=18)
        fig.text(0.04, 0.5, r'log(L/L$_\odot$)', va='center', rotation='vertical', fontsize=18)

        # Add colorbar for the figure, only if at least one plot was successful
        if sc:
            fig.colorbar(sc, ax=axes, orientation='vertical', label='Model Number', shrink=0.7)
        
        # Save the figure
        output_filename = os.path.join(output_dir, f'{model_name}_hr_diagrams_Z_{z_value:.4f}.png')
        plt.savefig(output_filename, bbox_inches='tight', dpi=150)
        logging.info(f'Saved HR diagram for Z={z_value:.4f} to {output_filename}')
        plt.close(fig)

    logging.info("HR diagram generation complete.")

