import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
from matplotlib import colors
import math


def generate_hrd_plots(grouped_detailed_dfs, plots_output_dir, model_name,
                       inst_strip_blue_edge_logT, inst_strip_blue_edge_logL,
                       inst_strip_red_edge_logT, inst_strip_red_edge_logL):
    """
    Generates HRD plots for each Z group from detailed DataFrame fragments.

    Args:
        grouped_detailed_dfs (dict): Dictionary where keys are Z values and values are lists of
                                     DataFrames containing detailed blue loop data for that Z.
        plots_output_dir (str): Directory where plot images will be saved.
        model_name (str): Name of the model (e.g., 'nad_convos') for filename/title.
        inst_strip_blue_edge_logT (list): Log Teff coordinates for the blue edge of the instability strip.
        inst_strip_blue_edge_logL (list): Log L coordinates for the blue edge of the instability strip.
        inst_strip_red_edge_logT (list): Log Teff coordinates for the red edge of the instability strip.
        inst_strip_red_edge_logL (list): Log L coordinates for the red edge of the instability strip.
    """
    print("Generating HRD plots...")
    os.makedirs(plots_output_dir, exist_ok=True)

    sorted_z_values = sorted(grouped_detailed_dfs.keys())

    for z_value in sorted_z_values:
        dfs_list = grouped_detailed_dfs.get(z_value, [])
        if not dfs_list:
            print(f'➡ No detailed data for Z={z_value:.4f}, skipping HRD plot generation for this Z.')
            continue

        print(f'➡ Processing HRD for Z={z_value:.4f}...')

        combined_z_df = pd.concat(dfs_list, ignore_index=True)

        unique_masses_for_z = sorted(combined_z_df['initial_mass'].unique())

        num_plots = len(unique_masses_for_z)
        if num_plots == 0:
            print(f"No stellar models found for Z={z_value:.4f} in combined data. Skipping HRD.")
            continue

        cols = 3
        rows = math.ceil(num_plots / cols)

        fig, axes = plt.subplots(rows, cols, figsize=(12, 14), constrained_layout=True)
        axes = axes.flatten()

        for i, mass in enumerate(unique_masses_for_z):
            ax = axes[i]

            mass_df = combined_z_df[combined_z_df['initial_mass'] == mass].copy()

            if mass_df.empty or 'log_Teff' not in mass_df.columns or 'log_L' not in mass_df.columns or 'model_number' not in mass_df.columns:
                ax.set_title(f'{mass:.1f} M$_\odot$\n(No Data)')
                ax.set_xticks([])
                ax.set_yticks([])
                continue

            log_Teff = mass_df['log_Teff']
            log_L = mass_df['log_L']
            model_number = mass_df['model_number']

            # Apply the max_Teff_index logic for post-MS evolution
            max_Teff_overall_index = log_Teff.idxmax()

            log_Teff_filtered = log_Teff.loc[max_Teff_overall_index:].reset_index(drop=True)
            log_L_filtered = log_L.loc[max_Teff_overall_index:].reset_index(drop=True)
            model_number_filtered = model_number.loc[max_Teff_overall_index:].reset_index(drop=True)

            if log_Teff_filtered.empty:
                 ax.set_title(f'{mass:.1f} M$_\odot$\n(No Evolution)')
                 ax.set_xticks([])
                 ax.set_yticks([])
                 continue

            norm = colors.Normalize(vmin=np.min(model_number_filtered), vmax=np.max(model_number_filtered))
            cmap = plt.cm.viridis

            sc = ax.scatter(log_Teff_filtered, log_L_filtered, c=model_number_filtered, cmap=cmap, label=f'{mass:.1f} M$_\odot$', s=10, norm=norm, zorder=2)

            ax.plot(inst_strip_blue_edge_logT, inst_strip_blue_edge_logL, color='blue', linestyle='dashed', zorder=1)
            ax.plot(inst_strip_red_edge_logT, inst_strip_red_edge_logL, color='red', linestyle='dashed', zorder=1)

            ax.set_title(f'{mass:.1f} M$_\odot$', fontsize=10)
            ax.invert_xaxis()

        for j in range(i + 1, len(axes)):
            fig.delaxes(axes[j])

        for j, ax in enumerate(axes):
            if j < num_plots:
                if j % cols == 0:
                    ax.set_ylabel(r'$\log L$', fontsize=12)
                else:
                    ax.set_yticklabels([])

                if j >= (rows - 1) * cols:
                    ax.set_xlabel(r'$\log T_{\rm eff}$', fontsize=12)
                else:
                    ax.set_xticklabels([])

        fig.suptitle(f'{model_name} HR Diagrams for Z = {z_value:.4f}', fontsize=16)

        if sc is not None:
            fig.colorbar(sc, ax=axes.ravel().tolist(), orientation='vertical', label='Model number', pad=0.01)

        filename = os.path.join(plots_output_dir, f'HR_diagram_{model_name}_z{z_value:.4f}.png')
        plt.savefig(filename, bbox_inches='tight', dpi=300)
        plt.close(fig)
        print(f"Saved HRD for Z={z_value:.4f} to {filename}")

    print("HRD plot generation complete.")