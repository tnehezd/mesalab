import os
import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from isochrones.mist.bc import MISTBolometricCorrectionGrid
import pkg_resources

# Initialize the bolometric correction grid ONCE when this module is loaded.
# Include Gaia bands (G, BP, RP) along with UBVRI.
try:
    bc_grid = MISTBolometricCorrectionGrid(['U', 'B', 'V', 'R', 'I', 'G', 'BP', 'RP'])
except Exception as e:
    print(f"ERROR: Error initializing MISTBolometricCorrectionGrid: {e}", file=sys.stderr)
    print("WARNING: Bolometric corrections will not be available for CMD plots.", file=sys.stderr)
    bc_grid = None

def z_to_feh(Z):
    """Converts metallicity (Z) to [Fe/H]."""
    Z_sun = 0.0152
    if Z <= 0:
        print(f"WARNING: Z value is non-positive ({Z}). Returning NaN for [Fe/H].", file=sys.stderr)
        return np.nan
    return np.log10(Z / Z_sun)

def generate_blue_loop_plots_with_bc(grouped_detailed_dfs, output_dir, output_type_label="combined_blue_loop_data"):
    """
    Generates HRD, CMD (G_BP - G_RP vs G), and LogL-LogG plots for blue loop stellar models,
    including bolometric corrections for Gaia magnitudes.

    Args:
        grouped_detailed_dfs (dict): Keys are Z values, values are DataFrames with stellar data.
        output_dir (str): Directory to save plots.
        output_type_label (str): Label to include in output filenames.
    """
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    print(f"Generating blue loop HRD, CMD, and LogL-LogG plots (with BCs) to '{output_dir}'...")

    try:
        isochrones_version = pkg_resources.get_distribution('isochrones').version
        print(f"INFO: Installed isochrones version: {isochrones_version}")
    except pkg_resources.DistributionNotFound:
        print("WARNING: isochrones package not found.", file=sys.stderr)

    cmd_plotting_possible = bc_grid is not None
    if not cmd_plotting_possible:
        print(f"WARNING: Skipping CMD plots due to BC grid initialization failure.", file=sys.stderr)

    for Z_val, combined_df_bl in grouped_detailed_dfs.items():
        if combined_df_bl.empty:
            print(f"Warning: No data for Z={Z_val}. Skipping.")
            continue

        unique_masses = np.sort(combined_df_bl['initial_mass'].unique())
        feh_val = z_to_feh(Z_val)

        if not np.isfinite(feh_val):
            print(f"ERROR: Non-finite [Fe/H] for Z={Z_val}. Skipping.", file=sys.stderr)
            continue

        fig_hrd, ax_hrd = plt.subplots(figsize=(10, 8))
        fig_cmd, ax_cmd = plt.subplots(figsize=(10, 8))
        fig_logg, ax_logg = plt.subplots(figsize=(10, 8))

        at_least_one_cmd_plot = False

        for mass_val in unique_masses:
            mass_df = combined_df_bl[combined_df_bl['initial_mass'] == mass_val].copy()
            if mass_df.empty:
                print(f"Warning: No data for mass={mass_val}, Z={Z_val}. Skipping mass.")
                continue

            # HRD plot (log_Teff vs log_L)
            hrd_df = mass_df.dropna(subset=['log_Teff', 'log_L'])
            if not hrd_df.empty:
                ax_hrd.plot(hrd_df['log_Teff'], hrd_df['log_L'], label=f'M={mass_val:.1f}', lw=1.5)
            else:
                print(f"Warning: No HRD data for mass={mass_val}, Z={Z_val}")

            # LogL vs log_g plot
            logg_df = mass_df.dropna(subset=['log_g', 'log_L'])
            if not logg_df.empty:
                ax_logg.plot(logg_df['log_g'], logg_df['log_L'], label=f'M={mass_val:.1f}', lw=1.5)
            else:
                print(f"Warning: No LogL-LogG data for mass={mass_val}, Z={Z_val}")

            # CMD plot with bolometric corrections
            if cmd_plotting_possible:
                bc_req_cols = ['log_Teff', 'log_g', 'log_L']
                bc_df = mass_df.dropna(subset=bc_req_cols).copy()
                if bc_df.empty:
                    print(f"Warning: No BC data for mass={mass_val}, Z={Z_val}. Skipping CMD for this mass.")
                    continue

                teff = 10**bc_df['log_Teff'].values
                logg = bc_df['log_g'].values

                BC_G, BC_BP, BC_RP = [], [], []

                for i in range(len(bc_df)):
                    params = [teff[i], logg[i], feh_val, 0.0]  # Av=0
                    try:
                        bc_g = bc_grid.interp(params, ['G'])[0]
                        bc_bp = bc_grid.interp(params, ['BP'])[0]
                        bc_rp = bc_grid.interp(params, ['RP'])[0]
                    except Exception as e:
                        print(f"Warning: BC interpolation error at index {i} for mass={mass_val}, Z={Z_val}: {e}")
                        bc_g, bc_bp, bc_rp = np.nan, np.nan, np.nan

                    BC_G.append(bc_g)
                    BC_BP.append(bc_bp)
                    BC_RP.append(bc_rp)

                bc_df['BC_G'] = BC_G
                bc_df['BC_BP'] = BC_BP
                bc_df['BC_RP'] = BC_RP

                M_bol = -2.5 * bc_df['log_L'].values + 4.74
                bc_df['M_G'] = M_bol - bc_df['BC_G']
                bc_df['M_BP'] = M_bol - bc_df['BC_BP']
                bc_df['M_RP'] = M_bol - bc_df['BC_RP']
                bc_df['G_BP_minus_G_RP'] = bc_df['M_BP'] - bc_df['M_RP']

                cmd_df = bc_df.dropna(subset=['G_BP_minus_G_RP', 'M_G'])
                if not cmd_df.empty:
                    ax_cmd.plot(cmd_df['G_BP_minus_G_RP'], cmd_df['M_G'], label=f'M={mass_val:.1f}', alpha=0.7, lw=1.5)
                    at_least_one_cmd_plot = True
                else:
                    print(f"Warning: No valid CMD data for mass={mass_val}, Z={Z_val}")

        # HRD figure settings
        ax_hrd.set_xlabel(r'$\log_{10} T_{\mathrm{eff}}$')
        ax_hrd.set_ylabel(r'$\log_{10} (L/L_{\odot})$')
        ax_hrd.set_title(f'HR Diagram (Z={Z_val:.4f})')
        ax_hrd.invert_xaxis()
        ax_hrd.legend(title='Initial Mass', loc='best')
        ax_hrd.grid(True, linestyle='--', alpha=0.6)
        fig_hrd.tight_layout()
        hrd_path = os.path.join(output_dir, f'HRD_Z{Z_val:.4f}_{output_type_label}.png')
        fig_hrd.savefig(hrd_path, dpi=200)
        plt.close(fig_hrd)
        print(f"Saved HRD plot: {hrd_path}")

        # CMD figure settings
        if at_least_one_cmd_plot:
            ax_cmd.set_xlabel(r'$G_{BP} - G_{RP}$')
            ax_cmd.set_ylabel(r'$M_G$')
            ax_cmd.set_title(f'CMD (Gaia) (Z={Z_val:.4f})')
            ax_cmd.invert_yaxis()
            ax_cmd.legend(title='Initial Mass', loc='best')
            ax_cmd.grid(True, linestyle='--', alpha=0.6)
            fig_cmd.tight_layout()
            cmd_path = os.path.join(output_dir, f'CMD_Gaia_Z{Z_val:.4f}_{output_type_label}.png')
            fig_cmd.savefig(cmd_path, dpi=200)
            plt.close(fig_cmd)
            print(f"Saved CMD plot: {cmd_path}")
        else:
            plt.close(fig_cmd)
            print(f"No valid CMD plots for Z={Z_val:.4f}")

        # LogL vs LogG figure settings
        ax_logg.set_xlabel(r'$\log_{10} g$')
        ax_logg.set_ylabel(r'$\log_{10} (L/L_{\odot})$')
        ax_logg.set_title(f'LogL vs LogG (Z={Z_val:.4f})')
        ax_logg.invert_xaxis()
        ax_logg.legend(title='Initial Mass', loc='best')
        ax_logg.grid(True, linestyle='--', alpha=0.6)
        fig_logg.tight_layout()
        logg_path = os.path.join(output_dir, f'LogL_LogG_Z{Z_val:.4f}_{output_type_label}.png')
        fig_logg.savefig(logg_path, dpi=200)
        plt.close(fig_logg)
        print(f"Saved LogL vs LogG plot: {logg_path}")

    print("All plots generated.")

def load_and_group_data(input_dir):
    """
    Loads all CSV files in input_dir, concatenates them,
    and groups by unique Z values into a dict of DataFrames.

    Assumes CSV files contain at least these columns:
    - Z (metallicity)
    - initial_mass
    - log_Teff, log_L, log_g (needed for plots)

    Args:
        input_dir (str): Directory containing CSV files.

    Returns:
        dict: {Z_value: DataFrame_with_all_data_for_that_Z}
    """
    all_dfs = []
    for filename in os.listdir(input_dir):
        if filename.lower().endswith('.csv'):
            filepath = os.path.join(input_dir, filename)
            try:
                df = pd.read_csv(filepath)
                all_dfs.append(df)
                print(f"Loaded {filename} with {len(df)} rows.")
            except Exception as e:
                print(f"Error loading {filename}: {e}", file=sys.stderr)

    if not all_dfs:
        print(f"ERROR: No CSV files loaded from '{input_dir}'.", file=sys.stderr)
        return {}

    full_df = pd.concat(all_dfs, ignore_index=True)

    if 'Z' not in full_df.columns:
        print(f"ERROR: 'Z' column not found in data.", file=sys.stderr)
        return {}

    grouped = {}
    for Z_val in full_df['Z'].unique():
        grouped[Z_val] = full_df[full_df['Z'] == Z_val].copy()

    return grouped

if __name__ == "__main__":
    # Customize these paths:
    input_data_directory = "./blue_loop_data"   # Folder with input CSV files
    output_plots_directory = "./blue_loop_plots"  # Folder to save plots

    grouped_data = load_and_group_data(input_data_directory)
    if grouped_data:
#        generate_blue_loop_plots_with_bc(grouped_data, output_plots_directory)
        generate_blue_loop_plots_with_bc(full_df, output_plots_directory)
    else:
        print("No valid data to plot.", file=sys.stderr)
