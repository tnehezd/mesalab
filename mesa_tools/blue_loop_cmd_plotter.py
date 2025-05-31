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

# --- MAJOR MODIFICATIONS START HERE ---
def generate_blue_loop_plots_with_bc(combined_df_all_data, output_dir, output_type_label="all_blue_loop_data"):
    """
    Generates HRD, CMD (G_BP - G_RP vs G), and LogL-LogG plots for all blue loop stellar models
    in a single plot, including bolometric corrections for Gaia magnitudes.

    Args:
        combined_df_all_data (pd.DataFrame): DataFrame with all stellar data (all Z and masses).
        output_dir (str): Directory to save plots.
        output_type_label (str): Label to include in output filenames.
    """
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    print(f"Generating combined blue loop HRD, CMD, and LogL-LogG plots (with BCs) to '{output_dir}'...")

    try:
        isochrones_version = pkg_resources.get_distribution('isochrones').version
        print(f"INFO: Installed isochrones version: {isochrones_version}")
    except pkg_resources.DistributionNotFound:
        print("WARNING: isochrones package not found.", file=sys.stderr)

    cmd_plotting_possible = bc_grid is not None
    if not cmd_plotting_possible:
        print(f"WARNING: Skipping CMD plots due to BC grid initialization failure.", file=sys.stderr)

    if combined_df_all_data.empty:
        print("Warning: No data provided for plotting. Skipping.", file=sys.stderr)
        return

    # It's good practice to rename 'Z' to 'initial_Z' as it's typically how it's named in MESA output
    # Or ensure your input CSVs have 'initial_Z' column.
    # I'll assume 'initial_Z' and 'initial_mass' for consistency with cli.py.
    # If your CSVs only have 'Z', you might need to rename it here:
    # combined_df_all_data.rename(columns={'Z': 'initial_Z'}, inplace=True)

    unique_masses = np.sort(combined_df_all_data['initial_mass'].unique())
    unique_zs = np.sort(combined_df_all_data['initial_Z'].unique())

    fig_hrd, ax_hrd = plt.subplots(figsize=(10, 8))
    fig_cmd, ax_cmd = plt.subplots(figsize=(10, 8))
    fig_logg, ax_logg = plt.subplots(figsize=(10, 8))

    at_least_one_cmd_plot = False

    # Iterate over both mass and Z to plot each individual track on the same figure
    for mass_val in unique_masses:
        mass_df = combined_df_all_data[combined_df_all_data['initial_mass'] == mass_val].copy()

        for Z_val in unique_zs:
            subset_df = mass_df[mass_df['initial_Z'] == Z_val].copy()
            if subset_df.empty:
                continue # Skip if no data for this specific Z and mass combination

            feh_val = z_to_feh(Z_val)
            if not np.isfinite(feh_val):
                print(f"ERROR: Non-finite [Fe/H] for Z={Z_val}. Skipping subset for mass={mass_val}.", file=sys.stderr)
                continue

            label_text = f'M={mass_val:.1f}, Z={Z_val:.4f}' # Updated label for clarity

            # HRD plot (log_Teff vs log_L)
            hrd_df = subset_df.dropna(subset=['log_Teff', 'log_L'])
            if not hrd_df.empty:
                ax_hrd.plot(hrd_df['log_Teff'], hrd_df['log_L'], label=label_text, lw=1.5)

            # LogL vs log_g plot
            logg_df = subset_df.dropna(subset=['log_g', 'log_L'])
            if not logg_df.empty:
                ax_logg.plot(logg_df['log_g'], logg_df['log_L'], label=label_text, lw=1.5)

            # CMD plot with bolometric corrections
            if cmd_plotting_possible:
                bc_req_cols = ['log_Teff', 'log_g', 'log_L']
                bc_df = subset_df.dropna(subset=bc_req_cols).copy()
                if bc_df.empty:
                    continue # Skip if no valid data for BCs

                teff = 10**bc_df['log_Teff'].values
                logg = bc_df['log_g'].values

                BC_G, BC_BP, BC_RP = [], [], []

                for i in range(len(bc_df)):
                    params = np.array([[teff[i], logg[i], feh_val, 0.0]]) # Av=0 for each point
                    try:
                        # bc_grid.interp returns an array of shape (N_points, N_filters)
                        bc_values = bc_grid.interp(params, ['G', 'BP', 'RP'])
                        BC_G.append(bc_values[0, 0])
                        BC_BP.append(bc_values[0, 1])
                        BC_RP.append(bc_values[0, 2])
                    except Exception as e:
                        # print(f"Warning: BC interpolation error at index {i} for M={mass_val}, Z={Z_val}: {e}")
                        BC_G.append(np.nan)
                        BC_BP.append(np.nan)
                        BC_RP.append(np.nan)

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
                    ax_cmd.plot(cmd_df['G_BP_minus_G_RP'], cmd_df['M_G'], label=label_text, alpha=0.7, lw=1.5)
                    at_least_one_cmd_plot = True
    # --- MAJOR MODIFICATIONS END HERE ---

    # HRD figure settings
    ax_hrd.set_xlabel(r'$\log_{10} T_{\mathrm{eff}}$')
    ax_hrd.set_ylabel(r'$\log_{10} (L/L_{\odot})$')
    ax_hrd.set_title(f'Combined HR Diagram (All Z)') # Updated title
    ax_hrd.invert_xaxis()
    ax_hrd.legend(title='Initial Mass, Z', loc='best', fontsize='small', ncol=2) # Updated legend title and columns
    ax_hrd.grid(True, linestyle='--', alpha=0.6)
    fig_hrd.tight_layout()
    hrd_path = os.path.join(output_dir, f'HRD_{output_type_label}.png') # Updated filename
    fig_hrd.savefig(hrd_path, dpi=200)
    plt.close(fig_hrd)
    print(f"Saved HRD plot: {hrd_path}")

    # CMD figure settings
    if at_least_one_cmd_plot:
        ax_cmd.set_xlabel(r'$G_{BP} - G_{RP}$')
        ax_cmd.set_ylabel(r'$M_G$')
        ax_cmd.set_title(f'Combined CMD (Gaia) (All Z)') # Updated title
        ax_cmd.invert_yaxis()
        ax_cmd.legend(title='Initial Mass, Z', loc='best', fontsize='small', ncol=2) # Updated legend title and columns
        ax_cmd.grid(True, linestyle='--', alpha=0.6)
        fig_cmd.tight_layout()
        cmd_path = os.path.join(output_dir, f'CMD_Gaia_{output_type_label}.png') # Updated filename
        fig_cmd.savefig(cmd_path, dpi=200)
        plt.close(fig_cmd)
        print(f"Saved CMD plot: {cmd_path}")
    else:
        plt.close(fig_cmd)
        print(f"No valid CMD plots for combined data.")

    # LogL vs LogG figure settings
    ax_logg.set_xlabel(r'$\log_{10} g$')
    ax_logg.set_ylabel(r'$\log_{10} (L/L_{\odot})$')
    ax_logg.set_title(f'Combined LogL vs LogG (All Z)') # Updated title
    ax_logg.invert_xaxis()
    ax_logg.legend(title='Initial Mass, Z', loc='best', fontsize='small', ncol=2) # Updated legend title and columns
    ax_logg.grid(True, linestyle='--', alpha=0.6)
    fig_logg.tight_layout()
    logg_path = os.path.join(output_dir, f'LogL_LogG_{output_type_label}.png') # Updated filename
    fig_logg.savefig(logg_path, dpi=200)
    plt.close(fig_logg)
    print(f"Saved LogL vs LogG plot: {logg_path}")

    print("All combined plots generated.")

def load_and_group_data(input_dir):
    # Renamed for clarity, though it still returns a single DataFrame.
    """
    Loads all CSV files in input_dir, concatenates them into a single DataFrame.

    Assumes CSV files contain at least these columns:
    - initial_Z (metallicity)
    - initial_mass
    - log_Teff, log_L, log_g (needed for plots)

    Args:
        input_dir (str): Directory containing CSV files.

    Returns:
        pd.DataFrame: A single DataFrame with all data.
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
        return pd.DataFrame() # Return empty DataFrame on error

    full_df = pd.concat(all_dfs, ignore_index=True)

    # Check for required columns based on MESA output conventions
    if 'initial_Z' not in full_df.columns:
        print(f"ERROR: 'initial_Z' column not found in data. Please check your CSVs.", file=sys.stderr)
        return pd.DataFrame()
    if 'initial_mass' not in full_df.columns:
        print(f"ERROR: 'initial_mass' column not found in data. Please check your CSVs.", file=sys.stderr)
        return pd.DataFrame()

    # Instead of grouping, it now simply returns the concatenated DataFrame.
    return full_df

if __name__ == "__main__":
    # Customize these paths:
    input_data_directory = "./blue_loop_data"   # Folder with input CSV files
    output_plots_directory = "./blue_loop_plots" # Folder to save plots

    # load_and_group_data now returns a single DataFrame
    combined_data_for_plotting = load_and_group_data(input_data_directory)
    if not combined_data_for_plotting.empty:
        # Call the function with the combined DataFrame
        generate_blue_loop_plots_with_bc(combined_data_for_plotting, output_plots_directory)
    else:
        print("No valid data to plot.", file=sys.stderr)