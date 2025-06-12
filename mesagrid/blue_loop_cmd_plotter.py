import os
import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.path import Path # Import Path
from isochrones.mist.bc import MISTBolometricCorrectionGrid
import pkg_resources
from tqdm.auto import tqdm

# Initialize the bolometric correction grid ONCE when this module is loaded.
print("INFO: Initializing MIST Bolometric Correction Grid... (This happens once)")
bc_grid = MISTBolometricCorrectionGrid(['J', 'H', 'K', 'G', 'BP', 'RP', 'g', 'r', 'i'])
print("INFO: MIST Bolometric Correction Grid initialized.")

# Helper function to calculate BC for a single row.
def _calculate_bc_for_single_point(args):
    original_idx, teff_i, logg_i, feh_i, av_i = args
    
    # Check if any input argument is NaN before attempting interpolation
    if not (np.isfinite(teff_i) and np.isfinite(logg_i) and np.isfinite(feh_i) and np.isfinite(av_i)):
        return (original_idx, np.nan, np.nan, np.nan)

    try:
        BC_G_val = bc_grid.interp([teff_i, logg_i, feh_i, av_i], ['G'])[0]
        BC_BP_val = bc_grid.interp([teff_i, logg_i, feh_i, av_i], ['BP'])[0]
        BC_RP_val = bc_grid.interp([teff_i, logg_i, feh_i, av_i], ['RP'])[0]
        return (original_idx, BC_G_val, BC_BP_val, BC_RP_val)
    except Exception as e:
        # Log the error for this specific point, but continue processing others
        # print(f"DEBUG: Error interpolating BC for idx {original_idx} (Teff={teff_i}, logg={logg_i}, feh={feh_i}, Av={av_i}): {e}", file=sys.stderr)
        return (original_idx, np.nan, np.nan, np.nan)

def z_to_feh(Z):
    Z_sun = 0.0152
    if Z <= 0: # Z cannot be zero or negative for log10 calculation and physical reasons
        return np.nan
    return np.log10(Z / Z_sun)

def generate_blue_loop_plots_with_bc(combined_df_all_data, output_dir, output_type_label="all_blue_loop_data"):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    print(f"Generating combined blue loop HRD, CMD, and LogL-LogG plots (with BCs) to '{output_dir}'...")

    try:
        isochrones_version = pkg_resources.get_distribution('isochrones').version
        print(f"INFO: Installed isochrones version: {isochrones_version}")
    except pkg_resources.DistributionNotFound:
        print("WARNING: isochrones package not found.", file=sys.stderr)

    if combined_df_all_data.empty:
        print("Warning: No data provided for plotting. Skipping.", file=sys.stderr)
        return

    if 'Z' in combined_df_all_data.columns and 'initial_Z' not in combined_df_all_data.columns:
        combined_df_all_data.rename(columns={'Z': 'initial_Z'}, inplace=True)
        print("INFO: Renamed 'Z' column to 'initial_Z' for consistency.")
    elif 'initial_Z' not in combined_df_all_data.columns:
        print(f"ERROR: Neither 'Z' nor 'initial_Z' column found in data. Cannot proceed.", file=sys.stderr)
        return

    # --- DEBUG: Initial data status ---
    print(f"DEBUG: Initial combined_df_all_data size: {len(combined_df_all_data)} rows.")
    print(f"DEBUG: Initial non-NaN counts in critical columns:")
    print(f"  log_Teff: {combined_df_all_data['log_Teff'].count()} / {len(combined_df_all_data)}")
    print(f"  log_g: {combined_df_all_data['log_g'].count()} / {len(combined_df_all_data)}")
    print(f"  initial_Z: {combined_df_all_data['initial_Z'].count()} / {len(combined_df_all_data)}")
    
    problematic_z_count = combined_df_all_data[combined_df_all_data['initial_Z'] <= 0]['initial_Z'].count()
    if problematic_z_count > 0:
        print(f"WARNING: {problematic_z_count} points have initial_Z <= 0 and will result in NaN for [Fe/H].")
    print(f"DEBUG: Sample initial_Z values: {combined_df_all_data['initial_Z'].head().to_string(float_format='%.4e')}")
    print(f"DEBUG: Unique initial_Z values: {combined_df_all_data['initial_Z'].unique()}")
    # --- END DEBUG: Initial data status ---

    combined_df_all_data['Mbol'] = np.nan
    combined_df_all_data['M_G'] = np.nan
    combined_df_all_data['M_BP'] = np.nan
    combined_df_all_data['M_RP'] = np.nan
    combined_df_all_data['G_BP_minus_G_RP'] = np.nan

    # Filter for BC calculation: only rows with finite Teff, logg, and positive Z
    bc_input_df_filtered = combined_df_all_data.dropna(subset=['log_Teff', 'log_g', 'initial_Z']).copy()
    bc_input_df_filtered = bc_input_df_filtered[bc_input_df_filtered['initial_Z'] > 0].copy() # Ensure positive Z

    print(f"DEBUG: bc_input_df_filtered size: {len(bc_input_df_filtered)} rows after dropna and positive Z filter.")

    if bc_input_df_filtered.empty:
        print("WARNING: No valid stellar data (log_Teff, log_g, initial_Z > 0) for BC calculation. Skipping BC and magnitude calculations.", file=sys.stderr)
    else:
        tasks = []
        print("DEBUG: Preparing tasks for BC calculation...")
        for original_idx, row in bc_input_df_filtered.iterrows():
            teff_i = 10**row['log_Teff']
            logg_i = row['log_g']
            feh_i = z_to_feh(row['initial_Z']) # This will be NaN if Z <= 0, handled by _calculate_bc_for_single_point
            av_i = 0.0 # Assuming no extinction
            tasks.append((original_idx, teff_i, logg_i, feh_i, av_i))

        print(f"DEBUG: Number of tasks prepared for BC calculation: {len(tasks)}")

        print("Starting serial bolometric correction calculation...")
        raw_results = []
        for task in tqdm(tasks, desc="Calculating BCs serially"):
            raw_results.append(_calculate_bc_for_single_point(task))
        print("Serial calculation completed.")

        print(f"DEBUG: Processing BC calculation results (length: {len(raw_results)}).")
        
        results_df = pd.DataFrame(raw_results, columns=['original_idx', 'BC_G_val', 'BC_BP_val', 'BC_RP_val'])
        results_df.set_index('original_idx', inplace=True)
        
        # Assign calculated BC values back to the original DataFrame
        combined_df_all_data['BC_G'] = results_df['BC_G_val']
        combined_df_all_data['BC_BP'] = results_df['BC_BP_val']
        combined_df_all_data['BC_RP'] = results_df['BC_RP_val']

        valid_bc_calculated_count = combined_df_all_data['BC_G'].count()
        print(f"DEBUG: Total BCs calculated (non-NaN BC_G values): {valid_bc_calculated_count} points.")

    print(f"DEBUG: Shape of combined_df_all_data AFTER BC application: {combined_df_all_data.shape}")
    print(f"DEBUG: Number of non-NaN BC_G values after application: {combined_df_all_data['BC_G'].count()}")

    # Calculate absolute magnitudes and color
    combined_df_all_data['Mbol'] = -2.5 * combined_df_all_data['log_L'] + 4.74 # Mbol is independent of BCs, depends on log_L
    combined_df_all_data['M_G'] = combined_df_all_data['Mbol'] - combined_df_all_data['BC_G']
    combined_df_all_data['M_BP'] = combined_df_all_data['Mbol'] - combined_df_all_data['BC_BP']
    combined_df_all_data['M_RP'] = combined_df_all_data['Mbol'] - combined_df_all_data['BC_RP']
    combined_df_all_data['G_BP_minus_G_RP'] = combined_df_all_data['M_BP'] - combined_df_all_data['M_RP']

    print(f"DEBUG: Shape of combined_df_all_data AFTER magnitude calculations: {combined_df_all_data.shape}")
    print(f"DEBUG: Number of non-NaN Mbol values after calculation: {combined_df_all_data['Mbol'].count()}")
    print(f"DEBUG: Number of non-NaN M_G values after calculation: {combined_df_all_data['M_G'].count()}")
    print(f"DEBUG: Number of non-NaN G_BP_minus_G_RP values after calculation: {combined_df_all_data['G_BP_minus_G_RP'].count()}")

    # --- HRD Plot ---
    print("\nGenerating HRD plot...")
    fig_hrd, ax_hrd = plt.subplots(figsize=(10, 8))
    hrd_plot_df = combined_df_all_data.dropna(subset=['log_Teff', 'log_L', 'initial_Z'])
    print(f"DEBUG: HRD plot DataFrame size after dropna for plotting: {len(hrd_plot_df)} rows")
    
    # Define min_z and max_z specific to the HRD plot data
    if not hrd_plot_df.empty:
        hrd_min_z = hrd_plot_df['initial_Z'].min()
        hrd_max_z = hrd_plot_df['initial_Z'].max()
        print(f"DEBUG: HRD Plot Z range (from hrd_plot_df): min_Z={hrd_min_z:.4e}, max_Z={hrd_max_z:.4e}") # Added debug print
        cmap_hrd = plt.cm.viridis
        norm_hrd = plt.Normalize(vmin=hrd_min_z, vmax=hrd_max_z)

        ax_hrd.scatter(hrd_plot_df['log_Teff'], hrd_plot_df['log_L'],
                       c=hrd_plot_df['initial_Z'], cmap=cmap_hrd, norm=norm_hrd,
                       s=1, alpha=0.5) # Increased alpha for better visibility

        # --- Define and add Instability Strip ---
        # Vertices are in (log_Teff, log_L) coordinates
        instability_strip_vertices = np.array([
            [3.83, 2.4],   # Top-left (Blue edge, high Teff, low L)
            [3.76, 4.5],   # Bottom-left (high Teff, high L)
            [3.65, 4.5],   # Bottom-right (Red edge, low Teff, high L)
            [3.77, 2.4]    # Top-right (low Teff, low L)
        ])
        
        # Important: The HRD Teff axis is inverted! So the top-left point has the highest log_Teff value.
        # The Path needs points to connect the polygon in order.
        # Here, the order is decreasing by Teff, then increasing by L, then increasing by Teff, then decreasing by L.
        # The instability_strip_vertices points are likely already in the correct order for Path.
        
        instability_path = Path(instability_strip_vertices)
        instability_patch = patches.PathPatch(instability_path, facecolor='gray', edgecolor='black', lw=2, alpha=0.3, label='Instability Strip')
        ax_hrd.add_patch(instability_patch)
        # --- End Instability Strip ---

        ax_hrd.set_xlabel(r'$\log_{10} T_{\mathrm{eff}}$')
        ax_hrd.set_ylabel(r'$\log_{10} (L/L_{\odot})$')
        ax_hrd.set_title(f'Combined HR Diagram (All Z)')
        ax_hrd.invert_xaxis()
        fig_hrd.colorbar(plt.cm.ScalarMappable(norm=norm_hrd, cmap=cmap_hrd), ax=ax_hrd, label='Initial Z')
        ax_hrd.grid(True, linestyle='--', alpha=0.6)
        ax_hrd.legend(loc='upper right') # Add legend to show instability strip label
        fig_hrd.tight_layout()
        hrd_path = os.path.join(output_dir, f'HRD_{output_type_label}.png')
        fig_hrd.savefig(hrd_path, dpi=200)
        plt.close(fig_hrd)
        print(f"Saved HRD plot: {hrd_path}")
    else:
        plt.close(fig_hrd)
        print("No valid HRD plot for combined data after dropping NaNs.")


    # --- CMD Plot ---
    print("\nGenerating CMD plot...")
    fig_cmd, ax_cmd = plt.subplots(figsize=(10, 8))
    cmd_plot_df = combined_df_all_data.dropna(subset=['G_BP_minus_G_RP', 'M_G', 'initial_Z'])
    print(f"DEBUG: CMD plot DataFrame size after dropna for plotting: {len(cmd_plot_df)} rows")
    if not cmd_plot_df.empty:
        # Define min_z and max_z specific to the CMD plot data
        cmd_min_z = cmd_plot_df['initial_Z'].min()
        cmd_max_z = cmd_plot_df['initial_Z'].max()
        print(f"DEBUG: CMD Plot Z range (from cmd_plot_df): min_Z={cmd_min_z:.4e}, max_Z={cmd_max_z:.4e}") # Added debug print
        cmap_cmd = plt.cm.viridis
        norm_cmd = plt.Normalize(vmin=cmd_min_z, vmax=cmd_max_z)

        ax_cmd.scatter(cmd_plot_df['G_BP_minus_G_RP'], cmd_plot_df['M_G'],
                       c=cmd_plot_df['initial_Z'], cmap=cmap_cmd, norm=norm_cmd,
                       s=1, alpha=0.5) # Increased alpha for better visibility
        ax_cmd.set_xlabel(r'$G_{BP} - G_{RP}$')
        ax_cmd.set_ylabel(r'$M_G$')
        ax_cmd.set_title(f'Combined CMD (Gaia) (All Z)')
        ax_cmd.invert_yaxis()
        fig_cmd.colorbar(plt.cm.ScalarMappable(norm=norm_cmd, cmap=cmap_cmd), ax=ax_cmd, label='Initial Z')
        ax_cmd.grid(True, linestyle='--', alpha=0.6)
        fig_cmd.tight_layout()
        cmd_path = os.path.join(output_dir, f'CMD_Gaia_{output_type_label}.png')
        fig_cmd.savefig(cmd_path, dpi=200)
        plt.close(fig_cmd)
        print(f"Saved CMD plot: {cmd_path}")
    else:
        plt.close(fig_cmd)
        print(f"No valid CMD plots for combined data after dropping NaNs in G_BP_minus_G_RP or M_G.")

    # --- LogL vs LogG Plot ---
    print("\nGenerating LogL vs LogG plot...")
    fig_logg, ax_logg = plt.subplots(figsize=(10, 8))
    logg_plot_df = combined_df_all_data.dropna(subset=['log_g', 'log_L', 'initial_Z'])
    print(f"DEBUG: LogL vs LogG plot DataFrame size after dropna for plotting: {len(logg_plot_df)} rows")
    if not logg_plot_df.empty:
        # Define min_z and max_z specific to the LogL vs LogG plot data
        logg_min_z = logg_plot_df['initial_Z'].min()
        logg_max_z = logg_plot_df['initial_Z'].max()
        print(f"DEBUG: LogL vs LogG Plot Z range (from logg_plot_df): min_Z={logg_min_z:.4e}, max_Z={logg_max_z:.4e}") # Added debug print
        cmap_logg = plt.cm.viridis
        norm_logg = plt.Normalize(vmin=logg_min_z, vmax=logg_max_z)

        ax_logg.scatter(logg_plot_df['log_g'], logg_plot_df['log_L'],
                        c=logg_plot_df['initial_Z'], cmap=cmap_logg, norm=norm_logg,
                        s=1, alpha=0.5) # Increased alpha for better visibility
        ax_logg.set_xlabel(r'$\log_{10} g$')
        ax_logg.set_ylabel(r'$\log_{10} (L/L_{\odot})$')
        ax_logg.set_title(f'Combined LogL vs LogG (All Z)')
        ax_logg.invert_xaxis()
        fig_logg.colorbar(plt.cm.ScalarMappable(norm=norm_logg, cmap=cmap_logg), ax=ax_logg, label='Initial Z')
        ax_logg.grid(True, linestyle='--', alpha=0.6)
        fig_logg.tight_layout()
        logg_path = os.path.join(output_dir, f'LogL_LogG_{output_type_label}.png')
        fig_logg.savefig(logg_path, dpi=200)
        plt.close(fig_logg)
        print(f"Saved LogL vs LogG plot: {logg_path}")
    else:
        plt.close(fig_logg)
        print(f"No valid LogL vs LogG plots for combined data after dropping NaNs.")

    print("\nAll combined plots generated.")


def load_and_group_data(input_dir):
    all_dfs = []
    print(f"INFO: Loading CSV files from '{input_dir}'...")
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
        return pd.DataFrame()

    full_df = pd.concat(all_dfs, ignore_index=True)
    print(f"INFO: All CSVs concatenated. Total rows: {len(full_df)}.")

    if 'initial_Z' not in full_df.columns and 'Z' not in full_df.columns:
        print(f"ERROR: Neither 'initial_Z' nor 'Z' column found in data. Please check your CSVs.", file=sys.stderr)
        return pd.DataFrame()
    if 'initial_mass' not in full_df.columns:
        print(f"ERROR: 'initial_mass' column not found in data. Please check your CSVs.", file=sys.stderr)
        return pd.DataFrame()

    if 'Z' in full_df.columns and 'initial_Z' not in full_df.columns:
        full_df.rename(columns={'Z': 'initial_Z'}, inplace=True)
        print("INFO: Renamed 'Z' column to 'initial_Z' for consistency.")
    
    return full_df

if __name__ == "__main__":
    input_data_directory = "./blue_loop_data"
    output_plots_directory = "./blue_loop_plots"

    combined_data_for_plotting = load_and_group_data(input_data_directory)

    if not combined_data_for_plotting.empty:
        generate_blue_loop_plots_with_bc(combined_data_for_plotting, output_plots_directory)
    else:
        print("No valid data to plot.", file=sys.stderr)
