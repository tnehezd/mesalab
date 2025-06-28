import pandas as pd
import os
import re
from tqdm import tqdm
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pygyre
from scipy.signal import find_peaks
from scipy.integrate import trapezoid

# Import multiprocessing
from multiprocessing import Pool, cpu_count

# --- Configuration ---
# IMPORTANT: Adjust these paths to precisely match your system's setup.

# Base directory of your 'mesa_blue_loop' project.
project_base_dir = "/home/tnd/mesagrid/mesa_blue_loop"

# This is the base directory where your GYRE output 'run_...' folders are located.
gyre_results_base_dir = os.path.join(project_base_dir, "output_mid", "gyre_output")

# This is the base directory where your MESA run LOGS folders are located.
# PLEASE DOUBLE-CHECK THIS PATH TO ENSURE IT'S CORRECT FOR YOUR SETUP!
mesa_logs_base_dir = "/home/tnd/mesa-r23.05.1/STRANGE/nad_convos_mid_less"

# Name of the output CSV file (will contain all collected data).
output_csv_filename = "gyre_modes_with_mesa_params.csv"

# Output directory for plots (within project_base_dir, will be created if it doesn't exist).
# Módosítva, hogy a 'gyre_profiles' alkönyvtárat is tartalmazza
plot_output_dir = os.path.join(project_base_dir, "output_mid", "plots")
gyre_profiles_plot_dir = os.path.join(plot_output_dir, "gyre_profiles") # Új mappa a radiális profiloknak

# Ensure the plot output directories exist
os.makedirs(plot_output_dir, exist_ok=True)
os.makedirs(gyre_profiles_plot_dir, exist_ok=True) # Új mappa létrehozása

# --- Global definition for detail file columns (for .TXT files) ---
detail_columns = [
    'Gamma_1', 'P', 'T', 'dW_dx', 'kap_T', 'kap_rho', 'rho', 'x',
    'Re(xi_h)', 'Im(xi_h)', 'Re(xi_r)', 'Im(xi_r)'
]

# --- Function to parse MESA run parameters from directory names ---
def parse_mesa_run_name(run_name):
    match_mass = re.search(r'(\d+\.?\d*)MSUN', run_name)
    match_z = re.search(r'z(\d+\.?\d*)', run_name)
    mass = float(match_mass.group(1)) if match_mass else np.nan
    z = float(match_z.group(1)) if match_z else np.nan
    return mass, z

# --- Data Collection Function ---
def collect_gyre_mesa_data():
    all_gyre_modes = []

    print("Starting data collection from GYRE and MESA results...")

    # --- calculate_integrated_work_in_zones function (No change here) ---
    def calculate_integrated_work_in_zones(x, dW_dx, gamma_1):
        """
        Calculates the integrated work (dW/dx) in the He II and H/He I ionization zones.
        Identifies zones based on Gamma_1 dips, prioritizing the outermost two significant dips.
        Returns a tuple: (integrated_work_HeII, integrated_work_HHeI)
        """
        integrated_work_HeII = np.nan
        integrated_work_HHeI = np.nan

        # Basic checks for valid input data
        if len(gamma_1) < 10 or np.all(np.isnan(gamma_1)) or np.all(np.isnan(dW_dx)):
            return integrated_work_HeII, integrated_work_HHeI

        # Ensure x and dW_dx are Series for consistent indexing with .iloc
        x_series = pd.Series(x)
        dW_dx_series = pd.Series(dW_dx)

        # 1. Smooth Gamma_1 to reduce noise and highlight major dips
        # Adjust window size if needed (e.g., 5 to 9 depending on profile smoothness)
        gamma_1_smoothed = pd.Series(gamma_1).rolling(window=7, center=True, min_periods=1).mean().values

        # 2. Find peaks in the negative smoothed Gamma_1 (which correspond to dips)
        # prominence: Controls the "depth" of the dip to be considered significant.
        #             Adjust this value (e.g., 0.05, 0.1, 0.02) based on your models.
        # distance: Minimum horizontal distance (in data points) between detected dips.
        #           Adjust this to avoid detecting too many closely spaced noise dips.
        peaks, properties = find_peaks(-gamma_1_smoothed, prominence=0.05, distance=len(gamma_1_smoothed) // 10)

        # 3. Collect significant dips' x-coordinates and original indices, then sort by x-coordinate in descending order
        # This means the outermost dip (largest x) comes first.
        significant_dips_info = sorted(
            [(x_series.iloc[p], p, properties['prominences'][i]) for i, p in enumerate(peaks)],
            key=lambda item: item[0], # Sort by x-coordinate
            reverse=True              # Descending order (outermost first)
        )

        # Initialize dip locations
        he_i_h_dip_x = np.nan
        he_i_h_dip_idx = -1
        he_ii_dip_x = np.nan
        he_ii_dip_idx = -1

        # 4. Identify the two outermost (largest x) significant dips
        # Assume the outermost is H/He I and the next is He II
        if len(significant_dips_info) >= 1:
            he_i_h_dip_x, he_i_h_dip_idx, _ = significant_dips_info[0]

        if len(significant_dips_info) >= 2:
            he_ii_dip_x, he_ii_dip_idx, _ = significant_dips_info[1]
        
        # Heuristic for cases where only one significant dip is found
        # If there's only one dip and it's sufficiently deep (e.g., x < 0.5),
        # it's more likely to be the He II zone.
        elif len(significant_dips_info) == 1:
            # You might need to adjust the x-threshold (e.g., 0.5) based on your stellar models
            # to differentiate between a lone H/He I or a lone He II.
            # A common approach is to expect He II closer to the core.
            if he_i_h_dip_x < 0.5: # If the single dip is relatively deep in the star
                he_ii_dip_x = he_i_h_dip_x
                he_ii_dip_idx = he_i_h_dip_idx
                he_i_h_dip_x = np.nan # No clear H/He I if only one deep dip
            # Else, if the single dip is at x >= 0.5, it's likely H/He I, and He II is not found (remains nan)


        # Integration window size around the dip
        delta_x_zone = 0.03 # +/- 0.03 in fractional radius around the dip

        # Integrate work in He II zone
        if not np.isnan(he_ii_dip_x):
            x_start_heii = max(0, he_ii_dip_x - delta_x_zone)
            x_end_heii = min(1, he_ii_dip_x + delta_x_zone)
            heii_indices = (x_series >= x_start_heii) & (x_series <= x_end_heii)
            if np.any(heii_indices):
                integrated_work_HeII = trapezoid(dW_dx_series[heii_indices], x_series[heii_indices])

        # Integrate work in H/He I zone
        if not np.isnan(he_i_h_dip_x):
            x_start_hhei = max(0, he_i_h_dip_x - delta_x_zone)
            x_end_hhei = min(1, he_i_h_dip_x + delta_x_zone)
            hhei_indices = (x_series >= x_start_hhei) & (x_series <= x_end_hhei)
            if np.any(hhei_indices):
                integrated_work_HHeI = trapezoid(dW_dx_series[hhei_indices], x_series[hhei_indices])
        
        return integrated_work_HeII, integrated_work_HHeI
    # --- END calculate_integrated_work_in_zones function ---


    run_directories = [d for d in os.listdir(gyre_results_base_dir)
                       if os.path.isdir(os.path.join(gyre_results_base_dir, d)) and d.startswith("run")]

    for run_dir_name in tqdm(run_directories, desc="Processing MESA runs"):
        current_gyre_run_path = os.path.join(gyre_results_base_dir, run_dir_name)

        mass, z = parse_mesa_run_name(run_dir_name)
        if pd.isna(mass) or pd.isna(z):
            tqdm.write(f"Warning: Could not parse mass or Z from directory name: {run_dir_name}. Skipping this run.")
            continue

        corresponding_mesa_logs_path = os.path.join(mesa_logs_base_dir, run_dir_name, "LOGS")

        profiles_index_path = os.path.join(corresponding_mesa_logs_path, "profiles.index")
        profile_to_model = {}

        if os.path.exists(profiles_index_path):
            try:
                with open(profiles_index_path, 'r') as f:
                    lines = f.readlines()[1:]
                    for line in lines:
                        parts = line.strip().split()
                        if len(parts) == 3:
                            model_num, _, profile_num = map(int, parts)
                            profile_to_model[profile_num] = model_num
            except Exception as e:
                tqdm.write(f"   Error reading profiles.index in {run_dir_name}: {e}. Skipping this run.")
                continue
        else:
            tqdm.write(f"   profiles.index not found for {run_dir_name} at {profiles_index_path}. Skipping this run.")
            continue

        history_file_path = os.path.join(corresponding_mesa_logs_path, "history.data")
        mesa_history_df = pd.DataFrame()

        if os.path.exists(history_file_path):
            try:
                with open(history_file_path, 'r') as f:
                    lines = f.readlines()
                header_line = lines[5].strip().split()

                mesa_history_df = pd.read_csv(history_file_path,
                                               skiprows=6,
                                               sep=r'\s+',
                                               names=header_line,
                                               engine='python')

                required_history_cols = ['model_number', 'log_L', 'log_Teff', 'star_mass']
                if not all(col in mesa_history_df.columns for col in required_history_cols):
                    tqdm.write(f"Warning: Missing required columns in history.data for {run_dir_name}. Available: {mesa_history_df.columns.tolist()}. Skipping this run.")
                    continue

            except Exception as e:
                tqdm.write(f"Error reading {history_file_path}: {e}. Skipping this run.")
                continue
        else:
            tqdm.write(f"Error: MESA history.data not found for {run_dir_name} at {history_file_path}. Skipping this run.")
            continue

        profile_folders = [d for d in os.listdir(current_gyre_run_path)
                           if os.path.isdir(os.path.join(current_gyre_run_path, d)) and d.startswith("profile")]

        if not profile_folders:
            tqdm.write(f"   No 'profileXXXXX' folders found in {current_gyre_run_path}. Skipping this run.")
            continue

        for profile_folder_name in profile_folders:
            current_profile_path = os.path.join(current_gyre_run_path, profile_folder_name)
            summary_h5_path = os.path.join(current_profile_path, "summary.h5")

            profile_value_match = re.search(r'profile(\d+)', profile_folder_name)
            if not profile_value_match:
                tqdm.write(f"   Warning: Could not extract profile number from folder name: {profile_folder_name}. Skipping this profile.")
                continue
            profile_value = int(profile_value_match.group(1))

            if not os.path.exists(summary_h5_path):
                continue

            model_num = profile_to_model.get(profile_value, np.nan)
            if pd.isna(model_num):
                tqdm.write(f"   ⚠️ No valid model number mapped for profile {profile_value} in {run_dir_name}. Skipping modes from this profile.")
                continue

            mesa_row = mesa_history_df[mesa_history_df['model_number'] == model_num]
            if mesa_row.empty:
                tqdm.write(f"   Warning: No MESA history entry found for model {model_num} in {run_dir_name}. Skipping modes from this profile.")
                continue

            try:
                gyre_table = pygyre.read_output(summary_h5_path)

                if len(gyre_table) == 0:
                    continue

                for mode_idx in range(len(gyre_table)):
                    mode_row_gyre = gyre_table[mode_idx]

                    mode_l = int(mode_row_gyre['l']) if 'l' in mode_row_gyre.dtype.names and not np.isnan(mode_row_gyre['l']) else None
                    mode_npg = int(mode_row_gyre['n_pg']) if 'n_pg' in mode_row_gyre.dtype.names and not np.isnan(mode_row_gyre['n_pg']) else None

                    detail_file_name = None
                    if mode_l is not None and mode_npg is not None:
                        n_str = f"+{mode_npg}" if mode_npg >= 0 else str(mode_npg)
                        detail_file_name = f"detail.l{mode_l}.n{n_str}.TXT"

                    full_detail_file_path = os.path.join(current_profile_path, detail_file_name) if detail_file_name else None

                    integrated_work_HeII = np.nan
                    integrated_work_HHeI = np.nan

                    if full_detail_file_path and os.path.exists(full_detail_file_path):
                        try:
                            gyre_detail_data_df = pd.read_csv(
                                full_detail_file_path,
                                skiprows=6,
                                sep=r'\s+',
                                names=detail_columns,
                                engine='python'
                            )
                            if all(col in gyre_detail_data_df.columns for col in ['x', 'dW_dx', 'Gamma_1']):
                                integrated_work_HeII, integrated_work_HHeI = calculate_integrated_work_in_zones(
                                    gyre_detail_data_df['x'],
                                    gyre_detail_data_df['dW_dx'],
                                    gyre_detail_data_df['Gamma_1']
                                )
                            else:
                                tqdm.write(f"      Missing essential columns (x, dW_dx, Gamma_1) in detail file {detail_file_name}. Cannot calculate integrated work.")
                        except Exception as detail_e:
                            tqdm.write(f"      Error reading detail file {detail_file_name} for integrated work calculation: {detail_e}")

                    new_row = {
                        'mass': mass,
                        'Z': z,
                        'model_number': model_num,
                        'profile_number': profile_value,
                        'log_L': mesa_row['log_L'].iloc[0],
                        'log_Teff': mesa_row['log_Teff'].iloc[0],
                        'star_mass_mesa': mesa_row['star_mass'].iloc[0],
                        'freq_real': mode_row_gyre['freq'].real if 'freq' in mode_row_gyre.dtype.names else np.nan,
                        'freq_imag': mode_row_gyre['freq'].imag if 'freq' in mode_row_gyre.dtype.names else np.nan,
                        'l': mode_l,
                        'n_pg': mode_npg,
                        'eta': mode_row_gyre['eta'] if 'eta' in mode_row_gyre.dtype.names else np.nan,
                        'detail_file_path': full_detail_file_path,
                        'profile_folder_path': current_profile_path, # Ezt a sort majd nem fogjuk használni a plot mentéséhez
                        'center_h1': mesa_row['center_h1'].iloc[0] if 'center_h1' in mesa_row.columns else np.nan,
                        'star_age': mesa_row['star_age'].iloc[0] if 'star_age' in mesa_row.columns else np.nan,
                        'log_g': mesa_row['log_g'].iloc[0] if 'log_g' in mesa_row.columns else np.nan,
                        'integrated_work_HeII_zone': integrated_work_HeII,
                        'integrated_work_HHeI_zone': integrated_work_HHeI
                    }
                    all_gyre_modes.append(new_row)

            except Exception as e:
                tqdm.write(f"Error reading {summary_h5_path} with pygyre: {e}. Skipping this profile's GYRE modes.")
                continue

    if all_gyre_modes:
        final_df = pd.DataFrame(all_gyre_modes)
        final_df_sorted = final_df.sort_values(by=['mass', 'Z', 'model_number', 'l', 'n_pg'])

        output_csv_path = os.path.join(project_base_dir, output_csv_filename)
        final_df_sorted.to_csv(output_csv_path, index=False)
        print(f"\nSuccessfully collected all data: {output_csv_path}")
        print(f"Total {len(final_df_sorted)} modes processed and saved.")
        return final_df_sorted
    else:
        print("\nNo GYRE modes were found or processed after iterating through all runs.")
        return pd.DataFrame()

# --- HRD Plotting Function (No change here, as it saves to a central plot_output_dir) ---
def plot_gyre_hrd(df_modes_filtered):
    if df_modes_filtered.empty:
        print("No data available for plotting after filtering or collection.")
        return

    plt.figure(figsize=(10, 8))
    df_npg_filtered = df_modes_filtered[(df_modes_filtered['n_pg'].notna()) & (np.abs(df_modes_filtered['n_pg']) <= 20)].copy()
    if not df_npg_filtered.empty:
        unique_npg_values = sorted(df_npg_filtered['n_pg'].dropna().astype(int).unique())

        sns.scatterplot(data=df_npg_filtered,
                        x='log_Teff',
                        y='log_L',
                        hue=df_npg_filtered['n_pg'].astype(str),
                        palette='viridis',
                        s=50,
                        alpha=0.7,
                        edgecolor='w',
                        linewidth=0.5)

        plt.xlabel(r'$\log T_{\mathrm{eff}}$')
        plt.ylabel(r'$\log (L/L_{\odot})$')
        plt.title(f'HRD - GYRE Modes by $|n_{{pg}}|$ (up to 20) ({len(df_npg_filtered)} Modes)')
        plt.gca().invert_xaxis()
        plt.grid(True, linestyle='--', alpha=0.6)

        if len(unique_npg_values) < 30:
            plt.legend(title=r'$n_{pg}$ Mode Number', bbox_to_anchor=(1.05, 1), loc='upper left',
                                 labels=[str(int(val)) for val in unique_npg_values])
        else:
            plt.legend(title=r'$n_{pg}$ Mode Number', bbox_to_anchor=(1.05, 1), loc='upper left')

        plt.tight_layout()
        plot_filename_npg = os.path.join(plot_output_dir, "hrd_gyre_modes_by_npg_filtered.png")
        plt.savefig(plot_filename_npg, dpi=300)
        print(f"HRD plot (by $n_{{pg}}$, filtered to $|n_{{pg}}| \le 20$) saved to: {plot_filename_npg}")
    else:
        print("No modes found for plotting HRD by n_pg (after |n_pg| <= 20 filter).")

    df_eta_filtered = df_modes_filtered[df_modes_filtered['eta'].notna()].copy()

    if not df_eta_filtered.empty:
        plt.figure(figsize=(10, 8))
        sns.scatterplot(data=df_eta_filtered,
                        x='log_Teff',
                        y='log_L',
                        hue='eta',
                        palette='Spectral_r',
                        s=50,
                        alpha=0.7,
                        edgecolor='w',
                        linewidth=0.5)

        plt.xlabel(r'$\log T_{\mathrm{eff}}$')
        plt.ylabel(r'$\log (L/L_{\odot})$')
        plt.title(f'HRD - GYRE Modes by $\eta$ (Total Modes: {len(df_eta_filtered)})')
        plt.gca().invert_xaxis()
        plt.grid(True, linestyle='--', alpha=0.6)
        plt.legend(title=r'$\eta$', bbox_to_anchor=(1.05, 1), loc='upper left')
        plt.tight_layout()

        plot_filename_eta = os.path.join(plot_output_dir, "hrd_gyre_modes_by_eta.png")
        plt.savefig(plot_filename_eta, dpi=300)
        print(f"HRD plot (by $\eta$) saved to: {plot_filename_eta}")
    else:
        print("No modes found for plotting HRD by eta.")

    df_unstable_modes = df_modes_filtered[df_modes_filtered['freq_imag'] < 0].copy()

    if not df_unstable_modes.empty:
        plt.figure(figsize=(10, 8))
        sns.scatterplot(data=df_unstable_modes,
                        x='log_Teff',
                        y='log_L',
                        hue='freq_imag',
                        palette='coolwarm',
                        s=50,
                        alpha=0.7,
                        edgecolor='w',
                        linewidth=0.5)

        plt.xlabel(r'$\log T_{\mathrm{eff}}$')
        plt.ylabel(r'$\log (L/L_{\odot})$')
        plt.title(f'HRD - Unstable GYRE Modes (Total Unstable: {len(df_unstable_modes)})')
        plt.gca().invert_xaxis()
        plt.grid(True, linestyle='--', alpha=0.6)
        plt.legend(title=r'$\mathrm{Im}(freq)$', bbox_to_anchor=(1.05, 1), loc='upper left')
        plt.tight_layout()

        plot_filename_imag = os.path.join(plot_output_dir, "hrd_gyre_modes_by_freq_imag.png")
        plt.savefig(plot_filename_imag, dpi=300)
        print(f"HRD plot (by Im(freq)) saved to: {plot_filename_imag}")
    else:
        print("No unstable modes found for plotting.")

    plt.show()


# --- Worker function for parallel plotting ---
def _plot_single_radial_profile(mode_data):
    """
    Worker function to plot a single radial profile.
    This function is designed to be called by a multiprocessing Pool.
    It takes a dictionary 'mode_data' containing all necessary mode properties.
    """
    # Unpack the dictionary
    mass = mode_data['mass']
    Z = mode_data['Z']
    model_number = mode_data['model_number']
    l = mode_data['l']
    n_pg = mode_data['n_pg']
    eta = mode_data['eta']
    detail_file_path = mode_data['detail_file_path']
    # profile_folder_path = mode_data['profile_folder_path'] # Ezt már nem használjuk a mentési útvonalhoz

    if not detail_file_path or not os.path.exists(detail_file_path):
        print(f"   Skipping radial plot for mode l={l}, n_pg={n_pg} (Model {model_number}): Detail file not found at {detail_file_path}")
        return

    try:
        gyre_detail_data_df = pd.read_csv(
            detail_file_path,
            skiprows=6,
            sep=r'\s+',
            names=detail_columns, # Ensure detail_columns is accessible or passed if needed
            engine='python'
        )

        x = gyre_detail_data_df['x']
        xi_r_real = gyre_detail_data_df['Re(xi_r)']
        xi_r_imag = gyre_detail_data_df['Im(xi_r)']
        dw_dx = gyre_detail_data_df['dW_dx']
        gamma_1 = gyre_detail_data_df['Gamma_1']
        kap_t = gyre_detail_data_df['kap_T']
        temp = gyre_detail_data_df['T']

        xi_r_abs = np.sqrt(xi_r_real**2 + xi_r_imag**2)
        log10_xi_r_abs = np.log10(xi_r_abs + 1e-30)

        # Ionization zone plotting logic
        gamma_1_min_indices = []
        if len(gamma_1) > 2 and not np.all(np.isnan(gamma_1)):
            # Smooth Gamma_1 for finding dips, using the same window as in calculate_integrated_work_in_zones
            gamma_1_smoothed_for_plot = pd.Series(gamma_1).rolling(window=7, center=True, min_periods=1).mean().values
            
            # Find peaks in the negative smoothed Gamma_1 (dips), using the same prominence and distance
            peaks_for_plot, properties_for_plot = find_peaks(
                -gamma_1_smoothed_for_plot, 
                prominence=0.05, # Ensure this matches the value in calculate_integrated_work_in_zones
                distance=len(gamma_1_smoothed_for_plot) // 10 # Ensure this matches
            )
            
            # Sort detected dips by their x-coordinate in descending order (outermost first)
            # This is consistent with how calculate_integrated_work_in_zones identifies zones.
            x_series_for_plot = pd.Series(x) # Ensure x is a Series for .iloc
            significant_dips_plot_info = sorted(
                [(x_series_for_plot.iloc[p], p, properties_for_plot['prominences'][i]) for i, p in enumerate(peaks_for_plot)],
                key=lambda item: item[0], # Sort by x-coordinate
                reverse=True              # Descending order (outermost first)
            )
            
            # Extract the indices of the two most prominent/outermost dips
            if len(significant_dips_plot_info) >= 1:
                # Outermost (H/He I)
                gamma_1_min_indices.append(significant_dips_plot_info[0][1]) 
            if len(significant_dips_plot_info) >= 2:
                # Second outermost (He II)
                gamma_1_min_indices.append(significant_dips_plot_info[1][1])
            
            # Sort the indices for consistent plotting order, if necessary (optional)
            gamma_1_min_indices = sorted(list(set(gamma_1_min_indices))) # Remove duplicates and sort

        fig, axs = plt.subplots(5, 1, figsize=(10, 15), sharex=True)
        fig.suptitle(f'Radial Profiles - M {mass:.1f} Z {Z:.4f} | Model {model_number} | l={l}, n_pg={n_pg} | $\eta$={eta:.2e}', fontsize=12)

        axs[0].plot(x, log10_xi_r_abs, label=r'$\log_{10}(|\xi_r|)$')
        axs[0].set_ylabel(r'$\log_{10}(|\xi_r|)$')
        axs[0].set_title('Absolute Radial Displacement (log scale)')
        axs[0].grid(True, linestyle=':', alpha=0.7)

        axs[2].plot(x, gamma_1, label=r'$\Gamma_1$', color='green')
        axs[2].set_ylabel(r'$\Gamma_1$')
        axs[2].set_title('Adiabatic Exponent ($\Gamma_1$)')
        axs[2].grid(True, linestyle=':', alpha=0.7)

        axs[3].plot(x, kap_t, label=r'$\kappa_T$', color='blue')
        axs[3].set_ylabel(r'$\kappa_T$')
        axs[3].set_title(r'Opacity Derivative ($\kappa_T$)')
        axs[3].grid(True, linestyle=':', alpha=0.7)

        for ax_idx in range(5):
            for min_idx in gamma_1_min_indices:
                ax = axs[ax_idx]
                if min_idx < len(x):
                    # Only add label for the first vertical line to avoid duplicates in legend
                    if ax_idx == 0 and gamma_1_min_indices.index(min_idx) == 0:
                        ax.axvline(x.iloc[min_idx], color='r', linestyle='--', label='Ionization Zone')
                    else:
                        ax.axvline(x.iloc[min_idx], color='r', linestyle='--')

        if gamma_1_min_indices:
            axs[0].legend(loc='best') # Ensure legend appears if ionization zones are marked

        axs[1].plot(x, dw_dx, label=r'$\mathrm{d}W/\mathrm{d}x$', color='orange')
        axs[1].set_ylabel(r'$\mathrm{d}W/\mathrm{d}x$')
        axs[1].set_title('Differential Work')
        axs[1].grid(True, linestyle=':', alpha=0.7)

        ax_temp = axs[2].twinx()
        ax_temp.plot(x, temp, label='Temperature (K)', color='purple', linestyle=':', alpha=0.6)
        ax_temp.set_ylabel('Temperature (K)', color='purple')
        ax_temp.tick_params(axis='y', labelcolor='purple')

        axs[4].plot(x, temp, label='Temperature (K)', color='purple')
        axs[4].set_xlabel('Fractional Radius ($x = r/R$)', fontsize=10)
        axs[4].set_ylabel('Temperature (K)')
        axs[4].set_title('Temperature Profile')
        axs[4].set_yscale('log')
        axs[4].grid(True, linestyle=':', alpha=0.7)

        plt.tight_layout(rect=[0, 0.03, 1, 0.95])

        # ÚJ MENTÉSI ÚTVONAL ÉS FÁJLNÉV FORMÁTUM
        # Létrehozza a gyre_profiles mappát, ha még nem létezik
        os.makedirs(gyre_profiles_plot_dir, exist_ok=True) 
        
        # A fájlnév tartalmazza a tömeget és a Z-t
        plot_filename = os.path.join(
            gyre_profiles_plot_dir, 
            f"M{mass:.1f}_Z{Z:.4f}_l{l}_npg{n_pg}_model{model_number}.png"
        )
        plt.savefig(plot_filename, dpi=300)
        plt.close(fig)

    except Exception as e:
        print(f"   Error plotting radial profiles for mode l={l}, n_pg={n_pg} (Model {model_number}) from {detail_file_path}: {e}")


# --- Radial Profile Plotting Function (now using multiprocessing) ---
def plot_radial_profiles(df_modes_all, max_npg_to_plot=np.inf):
    df_unstable = df_modes_all[df_modes_all['eta'] > 0].copy()

    if df_unstable.empty:
        print("\nNo unstable modes found to generate radial profile plots.")
        return

    if max_npg_to_plot != np.inf:
        df_to_plot = df_unstable[(df_unstable['n_pg'].notna()) & (np.abs(df_unstable['n_pg']) <= max_npg_to_plot)].copy()
    else:
        df_to_plot = df_unstable.copy()

    if df_to_plot.empty:
        print(f"\nNo unstable modes found to plot (possibly none within the |n_pg| filter if applied).")
        return

    print(f"\nGenerating radial profile plots for unstable modes in parallel...")
    print(f"Total of {len(df_to_plot)} modes will be plotted.")

    num_processes = cpu_count() - 1 if cpu_count() > 1 else 1
    print(f"Using {num_processes} processes for plotting.")

    # Convert DataFrame rows to a list of dictionaries for pickling
    # Fontos: a 'profile_folder_path' már nem szükséges, de a 'mass' és 'Z' igen!
    modes_to_process = df_to_plot[[
        'mass', 'Z', 'model_number', 'l', 'n_pg', 'eta',
        'detail_file_path'
    ]].to_dict(orient='records')


    with Pool(processes=num_processes) as pool:
        for _ in tqdm(pool.imap_unordered(_plot_single_radial_profile, modes_to_process), total=len(modes_to_process), desc="Generating radial plots"):
            pass

    print("\nFinished generating radial profile plots.")


# --- Main execution logic ---
if __name__ == '__main__':
    # 1. Collect data and save to CSV
    df_gyre_modes = collect_gyre_mesa_data()

    # 2. Generate HRD plots from the collected data
    if not df_gyre_modes.empty:
        plot_gyre_hrd(df_gyre_modes.copy())

        # 3. Generate Radial Profile plots for unstable modes
        plot_radial_profiles(df_gyre_modes.copy(), max_npg_to_plot=np.inf)
    else:
        print("No GYRE modes available to generate plots.")
