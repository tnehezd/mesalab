import pandas as pd
import os
import re
from tqdm import tqdm
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pygyre # Ensure you have pygyre installed (pip install pygyre)

# --- Configuration ---
# IMPORTANT: Adjust these paths to precisely match your system's setup.

# Base directory of your 'mesa_blue_loop' project.
# This is where your 'gyre_plot.py' script is, and where 'mesalab_results' is located.
project_base_dir = "/home/tnd/mesagrid/mesa_blue_loop"

# This is the base directory where your GYRE output 'run_...' folders are located.
# Each 'run_...' folder is expected to contain 'profileXXXXX' subfolders,
# and inside those 'profileXXXXX' folders, you'll find 'summary.h5'.
gyre_results_base_dir = os.path.join(project_base_dir, "output", "gyre_output")

# This is the base directory where your MESA run LOGS folders are located.
# Each 'run_...' folder here should contain a 'LOGS' subfolder with 'history.data' and 'profiles.index'.
mesa_logs_base_dir = "/home/tnd/mesa-r23.05.1/STRANGE/nad_convos_mid"

# Name of the output CSV file (will contain all collected data).
output_csv_filename = "gyre_modes_with_mesa_params.csv"

# Output directory for plots (within project_base_dir, will be created if it doesn't exist).
plot_output_dir = os.path.join(project_base_dir, "output", "plots")

# Ensure the plot output directory exists
os.makedirs(plot_output_dir, exist_ok=True)

# --- Function to parse MESA run parameters from directory names ---
def parse_mesa_run_name(run_name):
    """
    Parses MESA run parameters (Mass and Z) from the directory name.
    Expected format: 'run_nad_convos_mid_X.XMSUN_zY.YYYY'
    """
    match_mass = re.search(r'(\d+\.?\d*)MSUN', run_name)
    match_z = re.search(r'z(\d+\.?\d*)', run_name)
    mass = float(match_mass.group(1)) if match_mass else np.nan
    z = float(match_z.group(1)) if match_z else np.nan
    return mass, z

# --- Data Collection Function ---
def collect_gyre_mesa_data():
    """
    Collects GYRE mode data and corresponding MESA stellar parameters.
    It iterates through GYRE output directories, reads summary.h5 files,
    and links them to MESA history.data using profiles.index.
    """
    all_gyre_modes = []
    
    print("Starting data collection from GYRE and MESA results...")

    # Get a list of all 'run_...' directories in the GYRE results base directory
    run_directories = [d for d in os.listdir(gyre_results_base_dir) 
                       if os.path.isdir(os.path.join(gyre_results_base_dir, d)) and d.startswith("run")]

    # Iterate through GYRE result directories with a progress bar
    for run_dir_name in tqdm(run_directories, desc="Processing MESA runs"):
        current_gyre_run_path = os.path.join(gyre_results_base_dir, run_dir_name)
        
        # Parse mass and metallicity from the run directory name
        mass, z = parse_mesa_run_name(run_dir_name)
        if pd.isna(mass) or pd.isna(z): # Check for NaN values from parsing
            tqdm.write(f"Warning: Could not parse mass or Z from directory name: {run_dir_name}. Skipping this run.")
            continue

        # Construct path to the corresponding MESA LOGS directory for this run
        corresponding_mesa_logs_path = os.path.join(mesa_logs_base_dir, run_dir_name, "LOGS")
        
        # --- Read profiles.index to map profile numbers to model numbers ---
        profiles_index_path = os.path.join(corresponding_mesa_logs_path, "profiles.index")
        profile_to_model = {}

        if os.path.exists(profiles_index_path):
            try:
                # Skip the header line in profiles.index (often a text description)
                with open(profiles_index_path, 'r') as f:
                    lines = f.readlines()[1:] 
                    for line in lines:
                        parts = line.strip().split()
                        if len(parts) == 3: # Expecting 'model_number', 'priority', 'profile_number'
                            model_num, _, profile_num = map(int, parts)
                            profile_to_model[profile_num] = model_num
            except Exception as e:
                tqdm.write(f"  Error reading profiles.index in {run_dir_name}: {e}. Skipping this run.")
                continue # Skip to next run folder if profiles.index is problematic
        else:
            tqdm.write(f"  profiles.index not found for {run_dir_name} at {profiles_index_path}. Skipping this run.")
            continue

        # --- Read the MESA history.data file for this run (once per run) ---
        history_file_path = os.path.join(corresponding_mesa_logs_path, "history.data")
        mesa_history_df = pd.DataFrame() # Initialize empty DataFrame

        if os.path.exists(history_file_path):
            try:
                # Read header from the 6th line (index 5)
                with open(history_file_path, 'r') as f:
                    lines = f.readlines()
                header_line = lines[5].strip().split() 
                
                # Read using pandas, explicitly setting column names, whitespace delimiter, and Python engine
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

        # --- Iterate through profile directories within the current run_dir_name ---
        # The summary.h5 files are located inside 'profileXXXXX' subfolders.
        profile_folders = [d for d in os.listdir(current_gyre_run_path) 
                           if os.path.isdir(os.path.join(current_gyre_run_path, d)) and d.startswith("profile")]

        if not profile_folders:
            tqdm.write(f"  No 'profileXXXXX' folders found in {current_gyre_run_path}. Skipping this run.")
            continue

        for profile_folder_name in profile_folders:
            current_profile_path = os.path.join(current_gyre_run_path, profile_folder_name)
            summary_h5_path = os.path.join(current_profile_path, "summary.h5") # The specific summary.h5 file

            # Extract profile_value (e.g., 1 from 'profile00001')
            profile_value_match = re.search(r'profile(\d+)', profile_folder_name)
            if not profile_value_match:
                tqdm.write(f"  Warning: Could not extract profile number from folder name: {profile_folder_name}. Skipping this profile.")
                continue
            profile_value = int(profile_value_match.group(1))

            if not os.path.exists(summary_h5_path):
                # tqdm.write(f"  summary.h5 not found in {current_profile_path}. Skipping this profile.") # Uncomment for more verbose output
                continue # Silently skip if no summary.h5 (e.g., if GYRE failed for this profile)

            # Find corresponding model_number from profiles.index
            model_num = profile_to_model.get(profile_value, np.nan)
            if pd.isna(model_num):
                tqdm.write(f"  ⚠️ No valid model number mapped for profile {profile_value} in {run_dir_name}. Skipping modes from this profile.")
                continue
            
            # Find the corresponding MESA history entry using the model_number
            mesa_row = mesa_history_df[mesa_history_df['model_number'] == model_num]
            if mesa_row.empty:
                tqdm.write(f"  Warning: No MESA history entry found for model {model_num} in {run_dir_name}. Skipping modes from this profile.")
                continue
            
            # --- Read GYRE summary.h5 file for this profile ---
            try:
                # pygyre.read_output returns a structured numpy array or similar, containing all modes
                gyre_table = pygyre.read_output(summary_h5_path)
                
                if len(gyre_table) == 0:
                    continue # Skip if the summary file has no modes

                # Append MESA parameters to each GYRE mode row found in summary.h5
                for mode_idx in range(len(gyre_table)):
                    mode_row_gyre = gyre_table[mode_idx]
                    
                    new_row = {
                        'mass': mass,
                        'Z': z,
                        'model_number': model_num,
                        'profile_number': profile_value, # Store the profile number too
                        'log_L': mesa_row['log_L'].iloc[0],
                        'log_Teff': mesa_row['log_Teff'].iloc[0],
                        'star_mass_mesa': mesa_row['star_mass'].iloc[0], # Mass from MESA history
                        # GYRE mode properties (now checking for n_p, n_g, eta)
                        'freq_real': mode_row_gyre['freq'].real if 'freq' in mode_row_gyre.dtype.names else np.nan,
                        'freq_imag': mode_row_gyre['freq'].imag if 'freq' in mode_row_gyre.dtype.names else np.nan,
                        'l': mode_row_gyre['l'] if 'l' in mode_row_gyre.dtype.names else np.nan,
                        'n_pg': mode_row_gyre['n_pg'] if 'n_pg' in mode_row_gyre.dtype.names else np.nan,
                        'n_p': mode_row_gyre['n_p'] if 'n_p' in mode_row_gyre.dtype.names else np.nan, # Added n_p (still read if available)
                        'n_g': mode_row_gyre['n_g'] if 'n_g' in mode_row_gyre.dtype.names else np.nan, # Added n_g (still read if available)
                        'eta': mode_row_gyre['eta'] if 'eta' in mode_row_gyre.dtype.names else np.nan, # Added eta
                        # Add other history columns you might want, e.g., 'center_h1', 'star_age', 'log_g'
                        'center_h1': mesa_row['center_h1'].iloc[0] if 'center_h1' in mesa_row.columns else np.nan,
                        'star_age': mesa_row['star_age'].iloc[0] if 'star_age' in mesa_row.columns else np.nan,
                        'log_g': mesa_row['log_g'].iloc[0] if 'log_g' in mesa_row.columns else np.nan
                    }
                    all_gyre_modes.append(new_row)

            except Exception as e:
                tqdm.write(f"Error reading {summary_h5_path} with pygyre: {e}. Skipping this profile's GYRE modes.")
                continue
    
    # --- Finalize and save data ---
    if all_gyre_modes:
        final_df = pd.DataFrame(all_gyre_modes)
        # Sort by mass, Z, model for consistency
        final_df_sorted = final_df.sort_values(by=['mass', 'Z', 'model_number'])
        
        output_csv_path = os.path.join(project_base_dir, output_csv_filename)
        final_df_sorted.to_csv(output_csv_path, index=False)
        print(f"\nSuccessfully collected all data: {output_csv_path}")
        print(f"Total {len(final_df_sorted)} modes processed and saved.")
        return final_df_sorted
    else:
        print("\nNo GYRE modes were found or processed after iterating through all runs.")
        return pd.DataFrame() # Return an empty DataFrame if no data

# --- HRD Plotting Function ---
def plot_gyre_hrd(df_modes_filtered):
    """
    Generates Hertzsprung-Russell Diagram (HRD) plots of GYRE modes.
    Plots colored by 'n_pg' (mode order, filtered to abs(n_pg) <= 20) and 'eta'.
    """
    if df_modes_filtered.empty:
        print("No data available for plotting after filtering or collection.")
        return

    # --- Plot 1: HRD colored by n_pg (filtered to abs(n_pg) <= 20) ---
    plt.figure(figsize=(10, 8))

    # Filter for abs(n_pg) <= 20 (and n_pg not NaN)
    df_npg_filtered = df_modes_filtered[(df_modes_filtered['n_pg'].notna()) & (np.abs(df_modes_filtered['n_pg']) <= 20)].copy()

    if not df_npg_filtered.empty:
        # Convert n_pg to integer and then string for categorical hue mapping
        # Only include unique integer values in the legend
        unique_npg_values = sorted(df_npg_filtered['n_pg'].dropna().astype(int).unique())
        
        sns.scatterplot(data=df_npg_filtered,
                        x='log_Teff',
                        y='log_L',
                        hue=df_npg_filtered['n_pg'].astype(str), # Convert to string for discrete colors
                        palette='viridis', # Colormap for n_pg
                        s=50,
                        alpha=0.7,
                        edgecolor='w',
                        linewidth=0.5)

        plt.xlabel(r'$\log T_{\mathrm{eff}}$')
        plt.ylabel(r'$\log (L/L_{\odot})$')
        plt.title(f'Hertzsprung-Russell Diagram - GYRE Modes by $|n_{{pg}}|$ (up to 20) ({len(df_npg_filtered)} Modes)')
        plt.gca().invert_xaxis() # Invert x-axis for traditional HRD (Teff increases to the left)
        plt.grid(True, linestyle='--', alpha=0.6)
        
        # Manually create legend if unique_npg_values are not too many
        if len(unique_npg_values) < 30: # Limit legend entries for readability
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


    # --- Plot 2: HRD colored by eta (eigenvalue imaginary part) ---
    # Only plot modes where 'eta' is not NaN
    df_eta_filtered = df_modes_filtered[df_modes_filtered['eta'].notna()].copy()

    if not df_eta_filtered.empty:
        plt.figure(figsize=(10, 8))
        sns.scatterplot(data=df_eta_filtered,
                        x='log_Teff',
                        y='log_L',
                        hue='eta', # Color by eta
                        palette='Spectral_r', # A good diverging colormap, _r reverses it
                        s=50,
                        alpha=0.7,
                        edgecolor='w',
                        linewidth=0.5)

        plt.xlabel(r'$\log T_{\mathrm{eff}}$')
        plt.ylabel(r'$\log (L/L_{\odot})$')
        plt.title(f'Hertzsprung-Russell Diagram - GYRE Modes by $\eta$ (Total Modes: {len(df_eta_filtered)})')
        plt.gca().invert_xaxis()
        plt.grid(True, linestyle='--', alpha=0.6)
        plt.legend(title=r'$\eta$', bbox_to_anchor=(1.05, 1), loc='upper left')
        plt.tight_layout()

        plot_filename_eta = os.path.join(plot_output_dir, "hrd_gyre_modes_by_eta.png")
        plt.savefig(plot_filename_eta, dpi=300)
        print(f"HRD plot (by $\eta$) saved to: {plot_filename_eta}")
    else:
        print("No modes found for plotting HRD by eta.")


    # --- Plot 3: HRD colored by freq_imag (for unstable modes) ---
    df_unstable_modes = df_modes_filtered[df_modes_filtered['freq_imag'] > 0].copy()
    
    if not df_unstable_modes.empty:
        plt.figure(figsize=(10, 8))
        sns.scatterplot(data=df_unstable_modes,
                        x='log_Teff',
                        y='log_L',
                        hue='freq_imag', # Color by imaginary frequency
                        palette='coolwarm', # Another colormap for positive/negative values
                        s=50,
                        alpha=0.7,
                        edgecolor='w',
                        linewidth=0.5)

        plt.xlabel(r'$\log T_{\mathrm{eff}}$')
        plt.ylabel(r'$\log (L/L_{\odot})$')
        plt.title(f'Hertzsprung-Russell Diagram - Unstable GYRE Modes (Total Unstable: {len(df_unstable_modes)})')
        plt.gca().invert_xaxis()
        plt.grid(True, linestyle='--', alpha=0.6)
        plt.legend(title=r'$\mathrm{Im}(\freq)$', bbox_to_anchor=(1.05, 1), loc='upper left')
        plt.tight_layout()

        plot_filename_imag = os.path.join(plot_output_dir, "hrd_gyre_modes_by_freq_imag.png")
        plt.savefig(plot_filename_imag, dpi=300)
        print(f"HRD plot (by Im(freq)) saved to: {plot_filename_imag}") 
    else:
        print("No unstable modes found for plotting.")

    plt.show() # Display the plots (this will pause execution until plots are closed)

# --- Main execution logic ---
if __name__ == '__main__':
    # 1. Collect data and save to CSV
    df_gyre_modes = collect_gyre_mesa_data()

    # 2. Generate plots from the collected data
    if not df_gyre_modes.empty:
        df_modes_to_plot = df_gyre_modes.copy()
        
        plot_gyre_hrd(df_modes_to_plot)
    else:
        print("No GYRE modes available to generate plots.")
