import os
import re
import pandas as pd
import numpy as np
import pygyre # Make sure you have pygyre installed (pip install pygyre)
from tqdm import tqdm # Import tqdm for progress bar

# --- Configuration (Adjust these paths to your system) ---
# Base directory for the MESA grid project (where this script is located)
project_base_dir = "/home/tnd/mesagrid/mesa_blue_loop"

# This should point to the folder containing your 'run_...' folders that hold GYRE outputs
# E.g., if GYRE outputs are in /home/tnd/mesagrid/mesa_blue_loop/mesalab_results/run_XXX/...
gyre_data_base_folder = os.path.join(project_base_dir, "mesalab_results")

# This should point to the folder containing your 'run_...' folders that hold MESA LOGS
# E.g., if MESA LOGS are in /home/tnd/mesa-r23.05.1/STRANGE/nad_convos_mid/run_XXX/LOGS/...
mesa_logs_base_folder = "/home/tnd/mesa-r23.05.1/STRANGE/nad_convos_mid"

# Output CSV file name
output_csv_filename = "gyre_summary_with_history_data.csv"

# --- End Configuration ---

# Function to extract mass and Z from folder name (unchanged from your code)
def extract_mass_and_Z(folder_name):
    mass_match = re.search(r'(\d+(\.\d+)?)MSUN', folder_name)
    Z_match = re.search(r'z(\d+\.\d+)', folder_name)
    mass = float(mass_match.group(1)) if mass_match else np.nan
    Z = float(Z_match.group(1)) if Z_match else np.nan
    return mass, Z

# Data collection
all_data = []

print("Checking run folders and extracting data...")

# Get list of all 'run_...' folders in the GYRE data base directory
run_folders = [f for f in os.listdir(gyre_data_base_folder) if os.path.isdir(os.path.join(gyre_data_base_folder, f)) and f.startswith("run")]

# Add tqdm for a progress bar
for gyre_run_folder_name in tqdm(run_folders, desc="Processing MESA runs"):
    full_gyre_run_path = os.path.join(gyre_data_base_folder, gyre_run_folder_name)
    
    mass, Z = extract_mass_and_Z(gyre_run_folder_name)

    # Path to the corresponding MESA LOGS folder for this run
    corresponding_mesa_run_path = os.path.join(mesa_logs_base_folder, gyre_run_folder_name)
    
    profiles_index_path = os.path.join(corresponding_mesa_run_path, "LOGS", "profiles.index")
    profile_to_model = {}

    if os.path.exists(profiles_index_path):
        try:
            with open(profiles_index_path, 'r') as f:
                lines = f.readlines()[1:] # Skip the first line (header)
                for line in lines:
                    parts = line.strip().split()
                    if len(parts) == 3:
                        model_num, _, profile_num = map(int, parts)
                        profile_to_model[profile_num] = model_num
        except Exception as e:
            tqdm.write(f"  Error reading profiles.index in {gyre_run_folder_name}: {e}")
            continue # Skip to next run folder if profiles.index is problematic
    else:
        tqdm.write(f"  profiles.index not found for {gyre_run_folder_name}. Skipping.")
        continue

    # Path to the 'gyre_out' folder for this run
    gyre_out_path = os.path.join(full_gyre_run_path, "gyre_out")
    if not os.path.isdir(gyre_out_path):
        tqdm.write(f"  gyre_out folder not found for {gyre_run_folder_name}. Skipping.")
        continue

    # Process each summary_XXX.h5 file in gyre_out
    summary_files = [f for f in os.listdir(gyre_out_path) if f.startswith("summary_") and f.endswith(".h5")]
    if not summary_files:
        tqdm.write(f"  No summary_*.h5 files found in {gyre_out_path}. Skipping.")
        continue

    for fname in summary_files:
        summary_path = os.path.join(gyre_out_path, fname)
        profile_value_match = re.search(r'summary_(\d+)\.h5', fname)
        profile_value = int(profile_value_match.group(1)) if profile_value_match else -1

        try:
            # Read GYRE table using pygyre
            table = pygyre.read_output(summary_path)
            # Ensure 'modes' key is used for the actual mode data in pygyre output if it's structured that way
            # pygyre.read_output directly returns a table (dict-like), so check for 'omega' and 'eta'
            
            # pygyre.read_output returns a structured numpy array or similar,
            # we need to ensure we're getting all modes for this profile, not just the first one.
            # Let's assume 'table' contains all modes for a given profile.
            
            # The previous code only took the first mode's omega and eta.
            # To get all modes from a summary file, we need to iterate over the table.
            if len(table) == 0:
                # tqdm.write(f"  No modes found in {summary_path}. Skipping.")
                continue # Skip if the summary file has no modes

            # History file path for this run
            history_file_path = os.path.join(corresponding_mesa_run_path, "LOGS", "history.data")
            
            if os.path.exists(history_file_path):
                try:
                    # Read history.data and find the relevant row
                    with open(history_file_path, 'r') as f:
                        lines = f.readlines()
                    
                    # Extract header from the 6th line (index 5)
                    header_line = lines[5].strip().split() 
                    # Read using pandas, explicitly setting column names and Python engine for robustness
                    df_history = pd.read_csv(history_file_path, skiprows=6, sep=r'\s+', names=header_line, engine='python') # Added engine='python' and sep='\s+'

                    model_num = profile_to_model.get(profile_value, np.nan)
                    if pd.isna(model_num): # Check for NaN after lookup
                        tqdm.write(f"  ⚠️ No valid model number mapped for profile {profile_value} in {gyre_run_folder_name}. Skipping modes from this profile.")
                        continue

                    match = df_history[df_history['model_number'] == model_num]
                    if not match.empty:
                        match_row = match.iloc[0]
                        
                        # Iterate through each mode found in the GYRE summary file
                        for mode_idx in range(len(table)):
                            mode_row_gyre = table[mode_idx]
                            
                            row_data = {
                                'mass': mass,
                                'Z': Z,
                                'profile': profile_value,
                                'model': model_num,
                                'freq_real': mode_row_gyre['omega'].real if 'omega' in mode_row_gyre.dtype.names else np.nan, # pygyre returns complex 'omega'
                                'freq_imag': mode_row_gyre['omega'].imag if 'omega' in mode_row_gyre.dtype.names else np.nan,
                                'n_p': mode_row_gyre['n_p'] if 'n_p' in mode_row_gyre.dtype.names else np.nan,
                                'n_g': mode_row_gyre['n_g'] if 'n_g' in mode_row_gyre.dtype.names else np.nan,
                                'l': mode_row_gyre['l'] if 'l' in mode_row_gyre.dtype.names else np.nan,
                                'log_L': match_row['log_L'],
                                'log_Teff': match_row['log_Teff'],
                                'center_h1': match_row['center_h1'],
                                'star_age': match_row['star_age'],
                                'log_g': match_row['log_g']
                            }
                            all_data.append(row_data)
                    else:
                        tqdm.write(f"  ⚠️ No matching model number ({model_num}) found in history.data for {gyre_run_folder_name}. Skipping modes from this profile.")

                except Exception as e:
                    tqdm.write(f"  ⚠️ Failed to read history data for model {model_num if 'model_num' in locals() else 'N/A'} in {gyre_run_folder_name}: {e}")
            else:
                tqdm.write(f"  ⚠️ history.data not found for {gyre_run_folder_name}. Skipping modes from this profile.")

        except Exception as e:
            tqdm.write(f"  Error reading {summary_path} with pygyre: {e}")

# Create DataFrame and save
if all_data:
    df = pd.DataFrame(all_data)

    # Sort by mass, Z, model
    df_sorted = df.sort_values(by=['mass', 'Z', 'model'])

    # Save to CSV
    output_csv_path = os.path.join(project_base_dir, output_csv_filename) # Save to project base dir for easy access
    df_sorted.to_csv(output_csv_path, index=False)

    print(f"\n✔ Done. Saved to '{output_csv_path}'")
else:
    print("\nNo data collected. Check paths and input files.")
