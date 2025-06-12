import os
import re
import pandas as pd
import logging
from tqdm import tqdm # For progress bar
import argparse # For type hinting, when this function receives arguments from CLI

# Configure logging for this module
# This setup ensures consistent logging if this module is run standalone for testing,
# but the main CLI's logger will take precedence when called as a subcommand.
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')


def find_nearest_profiles(profiles_index_path: str, target_model_number: int):
    """
    Finds the profile numbers corresponding to the model numbers just smaller and just larger
    than the target_model_number from the profiles.index file.

    Args:
        profiles_index_path (str): Path to the profiles.index file.
        target_model_number (int): The model number to search for.

    Returns:
        tuple: A tuple containing (smaller_profile_number, larger_profile_number) if found,
               otherwise (None, None).
    """
    model_numbers = []
    profile_numbers = []

    if not os.path.exists(profiles_index_path):
        logging.warning(f"profiles.index not found at: {profiles_index_path}. Cannot find nearest profiles.")
        return None, None

    try:
        with open(profiles_index_path, 'r') as f:
            # Skip first header line if it contains non-numeric data
            first_line = f.readline().strip()
            if not re.match(r'^\d', first_line): # If first line starts with non-digit (likely header)
                pass # Already skipped
            else:
                f.seek(0) # Rewind if it was actually data

            for line in f:
                parts = line.strip().split()
                if len(parts) < 3: # Expect at least model_number, priority, profile_number
                    continue
                try:
                    model_number = int(parts[0])
                    profile_number = int(parts[2])
                    model_numbers.append(model_number)
                    profile_numbers.append(profile_number)
                except ValueError:
                    logging.debug(f"Skipping malformed line in {profiles_index_path}: {line.strip()}")
                    continue
    except Exception as e:
        logging.error(f"Error reading profiles.index file {profiles_index_path}: {e}")
        return None, None

    if not model_numbers:
        logging.warning(f"No valid data lines found in {profiles_index_path}.")
        return None, None

    # Find the index of the model number just smaller than target
    smaller_idx = None
    for i in range(len(model_numbers) - 1, -1, -1):
        if model_numbers[i] <= target_model_number:
            smaller_idx = i
            break
    
    # Find the index of the model number just larger than target
    larger_idx = None
    for i in range(len(model_numbers)):
        if model_numbers[i] >= target_model_number:
            larger_idx = i
            break

    # If the target_model_number is outside the range of profiles.index,
    # or if we only find one side, return None or closest single profile
    if smaller_idx is None and larger_idx is None:
        logging.warning(f"No profiles found for target model {target_model_number} in {profiles_index_path}.")
        return None, None
    elif smaller_idx is None: # Target is before first profile
        logging.info(f"Target model {target_model_number} is before first profile. Using first available: {profile_numbers[0]}.")
        return profile_numbers[0], profile_numbers[0] # Return first profile as both
    elif larger_idx is None: # Target is after last profile
        logging.info(f"Target model {target_model_number} is after last profile. Using last available: {profile_numbers[-1]}.")
        return profile_numbers[-1], profile_numbers[-1] # Return last profile as both
    else:
        # If target_model_number itself is a profile, use its profile_number for both.
        if model_numbers[smaller_idx] == target_model_number:
            return profile_numbers[smaller_idx], profile_numbers[smaller_idx]
        if model_numbers[larger_idx] == target_model_number:
            return profile_numbers[larger_idx], profile_numbers[larger_idx]
        
        # Otherwise, return the profiles surrounding the target
        return profile_numbers[smaller_idx], profile_numbers[larger_idx]


def generate_gyre_in_file(output_base_dir: str, run_sub_dir: str, mass: float, z: float,
                           profile_number: int, model_name: str):
    """
    Generates a gyre.in file for a given MESA profile.

    Args:
        output_base_dir (str): The root directory where output for this run will be saved.
        run_sub_dir (str): The specific MESA run directory (e.g., 'run_M2.0_Z0.01').
        mass (float): Initial mass of the star for file naming.
        z (float): Metallicity of the star for file naming.
        profile_number (int): The profile number for which to generate the gyre.in file.
        model_name (str): The overall model name (e.g., 'nad_convos') for output structuring.
    """
    # Create output directory for this specific run's gyre.in files
    # The structure will be: output_base_dir / model_name / run_dir_name / gyre_in_files/
    
    run_dir_name = os.path.basename(run_sub_dir) 
    gyre_in_output_dir = os.path.join(output_base_dir, model_name, run_dir_name, 'gyre_in_files')
    os.makedirs(gyre_in_output_dir, exist_ok=True)
    logging.debug(f"Ensured output directory for gyre.in files: {gyre_in_output_dir}")

    gyre_in_filename = f"gyre_in_profile_{profile_number}.in"
    gyre_in_path = os.path.join(gyre_in_output_dir, gyre_in_filename)

    # Relative path from the gyre_in_output_dir to the MESA profile file.
    # This assumes GYRE will be run from the gyre_in_output_dir,
    # and needs to navigate *up* to the MESA run directory, then *down* into LOGS.
    # Calculate relative path carefully
    relative_path_to_logs = os.path.relpath(os.path.join(run_sub_dir, 'LOGS', f'profile{profile_number}.data.GYRE'), gyre_in_output_dir)


    try:
        with open(gyre_in_path, 'w') as f:
            f.write(f"""
&constants
/

&model
  model_type = 'EVOL'
  file = '{relative_path_to_logs}'
  file_format = 'MESA'
/

&mode
  l = 0
/

&osc
  inner_bound = 'ZERO_R'
  nonadiabatic = .TRUE.
/

&rot
/

&num
  diff_scheme = 'MAGNUS_GL2'
  restrict_roots = .FALSE.
/

&scan
  grid_type = 'INVERSE'
  freq_min = 0.01
  freq_max = 1.0 # Reduced max freq for faster initial scans, can be adjusted
  n_freq = 100
  freq_units = 'CYC_PER_DAY'
/

&grid
  x_i = 0.0015
  w_osc = 20.
  w_exp = 20.
/

&ad_output
/

&nad_output
  summary_file = 'gyre_out/summary_{profile_number}.h5'
  summary_item_list = 'l,n_pg,n_p,n_g,omega,eta'
  detail_template = 'gyre_out/detail_%ID_%N_profile_{profile_number}'
  detail_file_format = 'TXT'
  detail_item_list ='l,n_pg,eta,omega,freq,x,xi_r,xi_h'
  freq_units = 'CYC_PER_DAY'
/
""")
        logging.info(f"→ Generated gyre.in: {gyre_in_path}")
    except IOError as e:
        logging.error(f"Failed to write gyre.in file {gyre_in_path}: {e}")
    except Exception as e:
        logging.error(f"An unexpected error occurred while generating gyre.in for profile {profile_number}: {e}")


def run_gyre_input_generation(args: argparse.Namespace):
    """
    Core logic for generating GYRE input files based on a MESA grid.
    This function will be called by the main CLI.

    Args:
        args (argparse.Namespace): Arguments containing:
                                   - input_dir (str): Top-level directory of MESA runs (e.g., 'mesa_output/nad_convos_grid').
                                   - output_dir (str): Base directory for GYRE output files.
                                   - model_name (str): Identifier for the MESA model (e.g., 'nad_convos').
                                   - blue_loop_csv_path (str, optional): Path to the sorted_mass_Z_min_max.csv.
                                                                        If not provided, this function expects
                                                                        df_model_ranges to be passed directly.
                                   - df_model_ranges (pd.DataFrame, optional): DataFrame containing 'mass', 'Z',
                                                                               'min_model_number', 'max_model_number'.
                                                                               Used when data comes from memory.
    """
    input_dir = args.input_dir # e.g. path to 'mesa_output/nad_convos_grid'
    output_base_dir = args.output_dir # e.g. path to 'gyre_outputs'
    model_name = args.model_name # e.g. 'nad_convos'
    blue_loop_csv_path = getattr(args, 'blue_loop_csv_path', None)
    df_model_ranges = getattr(args, 'df_model_ranges', None) # Data from memory if passed

    if df_model_ranges is None:
        if not blue_loop_csv_path or not os.path.exists(blue_loop_csv_path):
            logging.error(f"Neither DataFrame nor valid blue loop CSV path provided. Cannot proceed with GYRE input generation.")
            return

        try:
            df_model_ranges = pd.read_csv(blue_loop_csv_path)
            logging.info(f"Loaded model ranges from CSV: {blue_loop_csv_path}")
        except Exception as e:
            logging.error(f"Error reading blue loop model numbers CSV {blue_loop_csv_path}: {e}. Exiting.")
            return

    if df_model_ranges.empty:
        logging.warning("Model ranges DataFrame is empty. No profiles to process.")
        return

    required_cols = ['mass', 'Z', 'min_model_number', 'max_model_number']
    if not all(col in df_model_ranges.columns for col in required_cols):
        logging.error(f"Model ranges DataFrame must contain columns: {required_cols}. Found: {df_model_ranges.columns.tolist()}. Cannot proceed.")
        return

    logging.info(f"Starting GYRE input file generation for MESA runs in '{input_dir}'.")

    # This part needs to correctly identify the MESA run directories within input_dir
    # based on the model_name, mass, and Z.
    # The current code in main() assumes a structure like base_dir/run_{model}_{mass}MSUN_z{Z}.
    # We need a function to scan for these specific run directories.
    
    # Placeholder for a function that scans MESA run directories
    # This might come from data_reader or a new utility in the future.
    def _scan_mesa_run_dirs(base_path, model_id, df_ranges):
        found_run_dirs = []
        for _, row in df_ranges.iterrows():
            mass = row['mass']
            Z = float(row['Z']) # Ensure Z is float for formatting
            mass_str = f"{mass:.1f}"
            Z_str = f"{Z:.4f}"
            
            # Construct the expected MESA run directory name
            # This must match the actual naming convention of your MESA output directories
            dir_name = f"run_{model_id}_{mass_str}MSUN_z{Z_str}"
            full_run_path = os.path.join(base_path, dir_name)
            
            if os.path.exists(full_run_path) and os.path.isdir(full_run_path):
                found_run_dirs.append({'path': full_run_path, 'mass': mass, 'Z': Z})
            else:
                logging.warning(f"MESA run directory not found for M={mass}, Z={Z} at expected path: {full_run_path}. Skipping.")
        return found_run_dirs
    
    # Use the placeholder scan function
    all_mesa_run_dirs_info = _scan_mesa_run_dirs(input_dir, model_name, df_model_ranges)

    if not all_mesa_run_dirs_info:
        logging.warning(f"No MESA run directories found in '{input_dir}' matching model name '{model_name}' and specified ranges.")
        return

    # Iterate through the DataFrame to generate gyre.in files
    # Use tqdm for a progress bar
    for _, row in tqdm(df_model_ranges.iterrows(), total=len(df_model_ranges), desc="Generating GYRE input files"):
        mass = row['mass']
        Z = float(row['Z'])
        min_model_number = row['min_model_number']
        max_model_number = row['max_model_number']

        # Find the corresponding run directory path from the scanned list
        current_run_info = next((item for item in all_mesa_run_dirs_info if item['mass'] == mass and item['Z'] == Z), None)
        
        if not current_run_info:
            logging.warning(f"MESA run directory information missing for M={mass}, Z={Z}. Skipping this entry.")
            continue

        run_dir_path = current_run_info['path']
        logs_path = os.path.join(run_dir_path, 'LOGS')
        profiles_index_path = os.path.join(logs_path, 'profiles.index')

        logging.info(f"→ Checking profiles.index for M={mass}, Z={Z}: {profiles_index_path}")
        if os.path.exists(profiles_index_path):
            logging.info(f"  --> Searching for min_model_number ({min_model_number})...")
            profiles_min = find_nearest_profiles(profiles_index_path, min_model_number)
            
            logging.info(f"  --> Searching for max_model_number ({max_model_number})...")
            profiles_max = find_nearest_profiles(profiles_index_path, max_model_number)

            if profiles_min and profiles_max:
                smaller_profile_min, larger_profile_min = profiles_min
                smaller_profile_max, larger_profile_max = profiles_max
                
                # Determine the actual range of profile numbers to generate
                # Use the 'larger' profile for min_model_number and 'smaller' profile for max_model_number
                # for a more inclusive range spanning the BL.
                # However, your original code used smaller_profile_min for min and larger_profile_max for max.
                # Let's stick to your original logic for now:
                # The minimum profile number to start generation is the 'smaller' profile found for min_model_number.
                # The maximum profile number to end generation is the 'larger' profile found for max_model_number.
                actual_min_profile_to_gen = smaller_profile_min
                actual_max_profile_to_gen = larger_profile_max

                logging.info(f"  ✓ Found closest profiles: Min BL model {min_model_number} -> profiles ({smaller_profile_min}, {larger_profile_min}). Max BL model {max_model_number} -> profiles ({smaller_profile_max}, {larger_profile_max}).")
                logging.info(f"  --> Generating GYRE files from profile {actual_min_profile_to_gen} to {actual_max_profile_to_gen}.")

                # Check if the required profile data.GYRE files exist within the range
                all_profiles_exist = True
                for p_num in range(actual_min_profile_to_gen, actual_max_profile_to_gen + 1):
                    profile_data_path = os.path.join(logs_path, f'profile{p_num}.data.GYRE')
                    if not os.path.exists(profile_data_path):
                        logging.warning(f"    Missing profile data file: {profile_data_path}. Cannot generate GYRE input for this profile range.")
                        all_profiles_exist = False
                        break
                
                if all_profiles_exist:
                    # Generate gyre.in files
                    generate_gyre_in_file(output_base_dir, run_dir_path, mass, Z, actual_min_profile_to_gen, model_name)
                    # Loop and generate for each profile number if range is inclusive
                    for profile_num in range(actual_min_profile_to_gen + 1, actual_max_profile_to_gen + 1):
                         generate_gyre_in_file(output_base_dir, run_dir_path, mass, Z, profile_num, model_name)
                else:
                    logging.warning(f"  ✗ Skipping GYRE file generation for M={mass}, Z={Z} due to missing profile data files.")
            else:
                logging.warning(f"  ⚠ No sufficient profile matches found for M={mass}, Z={Z}. Skipping GYRE input generation for this run.")
        else:
            logging.warning(f"  ✗ profiles.index not found for M={mass}, Z={Z} at: {profiles_index_path}. Skipping GYRE input generation for this run.")

    logging.info("GYRE input generation process completed.")

# The if __name__ == "__main__": block is now removed from here,
# as this function will be called by the main CLI script.
# If you still want to run it standalone for testing, you'd add:
# if __name__ == "__main__":
#     parser = argparse.ArgumentParser(description="Generate gyre.in files for MESA stellar evolution grids.")
#     parser.add_argument("--input_dir", type=str, required=True,
#                         help="Top-level directory of MESA runs (e.g., 'mesa_output/nad_convos_grid').")
#     parser.add_argument("--output_dir", type=str, default="gyre_inputs",
#                         help="Base directory for GYRE output files.")
#     parser.add_argument("--model_name", type=str, default="nad_convos",
#                         help="Identifier for the MESA model (e.g., 'nad_convos').")
#     parser.add_argument("--blue_loop_csv_path", type=str, default="sorted_mass_Z_min_max.csv",
#                         help="Path to the sorted_mass_Z_min_max.csv file.")
#     args = parser.parse_args()
#     run_gyre_input_generation(args)
