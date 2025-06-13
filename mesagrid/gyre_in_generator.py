import os
import re
import pandas as pd
import logging
from tqdm import tqdm
import argparse # For type hinting

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


def generate_gyre_in_file(output_base_dir: str, mesa_run_dir_path: str, model_name: str,
                           profile_number: int, mass: float, z: float):
    """
    Generates a gyre.in file for a given MESA profile.

    Args:
        output_base_dir (str): The root directory where output for this run will be saved (e.g., 'mesagrid_output').
        mesa_run_dir_path (str): The full path to the specific MESA run directory (e.g., '/path/to/mesa_output/run_2.00_0.01000').
        model_name (str): The overall model name (e.g., 'nad_convos') for output structuring.
        profile_number (int): The MESA profile number for which to generate the gyre.in file.
        mass (float): Initial mass of the star for file naming and organization.
        z (float): Metallicity of the star for file naming and organization.
    """
    # Create output directory for this specific run's gyre.in files
    # The structure will be: output_base_dir / gyre_model_name / mesa_run_dir_basename / gyre_in_files/
    
    # We need the base name of the MESA run directory, not its full path
    mesa_run_dir_basename = os.path.basename(mesa_run_dir_path) 
    
    gyre_in_output_dir = os.path.join(output_base_dir, mesa_run_dir_basename)
    os.makedirs(gyre_in_output_dir, exist_ok=True)
    logging.debug(f"Ensured output directory for gyre.in files: {gyre_in_output_dir}")

    gyre_in_filename = f"gyre_in_profile_{profile_number}.in"
    gyre_in_path = os.path.join(gyre_in_output_dir, gyre_in_filename)

    # Relative path from where GYRE will be run (gyre_in_output_dir)
    # to the MESA profile file (which is inside mesa_run_dir_path/LOGS/).
    # This requires navigating up from gyre_in_output_dir, then down to mesa_run_dir_path/LOGS/.
    relative_path_to_logs_dir = os.path.relpath(os.path.join(mesa_run_dir_path, 'LOGS'), gyre_in_output_dir)
    mesa_profile_file = os.path.join(relative_path_to_logs_dir, f'profile{profile_number}.data.GYRE')


    try:
        with open(gyre_in_path, 'w') as f:
            f.write(f"""
&constants
/

&model
  model_type = 'EVOL'
  file = '{mesa_profile_file}'
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
                                   - gyre_mesa_base_dir (str): Base directory of MESA runs (e.g., 'mesa_output/nad_convos_grid').
                                                                This is NOT the individual run_dir_path, but the parent.
                                   - gyre_output_dir (str): Base directory for GYRE output files (e.g., 'mesagrid_output/gyre_inputs').
                                   - gyre_model_name (str): Identifier for the MESA model (e.g., 'nad_convos').
                                   - gyre_blue_loop_csv (str, optional): Path to the sorted_mass_Z_min_max.csv.
                                                                        If not provided, this function expects
                                                                        df_model_ranges to be passed directly.
                                   - df_model_ranges (pd.DataFrame, optional): DataFrame containing 'mass', 'Z',
                                                                               'min_model_number', 'max_model_number',
                                                                               AND 'run_dir_path'.
                                                                               Used when data comes from memory.
    """
    gyre_output_root_dir = args.gyre_output_dir # This is the main GYRE output directory, e.g., 'mesagrid_output/gyre_input'
    gyre_model_name = args.gyre_model_name # This is the model identifier, e.g., 'nad_convos'
    blue_loop_csv_path = getattr(args, 'gyre_blue_loop_csv', None)
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
        logging.warning("Model ranges DataFrame is empty. No profiles to process for GYRE input generation.")
        return

    required_cols = ['initial_mass', 'initial_Z', 'min_model_number', 'max_model_number', 'run_dir_path']
    if not all(col in df_model_ranges.columns for col in required_cols):
        logging.error(f"Model ranges DataFrame must contain columns: {required_cols}. Found: {df_model_ranges.columns.tolist()}. Cannot proceed.")
        return

    logging.info(f"Starting GYRE input file generation using model ranges from {len(df_model_ranges)} entries.")

    # Iterate through the DataFrame to generate gyre.in files
    # Use tqdm for a progress bar
    for _, row in tqdm(df_model_ranges.iterrows(), total=len(df_model_ranges), desc="Generating GYRE input files"):
        mass = row['initial_mass']
        Z = float(row['initial_Z']) # Ensure Z is float for formatting
        min_model_number = int(row['min_model_number'])
        max_model_number = int(row['max_model_number'])
        mesa_run_dir_path = row['run_dir_path'] # Directly get the full path to the MESA run directory

        # Check if the MESA run directory actually exists
        if not os.path.exists(mesa_run_dir_path) or not os.path.isdir(mesa_run_dir_path):
            logging.warning(f"MESA run directory not found: {mesa_run_dir_path}. Skipping GYRE input generation for this entry.")
            continue
        
        logs_path = os.path.join(mesa_run_dir_path, 'LOGS')
        profiles_index_path = os.path.join(logs_path, 'profiles.index')

        logging.info(f"→ Checking profiles.index for M={mass}, Z={Z} (Run: {os.path.basename(mesa_run_dir_path)}): {profiles_index_path}")
        if os.path.exists(profiles_index_path):
            logging.debug(f"  --> Searching for min_model_number ({min_model_number}) and max_model_number ({max_model_number})...")
            
            # Find nearest profiles for the min_model_number
            # We are using the 'smaller' profile for the start of the range as per original logic.
            min_profile_info = find_nearest_profiles(profiles_index_path, min_model_number)
            
            # Find nearest profiles for the max_model_number
            # We are using the 'larger' profile for the end of the range as per original logic.
            max_profile_info = find_nearest_profiles(profiles_index_path, max_model_number)

            if min_profile_info and max_profile_info:
                # Use the 'smaller' profile number for the start of the range
                # Use the 'larger' profile number for the end of the range
                actual_min_profile_to_gen = min_profile_info[0] # smaller_profile_min
                actual_max_profile_to_gen = max_profile_info[1] # larger_profile_max

                # Basic validation: ensure min is not greater than max (can happen if range is very small or inverted)
                if actual_min_profile_to_gen > actual_max_profile_to_gen:
                    logging.warning(f"  ⚠ Calculated profile range is inverted for M={mass}, Z={Z} (Min: {actual_min_profile_to_gen}, Max: {actual_max_profile_to_gen}). Adjusting to single profile if possible.")
                    actual_max_profile_to_gen = actual_min_profile_to_gen # Fallback to single profile if range is inverted
                
                logging.info(f"  ✓ Found closest profiles: Min BL model {min_model_number} -> profile {min_profile_info[0]}. Max BL model {max_model_number} -> profile {max_profile_info[1]}.")
                logging.info(f"  --> Generating GYRE files from profile {actual_min_profile_to_gen} to {actual_max_profile_to_gen}.")

                # Check if the required profile data.GYRE files exist within the range
                all_profiles_exist_in_range = True
                for p_num in range(actual_min_profile_to_gen, actual_max_profile_to_gen + 1):
                    profile_data_path = os.path.join(logs_path, f'profile{p_num}.data.GYRE')
                    if not os.path.exists(profile_data_path):
                        logging.warning(f"    Missing profile data file: {profile_data_path}. Cannot generate GYRE input for profile {p_num} in this range.")
                        all_profiles_exist_in_range = False
                        break # Stop checking, skip this run's generation
                
                if all_profiles_exist_in_range:
                    # Generate gyre.in files for the determined range
                    for profile_num in range(actual_min_profile_to_gen, actual_max_profile_to_gen + 1):
                         generate_gyre_in_file(
                             gyre_output_root_dir,
                             mesa_run_dir_path, # Pass the full MESA run directory path
                             gyre_model_name,
                             profile_num,
                             mass,
                             Z
                         )
                else:
                    logging.warning(f"  ✗ Skipping GYRE file generation for M={mass}, Z={Z} (Run: {os.path.basename(mesa_run_dir_path)}) due to missing profile data files in the specified range.")
            else:
                logging.warning(f"  ⚠ No sufficient profile matches found for M={mass}, Z={Z} (Run: {os.path.basename(mesa_run_dir_path)}). Skipping GYRE input generation for this run.")
        else:
            logging.warning(f"  ✗ profiles.index not found for M={mass}, Z={Z} (Run: {os.path.basename(mesa_run_dir_path)}) at: {profiles_index_path}. Skipping GYRE input generation for this run.")

    logging.info("GYRE input generation process completed.")

