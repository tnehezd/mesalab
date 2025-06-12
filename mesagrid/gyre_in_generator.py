# mesagrid/gyre_input_generator.py

import os
import argparse
import logging
import re
import pandas as pd
from tqdm import tqdm # Assuming tqdm is installed and wanted for progress bars

# Import reusable components from your mesagrid package
from mesagrid.data_reader import scan_mesa_runs
from mesagrid.inlist_parser import get_mesa_params_from_inlist

# Configure logging for this specific tool
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
# You can set the level to DEBUG for more verbose output during development:
# logging.getLogger().setLevel(logging.DEBUG)


def find_nearest_profiles(profiles_index_path: str, target_model_number: int):
    """
    Reads a profiles.index file and finds the profile numbers
    corresponding to the model numbers immediately smaller and larger
    than the target_model_number.

    Args:
        profiles_index_path (str): Path to the profiles.index file.
        target_model_number (int): The model number to find profiles for.

    Returns:
        tuple: (profile_number_smaller, profile_number_larger) if both found,
               otherwise (None, None).
    """
    model_numbers = []
    profile_numbers = []

    if not os.path.exists(profiles_index_path):
        logging.warning(f"profiles.index not found at: {profiles_index_path}. Cannot find nearest profiles.")
        return None, None

    try:
        # Assuming profiles.index is space-delimited with header line (skipped)
        # Using pandas is more robust for CSV-like files, but sticking to original parsing for now.
        with open(profiles_index_path, 'r') as f:
            # Skip first header line if it contains non-numeric data
            # You might need to adjust skiprows if your file format differs
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
                    # model_number is parts[0], profile_number is parts[2]
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

    smaller_idx = None
    larger_idx = None

    # Find the index of the model number just smaller than target
    for i in range(len(model_numbers) - 1, -1, -1): # Iterate backwards
        if model_numbers[i] <= target_model_number:
            smaller_idx = i
            break
    
    # Find the index of the model number just larger than target
    for i in range(len(model_numbers)): # Iterate forwards
        if model_numbers[i] >= target_model_number:
            larger_idx = i
            break

    # If the target_model_number is outside the range of profiles.index,
    # or if we only find one side, return None
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


def generate_gyre_in_files(output_base_dir: str, run_sub_dir: str, mass: float, z: float,
                           min_profile_number: int, max_profile_number: int, model_name: str):
    """
    Generates gyre.in files for a range of profile numbers within a specific MESA run.
    Each gyre.in file is tailored to point to its corresponding MESA profile.

    Args:
        output_base_dir (str): The root directory where output for this run will be saved.
        run_sub_dir (str): The specific MESA run directory (e.g., 'run_M2.0_Z0.01').
        mass (float): Initial mass of the star for file naming.
        z (float): Metallicity of the star for file naming.
        min_profile_number (int): The starting profile number for gyre.in generation.
        max_profile_number (int): The ending profile number for gyre.in generation.
        model_name (str): The overall model name (e.g., 'nad_convos') for output structuring.
    """
    # Create output directory for this specific run's gyre.in files
    # The structure could be: output_base_dir / model_name / run_dir_name / gyre_in_files/
    # This ensures gyre.in files for different runs don't mix.
    
    # Extract the base run directory name (e.g., 'run_nad_convos_1.5MSUN_z0.0140')
    run_dir_name = os.path.basename(run_sub_dir) 
    
    gyre_in_output_dir = os.path.join(output_base_dir, model_name, run_dir_name, 'gyre_in_files')
    os.makedirs(gyre_in_output_dir, exist_ok=True)
    logging.debug(f"Ensured output directory for gyre.in files: {gyre_in_output_dir}")

    for profile_number in range(min_profile_number, max_profile_number + 1):
        gyre_in_filename = f"gyre_in_profile_{profile_number}.in"
        gyre_in_path = os.path.join(gyre_in_output_dir, gyre_in_filename)

        # Construct the relative path to the profile file for GYRE
        # Assuming GYRE is run from the directory containing gyre.in files,
        # and MESA profiles are in LOGS/ relative to the run_dir.
        # This path might need adjustment based on how GYRE is actually invoked relative to MESA output.
        # Here, it assumes 'gyre_in_files' is at the same level as 'run_dir_name',
        # and inside 'run_dir_name' there is 'LOGS'.
        
        # Relative path from gyre_in_output_dir to the profile file
        # Example: if gyre_in_output_dir is /output/model/run_X/gyre_in_files
        # and profile is in /input/model/run_X/LOGS/profileY.data.GYRE
        # We need to go up 2 levels, then down into the original run's LOGS dir.
        
        # Adjusting this path depends on your GYRE execution strategy.
        # The simplest is to put gyre.in files directly into the MESA run's LOGS dir,
        # or put them one level above in the run_dir itself.
        # Let's assume gyre.in files will be generated *inside* each specific MESA run directory for simplicity,
        # next to the 'LOGS' folder. This makes the path simpler.
        # If the user wants them in a separate output structure, we need to calculate
        # the relative path correctly.
        
        # --- Revision: Let's output gyre.in directly into the MESA run directory
        # (same level as LOGS) for easier relative pathing for GYRE.
        # This means `gyre_in_output_dir` should be the `run_sub_dir` itself.
        gyre_in_path_in_run_dir = os.path.join(run_sub_dir, gyre_in_filename)
        mesa_profile_relative_path = f"LOGS/profile{profile_number}.data.GYRE"


        try:
            with open(gyre_in_path_in_run_dir, 'w') as f:
                f.write(f"""
&constants
/

&model
  model_type = 'EVOL'
  file = '{mesa_profile_relative_path}'
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
  freq_max = 10.0
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
            logging.info(f"→ Generated gyre.in: {gyre_in_path_in_run_dir}")
        except IOError as e:
            logging.error(f"Failed to write gyre.in file {gyre_in_path_in_run_dir}: {e}")
        except Exception as e:
            logging.error(f"An unexpected error occurred while generating gyre.in for profile {profile_number}: {e}")


def main():
    """
    Main function for the mesagrid_gyre_in command-line tool.
    Generates gyre.in files for MESA runs based on specified model number ranges,
    typically identified as blue loop phases.
    """
    parser = argparse.ArgumentParser(description="Generate gyre.in files for MESA stellar evolution grids.")

    parser.add_argument("--input-dir", type=str, required=True,
                        help="Root directory containing MESA run subdirectories (e.g., 'run_M2.0_Z0.01').")
    parser.add_argument("--output-dir", type=str, required=True,
                        help="Output base directory for generated gyre.in files and results.")
    parser.add_argument("--model-name", type=str, default="MESA_GRID_GYRE",
                        help="A name for your model/project (e.g., 'nad_convos'), used for output organization. Default: MESA_GRID_GYRE.")
    parser.add_argument("--blue-loop-model-numbers-csv", type=str, required=True,
                        help="Path to the CSV file containing 'mass', 'Z', 'min_model_number', and 'max_model_number' for blue loop profiles. E.g., 'sorted_mass_Z_min_max_model_nad_convos.csv'.")
    parser.add_argument("--inlist-name", type=str, default="inlist_project",
                        help="Name of the inlist file to identify MESA runs within subdirectories (default: inlist_project).")
    
    args = parser.parse_args()

    input_dir = args.input_dir
    output_base_dir = args.output_dir
    model_name = args.model_name
    blue_loop_csv_path = args.blue_loop_model_numbers_csv
    inlist_name = args.inlist_name

    if not os.path.exists(blue_loop_csv_path):
        logging.error(f"Blue loop model numbers CSV not found: {blue_loop_csv_path}. Exiting.")
        sys.exit(1)

    try:
        df_model_ranges = pd.read_csv(blue_loop_csv_path)
        if df_model_ranges.empty:
            logging.warning(f"Blue loop model numbers CSV is empty: {blue_loop_csv_path}. No profiles to process.")
            return
        required_cols = ['mass', 'Z', 'min_model_number', 'max_model_number']
        if not all(col in df_model_ranges.columns for col in required_cols):
            logging.error(f"Blue loop CSV must contain columns: {required_cols}. Found: {df_model_ranges.columns.tolist()}. Exiting.")
            sys.exit(1)

    except Exception as e:
        logging.error(f"Error reading blue loop model numbers CSV {blue_loop_csv_path}: {e}. Exiting.")
        sys.exit(1)

    logging.info(f"Starting GYRE input file generation for MESA runs in '{input_dir}'.")
    logging.info(f"Using blue loop model ranges from: '{blue_loop_csv_path}'.")

    # Use the existing scan_mesa_runs from mesagrid.data_reader
    all_mesa_run_dirs = scan_mesa_runs(input_dir, inlist_name)

    if not all_mesa_run_dirs:
        logging.warning(f"No MESA run directories found in '{input_dir}' containing '{inlist_name}'.")
        return

    # Prepare a DataFrame to store results (profile numbers found)
    output_records = []

    for run_dir in tqdm(all_mesa_run_dirs, desc="Processing MESA runs for GYRE inputs"):
        mass, z_val = get_mesa_params_from_inlist(os.path.join(run_dir, inlist_name))
        
        if mass is None or z_val is None:
            logging.warning(f"Could not extract mass/Z from inlist in '{run_dir}'. Skipping.")
            continue

        # Find the corresponding row in the blue loop model ranges DataFrame
        # Use a small tolerance for Z comparison due to float precision
        matching_rows = df_model_ranges[
            (df_model_ranges['mass'].round(5) == round(mass, 5)) & # Compare rounded mass
            (df_model_ranges['Z'].round(7) == round(z_val, 7))     # Compare rounded Z
        ]

        if matching_rows.empty:
            logging.info(f"No blue loop model range found in CSV for M={mass:.1f}, Z={z_val:.4f}. Skipping this run.")
            output_records.append({'mass': mass, 'Z': z_val, 'min_model_number_csv': None, 'max_model_number_csv': None, 'min_profile_generated': None, 'max_profile_generated': None, 'gyre_in_dir': None, 'status': 'NoBLRange'})
            continue

        if len(matching_rows) > 1:
            logging.warning(f"Multiple blue loop model ranges found for M={mass:.1f}, Z={z_val:.4f}. Using the first one.")
        
        row_data = matching_rows.iloc[0]
        min_model_number_bl = int(row_data['min_model_number'])
        max_model_number_bl = int(row_data['max_model_number'])

        logs_path = os.path.join(run_dir, 'LOGS')
        profiles_index_path = os.path.join(logs_path, 'profiles.index')

        if not os.path.exists(profiles_index_path):
            logging.warning(f"profiles.index not found for M={mass:.1f}, Z={z_val:.4f} in '{logs_path}'. Cannot generate GYRE inputs.")
            output_records.append({'mass': mass, 'Z': z_val, 'min_model_number_csv': min_model_number_bl, 'max_model_number_csv': max_model_number_bl, 'min_profile_generated': None, 'max_profile_generated': None, 'gyre_in_dir': None, 'status': 'NoProfilesIndex'})
            continue

        logging.debug(f"Searching profiles for M={mass:.1f}, Z={z_val:.4f} within model range [{min_model_number_bl}, {max_model_number_bl}].")
        
        # Find profile numbers corresponding to the blue loop model range
        # Note: find_nearest_profiles returns (profile_smaller, profile_larger) for a SINGLE model number.
        # We need to find *all* profiles between min_model_number_bl and max_model_number_bl.
        
        # A more robust way to get all profiles in range:
        all_profiles_df = None
        try:
            all_profiles_df = pd.read_csv(profiles_index_path, delim_whitespace=True, skiprows=1, header=None,
                                          names=['model_number', 'priority', 'profile_number'])
        except Exception as e:
            logging.error(f"Failed to read profiles.index for M={mass:.1f}, Z={z_val:.4f}: {e}")
            output_records.append({'mass': mass, 'Z': z_val, 'min_model_number_csv': min_model_number_bl, 'max_model_number_csv': max_model_number_bl, 'min_profile_generated': None, 'max_profile_generated': None, 'gyre_in_dir': None, 'status': 'ProfilesIndexReadError'})
            continue

        if all_profiles_df is None or all_profiles_df.empty:
            logging.warning(f"No profiles found in {profiles_index_path} for M={mass:.1f}, Z={z_val:.4f}.")
            output_records.append({'mass': mass, 'Z': z_val, 'min_model_number_csv': min_model_number_bl, 'max_model_number_csv': max_model_number_bl, 'min_profile_generated': None, 'max_profile_generated': None, 'gyre_in_dir': None, 'status': 'NoProfilesFound'})
            continue

        # Filter profiles based on the blue loop model number range
        filtered_profiles_df = all_profiles_df[
            (all_profiles_df['model_number'] >= min_model_number_bl) &
            (all_profiles_df['model_number'] <= max_model_number_bl)
        ]

        if filtered_profiles_df.empty:
            logging.info(f"No profiles found in blue loop range [{min_model_number_bl}, {max_model_number_bl}] for M={mass:.1f}, Z={z_val:.4f}. Skipping GYRE input generation.")
            output_records.append({'mass': mass, 'Z': z_val, 'min_model_number_csv': min_model_number_bl, 'max_model_number_csv': max_model_number_bl, 'min_profile_generated': None, 'max_profile_generated': None, 'gyre_in_dir': None, 'status': 'NoProfilesInBLRange'})
            continue

        # Get the min and max profile_number from the filtered range
        min_profile_num_to_generate = filtered_profiles_df['profile_number'].min()
        max_profile_num_to_generate = filtered_profiles_df['profile_number'].max()

        logging.info(f"Generating gyre.in for M={mass:.1f}, Z={z_val:.4f} from profile {min_profile_num_to_generate} to {max_profile_num_to_generate}.")
        
        # Call the generation function
        generate_gyre_in_files(
            output_base_dir, run_dir, mass, z_val,
            min_profile_num_to_generate, max_profile_num_to_generate, model_name
        )
        
        # Record results
        output_records.append({
            'mass': mass,
            'Z': z_val,
            'min_model_number_csv': min_model_number_bl,
            'max_model_number_csv': max_model_number_bl,
            'min_profile_generated': min_profile_num_to_generate,
            'max_profile_generated': max_profile_num_to_generate,
            'gyre_in_dir': os.path.join(output_base_dir, model_name, os.path.basename(run_dir), 'gyre_in_files'),
            'status': 'Success'
        })

    # Save the final summary of generated files
    final_output_df = pd.DataFrame(output_records)
    summary_output_filename = os.path.join(output_base_dir, f"gyre_in_generation_summary_{model_name}.csv")
    final_output_df.to_csv(summary_output_filename, index=False)
    logging.info(f"GYRE input generation summary saved to: {summary_output_filename}")
    logging.info("✅ All GYRE input file generation complete.")


if __name__ == "__main__":
    main()