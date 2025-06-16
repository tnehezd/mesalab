import subprocess
import os
import multiprocessing
import shutil
import glob
import f90nml
import pandas as pd
import h5py
import re

# --- GYRE Execution Function ---
def run_single_gyre_model(
    model_profile_path,
    gyre_inlist_template_path,
    output_dir,
    gyre_executable,
    num_gyre_threads
):
    """
    Runs a single GYRE model for a given MESA profile.
    Dynamically generates the inlist file for the specific run.

    Args:
        model_profile_path (str): Absolute path to the MESA profile file.
        gyre_inlist_template_path (str): Path to the base GYRE inlist template file.
        output_dir (str): Directory where GYRE output and its inlist will be saved for this run.
        gyre_executable (str): Absolute path to the GYRE executable.
        num_gyre_threads (int): Number of OpenMP threads GYRE should use for this single run.
    
    Raises:
        subprocess.CalledProcessError: If GYRE execution fails.
        Exception: For other unexpected errors during the run.
    """
    os.makedirs(output_dir, exist_ok=True)
    
    # 1. Generate the inlist file for the current run
    # Read the template inlist
    nml = f90nml.read(gyre_inlist_template_path)

    # Set the MESA profile path in the inlist
    nml['model']['file'] = os.path.abspath(model_profile_path)

    # Output file names (summary_file, detail_template) are now read directly from gyre_inlist_template_path.
    # This allows full control over output naming and subdirectories from the gyre.in template itself.

    # Generate a unique inlist filename for this run
    profile_base_name = os.path.basename(model_profile_path).replace('.data.GYRE', '')
    run_inlist_path = os.path.join(output_dir, f'gyre_inlist_{profile_base_name}.in')
    nml.write(run_inlist_path, force=True) # Add force=True to overwrite existing files
    
    # 2. Set OpenMP threads
    # This sets the number of cores GYRE will use for *one* run
    os.environ['OMP_NUM_THREADS'] = str(num_gyre_threads)

    # 3. Assemble and run the GYRE command
    # Use os.path.basename(run_inlist_path) because cwd is set to output_dir
    command = [gyre_executable, os.path.basename(run_inlist_path)]

    print(f"**[{os.path.basename(output_dir)}]** Attempting to run GYRE with command: `{gyre_executable} {os.path.basename(run_inlist_path)}` (OMP_NUM_THREADS={num_gyre_threads})")

    try:
        # Important: cwd=output_dir, so GYRE writes the generated inlist and outputs there
        result = subprocess.run(command, capture_output=True, text=True, check=True, cwd=output_dir)

        print(f"**[{os.path.basename(output_dir)}] GYRE run SUCCESSFUL**!")
        print("--- Standard Output (stdout) ---")
        print(result.stdout)
        if result.stderr:
            print("--- Standard Error (stderr) ---")
            print(result.stderr)
        print("--- Run finished ---")

    except subprocess.CalledProcessError as e:
        print(f"**[{os.path.basename(output_dir)}] ERROR: GYRE run FAILED**, exit code: **{e.returncode}**!")
        print("--- Standard Output (stdout) ---")
        print(e.stdout)
        print("--- Standard Error (stderr) ---")
        print(e.stderr)
        print("--- Error during run ---")
        raise # Re-raise the exception for main() to catch
    except Exception as e:
        print(f"**[{os.path.basename(output_dir)}] UNEXPECTED ERROR occurred during GYRE run:** {e}")
        print("--- Error during run ---")
        raise # Re-raise the exception


# --- Main mesalab Controller Script ---

def main():
    config_file = 'gyre_config.in'

    if not os.path.exists(config_file):
        raise FileNotFoundError(f"Configuration file '{config_file}' not found. Please create it.")

    print(f"**Reading configuration from '{config_file}'...**")
    config = f90nml.read(config_file)

    # Accessing settings from the config file
    setup_cfg = config['setup']
    run_cfg = config['run_control']
    gyre_cfg = config['gyre_options']

    # --- Setup Paths ---
    mesa_dir = setup_cfg['mesa_dir'] # Base MESA installation directory
    mesasdk_root = setup_cfg['mesasdk_root']
    gyre_dir = setup_cfg['gyre_dir'] # Base GYRE installation directory (or containing GYRE executable)
    output_base_dir = setup_cfg['output_base_dir'] # Root directory for all mesalab outputs

    # Ensure output base directory exists
    os.makedirs(output_base_dir, exist_ok=True)
    print(f"**Output will be saved in: '{os.path.abspath(output_base_dir)}'**")

    # Determine GYRE executable path
    gyre_executable = os.path.join(gyre_dir, 'bin', 'gyre')
    
    # Fallback if gyre_dir already points directly to the binary folder
    if not (os.path.exists(gyre_executable) and os.path.isfile(gyre_executable) and os.access(gyre_executable, os.X_OK)):
        gyre_executable_fallback = os.path.join(gyre_dir, 'gyre')
        if (os.path.exists(gyre_executable_fallback) and os.path.isfile(gyre_executable_fallback) and os.access(gyre_executable_fallback, os.X_OK)):
            gyre_executable = gyre_executable_fallback
        else:
            raise FileNotFoundError(f"GYRE executable not found or not executable at '{gyre_executable}' or '{gyre_executable_fallback}'. "
                                    f"Please check your 'gyre_dir' in '{config_file}'.")
    print(f"**GYRE executable found at: '{gyre_executable}'**")


    # --- Run MESA (Placeholder) ---
    if run_cfg['run_mesa']:
        print("\n--- MESA Run (Not yet implemented) ---")
        pass

    # --- Run GYRE ---
    if run_cfg['run_gyre']:
        print("\n--- GYRE Run Starting ---")

        # This mesa_profile_source_dir is for the 'ALL_PROFILES' mode, usually mesa_dir/LOGS
        global_mesa_logs_dir = os.path.join(mesa_dir, gyre_cfg['mesa_profile_base_dir'])
        
        tasks = [] # List to store all GYRE tasks (profile_path, inlist_template, output_dir, executable, threads)
        
        if gyre_cfg['run_mode'].upper() == 'ALL_PROFILES':
            # This mode will process all profile*.data.GYRE files found under the configured global MESA LOGS directory.
            # WARNING: If different MESA runs produce profiles with overlapping numbers in this single LOGS dir,
            # their GYRE outputs might overwrite each other's results in the 'all_profiles_run_...' directories.
            # Using FILTERED_PROFILES is generally safer for grid-based studies.
            
            if not os.path.exists(global_mesa_logs_dir):
                raise FileNotFoundError(f"MESA profile base directory for 'ALL_PROFILES' mode not found: '{global_mesa_logs_dir}'. "
                                        f"Please check 'mesa_dir' and 'mesa_profile_base_dir' in '{config_file}'.")

            all_profile_paths = sorted(glob.glob(os.path.join(global_mesa_logs_dir, gyre_cfg['mesa_profile_pattern'])))
            print(f"**Run mode: ALL_PROFILES.** Found {len(all_profile_paths)} MESA profile(s) to process based on pattern '{gyre_cfg['mesa_profile_pattern']}'.")
            
            if not all_profile_paths:
                print(f"**WARNING:** No MESA profile files found to process. Skipping GYRE runs.")
            else:
                for profile_path in all_profile_paths:
                    profile_id_from_filename = os.path.basename(profile_path).replace('.data.GYRE', '') # e.g., 'profile00010'
                    
                    # Output directory for ALL_PROFILES mode, less specific but avoids direct overwrites by number
                    run_output_dir = os.path.join(output_base_dir, f"all_profiles_run_{profile_id_from_filename}") 
                    
                    tasks.append((
                        profile_path,
                        gyre_cfg['gyre_inlist_template'],
                        run_output_dir,
                        gyre_executable,
                        gyre_cfg['num_gyre_threads']
                    ))

        elif gyre_cfg['run_mode'].upper() == 'FILTERED_PROFILES':
            # This mode processes profiles based on a CSV file that specifies model ranges and MESA run directories.
            # This is the preferred mode for managing M-Z grids.
            filtered_csv_path = os.path.join(output_base_dir, gyre_cfg['filtered_profiles_csv']) 
            
            if not os.path.exists(filtered_csv_path):
                 raise FileNotFoundError(f"FILTERED_PROFILES mode selected, but CSV file '{filtered_csv_path}' not found. "
                                         f"Please ensure it exists in your '{output_base_dir}' directory.")

            print(f"**Run mode: FILTERED_PROFILES.** Reading filter criteria from '{filtered_csv_path}'...")
            
            try:
                filter_df = pd.read_csv(filtered_csv_path)
            except Exception as e:
                raise IOError(f"Error reading filtered profiles CSV '{filtered_csv_path}': {e}")

            if filter_df.empty:
                print(f"**WARNING:** Filtered profiles CSV '{filtered_csv_path}' is empty. No profiles to process for GYRE runs.")
                return 

            # Iterate through each row in the filter_df (each row represents a specific MESA run for a Mass/Z combination)
            for index, row in filter_df.iterrows():
                mass = row['initial_mass']
                Z = row['initial_Z']
                min_model = row['min_model_number']
                max_model = row['max_model_number']
                
                # Get the absolute path to the specific MESA run directory from the CSV
                mesa_run_specific_dir = row['run_dir_path'] 

                # Construct the path to the LOGS directory and profiles.index for this specific MESA run
                current_mesa_run_logs_dir = os.path.join(mesa_run_specific_dir, 'LOGS')
                current_profiles_index_path = os.path.join(current_mesa_run_logs_dir, 'profiles.index')

                print(f"\nProcessing M={mass}, Z={Z} from run directory: {os.path.basename(mesa_run_specific_dir)}")
                print(f"  Searching profiles in: {current_mesa_run_logs_dir}")

                # --- Manual parsing of profiles.index for the current MESA run ---
                # This ensures we are reading the correct profiles.index for each M-Z combination
                model_numbers_in_index = []
                profile_numbers_in_index = []

                if not os.path.exists(current_profiles_index_path):
                    print(f"  **WARNING:** profiles.index not found for M={mass}, Z={Z} at: '{current_profiles_index_path}'. Skipping this M-Z combination.")
                    continue 

                try:
                    with open(current_profiles_index_path, 'r') as f:
                        for line in f:
                            line = line.strip()
                            if not line or line.startswith('#'): # Skip empty lines and comments
                                continue
                            
                            parts = line.split()
                            if len(parts) < 2: # Ensure enough columns for model_number and prof_num
                                continue
                            
                            try:
                                # Model number is always the first column (index 0)
                                model_num = int(parts[0])
                                # Profile number is typically the last column (e.g., 'prof_num')
                                # Based on standard MESA profiles.index, this should be correct.
                                # If your file uses the 3rd column, change parts[-1] to parts[2].
                                profile_num = int(parts[-1]) 
                                
                                model_numbers_in_index.append(model_num)
                                profile_numbers_in_index.append(profile_num)
                            except ValueError:
                                # Skip lines that don't contain valid numbers in expected columns
                                continue
                except Exception as e:
                    print(f"  **ERROR:** Error reading profiles.index for M={mass}, Z={Z} at '{current_profiles_index_path}': {e}. Skipping.")
                    continue

                if not model_numbers_in_index:
                    print(f"  **WARNING:** No valid data lines found in '{current_profiles_index_path}' for M={mass}, Z={Z}. Skipping.")
                    continue

                # Create a lookup map from the manually parsed data for this specific run
                model_to_profile_map = dict(zip(model_numbers_in_index, profile_numbers_in_index))

                # Find all model numbers within the specified range for this M-Z combination
                selected_profile_numbers_for_this_run = set()
                
                models_in_range_for_this_run = [
                    model for model in model_numbers_in_index
                    if min_model <= model <= max_model
                ]
                
                for model in models_in_range_for_this_run:
                    if model in model_to_profile_map:
                        selected_profile_numbers_for_this_run.add(model_to_profile_map[model])

                if not selected_profile_numbers_for_this_run:
                    print(f"  **WARNING:** No MESA profiles found for M={mass}, Z={Z} within model range {min_model}-{max_model}. Skipping.")
                    continue

                # --- Create the unique output root directory for this M-Z MESA run ---
                # This will be `output_base_dir/basename_of_mesa_run_dir/`
                # e.g., `mesalab_results/run_nad_convos_mid_2.5MSUN_z0.0015/`
                specific_model_output_root = os.path.join(
                    output_base_dir,
                    os.path.basename(mesa_run_specific_dir) 
                )
                os.makedirs(specific_model_output_root, exist_ok=True) # Ensure this directory exists

                # Prepare GYRE tasks for each selected profile within this M-Z run
                for prof_num in sorted(list(selected_profile_numbers_for_this_run)):
                    expected_profile_name = f'profile{prof_num}.data.GYRE'
                    # The profile file is located in the LOGS directory of the specific MESA run
                    profile_path = os.path.join(current_mesa_run_logs_dir, expected_profile_name) 

                    if os.path.exists(profile_path):
                        # Create the final output directory for this specific GYRE run
                        # e.g., `mesalab_results/run_nad_convos_mid_2.5MSUN_z0.0015/profile00010/`
                        run_output_dir = os.path.join(specific_model_output_root, f'profile{prof_num:05d}') # 5-digit profile number
                        
                        tasks.append((
                            profile_path,
                            gyre_cfg['gyre_inlist_template'],
                            run_output_dir,
                            gyre_executable,
                            gyre_cfg['num_gyre_threads']
                        ))
                    else:
                        print(f"  **WARNING:** Filtered profile '{expected_profile_name}' not found for M={mass}, Z={Z} at '{profile_path}'. Skipping.")
            
            print(f"\nTotal GYRE tasks prepared: {len(tasks)}")

        else:
            raise ValueError(f"Invalid 'run_mode' specified in '{config_file}': {gyre_cfg['run_mode']}. "
                             f"Accepted values are 'ALL_PROFILES' or 'FILTERED_PROFILES'.")

        if not tasks:
            print(f"**WARNING:** No GYRE tasks were prepared. Skipping GYRE runs.")
        else:
            if gyre_cfg['enable_parallel']:
                print(f"**Parallel GYRE execution enabled.** Running {gyre_cfg['max_concurrent_gyre_runs']} job(s) concurrently.")
                with multiprocessing.Pool(processes=gyre_cfg['max_concurrent_gyre_runs']) as pool:
                    pool.starmap(run_single_gyre_model, tasks)
            else:
                print("**Parallel GYRE execution disabled.** Running jobs sequentially.")
                for task in tasks:
                    run_single_gyre_model(*task)
            
            print("\n--- GYRE Run Finished ---")

    # --- Run Analysis (Placeholder) ---
    if run_cfg['run_analysis']:
        print("\n--- Analysis Run (Not yet implemented) ---")
        pass

    # --- Run Plots (Placeholder) ---
    if run_cfg['run_plots']:
        print("\n--- Plotting Run (Not yet implemented) ---")
        pass

    print("\n**mesalab execution complete.**")

if __name__ == "__main__":
    try:
        main()
    except FileNotFoundError as e:
        print(f"\n**[Critical Error]:** {e}")
    except subprocess.CalledProcessError as e:
        print(f"\n**[GYRE Execution Error]:** A GYRE process failed. Check the output above for details.")
    except Exception as e:
        print(f"\n**[Unexpected Error]:** An unexpected error occurred: {e}")
        import traceback
        traceback.print_exc()