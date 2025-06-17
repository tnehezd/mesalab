import subprocess
import os
import multiprocessing
import shutil
import glob
import f90nml # Make sure you have this installed: pip install f90nml
import pandas as pd
import h5py # For potential post-processing of GYRE .h5 outputs
import re
import logging

# --- Configure logging for this module ---
# This setup ensures consistent logging. The main application's logger
# will typically take precedence when imported.
log_file_path = 'gyre_pipeline.log'
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - [GYRE Pipeline] %(levelname)s: %(message)s',
    handlers=[
        logging.FileHandler(log_file_path),
        logging.StreamHandler() # Also log to console
    ]
)

# --- Core GYRE Execution Function ---
def run_single_gyre_model(
    model_profile_path,
    gyre_inlist_template_path,
    output_dir,
    gyre_executable,
    num_gyre_threads
):
    """
    Runs a single GYRE model for a given MESA profile.
    Dynamically generates the inlist file for the specific run based on a template.

    Args:
        model_profile_path (str): Absolute path to the MESA profile file (e.g., profileXXXX.data.GYRE).
        gyre_inlist_template_path (str): Path to the base GYRE inlist template file.
        output_dir (str): Directory where GYRE output and its inlist will be saved for this run.
                          This will typically be structured like:
                          output_base_dir / mesa_run_dir_basename / profileXXXXX/
        gyre_executable (str): Absolute path to the GYRE executable.
        num_gyre_threads (int): Number of OpenMP threads GYRE should use for this single run.
    
    Raises:
        subprocess.CalledProcessError: If GYRE execution fails.
        Exception: For other unexpected errors during the run.
    """
    logging.info(f"Setting up GYRE run for profile: {os.path.basename(model_profile_path)}")
    os.makedirs(output_dir, exist_ok=True)
    
    # 1. Generate the inlist file for the current run
    run_inlist_path = None # Initialize to None
    try:
        # Read the template inlist using f90nml
        nml = f90nml.read(gyre_inlist_template_path)
    except FileNotFoundError:
        logging.error(f"GYRE inlist template not found: {gyre_inlist_template_path}")
        raise
    except f90nml.fortran_namelist.NamelistError as e:
        logging.error(f"Error parsing GYRE inlist template {gyre_inlist_template_path}: {e}")
        raise
    except Exception as e:
        logging.error(f"Error reading GYRE inlist template {gyre_inlist_template_path}: {e}")
        raise

    # Set the MESA profile path in the inlist
    # The 'file' parameter in '&model' block is updated to the absolute path of the profile
    nml['model']['file'] = os.path.abspath(model_profile_path)

    # Ensure the 'gyre_out' subdirectory exists within the specific run's output_dir.
    # This aligns with the 'summary_file = 'gyre_out/summary_...' setting in the template.
    gyre_output_subdir = os.path.join(output_dir, 'gyre_out')
    os.makedirs(gyre_output_subdir, exist_ok=True)
    
    # Generate a unique inlist filename for this run within its specific output_dir
    profile_base_name = os.path.basename(model_profile_path).replace('.data.GYRE', '')
    run_inlist_path = os.path.join(output_dir, f'gyre_inlist_{profile_base_name}.in')
    
    try:
        nml.write(run_inlist_path, force=True) # force=True will overwrite existing files
        logging.info(f"Generated specific GYRE inlist: {run_inlist_path}")
    except Exception as e:
        logging.error(f"Failed to write GYRE inlist file {run_inlist_path}: {e}")
        raise

    # 2. Set OpenMP threads for this specific GYRE process
    # This controls the number of CPU cores a single GYRE instance will utilize.
    original_omp_num_threads = os.environ.get('OMP_NUM_THREADS') # Store original value
    os.environ['OMP_NUM_THREADS'] = str(num_gyre_threads)
    logging.info(f"OMP_NUM_THREADS set to {num_gyre_threads} for this GYRE instance.")

    # 3. Assemble and run the GYRE command
    # Use os.path.basename(run_inlist_path) because cwd is set to output_dir,
    # meaning GYRE will look for the inlist by its name in the current working directory.
    command = [gyre_executable, os.path.basename(run_inlist_path)]

    logging.info(f"**[{os.path.basename(output_dir)}]** Attempting to run GYRE with command: `{gyre_executable} {os.path.basename(run_inlist_path)}`")

    try:
        # Important: cwd=output_dir, so GYRE reads the inlist and writes all its outputs (like .h5 files) there.
        result = subprocess.run(command, capture_output=True, text=True, check=True, cwd=output_dir)

        logging.info(f"**[{os.path.basename(output_dir)}] GYRE run SUCCESSFUL**!")
        logging.debug(f"--- Standard Output (stdout) for {os.path.basename(output_dir)} ---")
        logging.debug(result.stdout)
        if result.stderr:
            logging.debug(f"--- Standard Error (stderr) for {os.path.basename(output_dir)} ---")
            logging.debug(result.stderr)
        logging.info("--- Run finished ---")

    except FileNotFoundError:
        logging.error(f"ERROR: GYRE executable not found at '{gyre_executable}'. Please check the path specified in your config.")
        raise # Re-raise for the main error handler
    except subprocess.CalledProcessError as e:
        logging.error(f"**[{os.path.basename(output_dir)}] ERROR: GYRE run FAILED**, exit code: **{e.returncode}**!")
        logging.error(f"Command executed: {' '.join(e.cmd)}")
        logging.error(f"GYRE stdout:\n{e.stdout}")
        logging.error(f"GYRE stderr:\n{e.stderr}")
        raise # Re-raise the exception for main() to catch
    except Exception as e:
        logging.error(f"**[{os.path.basename(output_dir)}] UNEXPECTED ERROR occurred during GYRE run:** {e}")
        logging.error("--- Error during run ---")
        raise # Re-raise the exception
    finally:
        # Restore OMP_NUM_THREADS to its original value or unset if it wasn't set before
        if original_omp_num_threads is not None:
            os.environ['OMP_NUM_THREADS'] = original_omp_num_threads
            logging.info(f"OMP_NUM_THREADS restored to {original_omp_num_threads}.")
        else:
            if 'OMP_NUM_THREADS' in os.environ:
                del os.environ['OMP_NUM_THREADS']
                logging.info("OMP_NUM_THREADS unset.")


# --- Main GYRE Pipeline Orchestration Function ---
def run_gyre_workflow(config_file: str):
    """
    Orchestrates the GYRE runs based on configuration.
    This function encapsulates the logic previously found in your script's main function.

    Args:
        config_file (str): Path to the GYRE specific configuration file (e.g., 'gyre_config.in').
    """
    if not os.path.exists(config_file):
        raise FileNotFoundError(f"Configuration file '{config_file}' not found. Please ensure it exists.")

    logging.info(f"**Reading GYRE specific configuration from '{config_file}'...**")
    try:
        config = f90nml.read(config_file)
    except FileNotFoundError:
        logging.critical(f"Configuration file '{config_file}' not found.")
        raise
    except f90nml.fortran_namelist.NamelistError as e:
        logging.critical(f"Error parsing configuration file '{config_file}': {e}")
        raise
    except Exception as e:
        logging.critical(f"An unexpected error occurred while reading configuration '{config_file}': {e}")
        raise

    # Accessing settings from the config file
    setup_cfg = config.get('setup', {})
    gyre_cfg = config.get('gyre_options', {})

    # Validate essential configuration parameters
    required_setup_params = ['mesa_dir', 'gyre_dir', 'output_base_dir']
    for param in required_setup_params:
        if param not in setup_cfg:
            raise ValueError(f"Missing required parameter '{param}' in the '[setup]' section of '{config_file}'.")

    required_gyre_params = ['mesa_profile_base_dir', 'mesa_profile_pattern', 'gyre_inlist_template',
                            'run_mode', 'num_gyre_threads', 'enable_parallel', 'max_concurrent_gyre_runs']
    for param in required_gyre_params:
        if param not in gyre_cfg:
            raise ValueError(f"Missing required parameter '{param}' in the '[gyre_options]' section of '{config_file}'.")

    # --- Setup Paths ---
    mesa_dir = setup_cfg['mesa_dir']    
    gyre_dir = setup_cfg['gyre_dir']    
    output_base_dir = setup_cfg['output_base_dir']    

    os.makedirs(output_base_dir, exist_ok=True)
    logging.info(f"**GYRE outputs will be saved in: '{os.path.abspath(output_base_dir)}'**")

    # Determine GYRE executable path
    gyre_executable = os.path.join(gyre_dir, 'bin', 'gyre')
    
    if not (os.path.exists(gyre_executable) and os.path.isfile(gyre_executable) and os.access(gyre_executable, os.X_OK)):
        gyre_executable_fallback = os.path.join(gyre_dir, 'gyre')
        if (os.path.exists(gyre_executable_fallback) and os.path.isfile(gyre_executable_fallback) and os.access(gyre_executable_fallback, os.X_OK)):
            gyre_executable = gyre_executable_fallback
        else:
            raise FileNotFoundError(f"GYRE executable not found or not executable at '{gyre_executable}' or '{gyre_executable_fallback}'. "
                                    f"Please check your 'gyre_dir' in '{config_file}'.")
    logging.info(f"**GYRE executable found at: '{gyre_executable}'**")
    
    # Validate GYRE inlist template path
    gyre_inlist_template_full_path = os.path.abspath(gyre_cfg['gyre_inlist_template'])
    if not os.path.exists(gyre_inlist_template_full_path):
        raise FileNotFoundError(f"GYRE inlist template '{gyre_inlist_template_full_path}' specified in config not found.")
    logging.info(f"**GYRE inlist template: '{gyre_inlist_template_full_path}'**")

    logging.info("\n--- GYRE Run Starting ---")

    global_mesa_logs_dir = os.path.join(mesa_dir, gyre_cfg['mesa_profile_base_dir'])
    
    tasks = []    
    
    run_mode = gyre_cfg['run_mode'].upper()

    if run_mode == 'ALL_PROFILES':
        if not os.path.exists(global_mesa_logs_dir):
            raise FileNotFoundError(f"MESA profile base directory for 'ALL_PROFILES' mode not found: '{global_mesa_logs_dir}'. "
                                    f"Please check 'mesa_dir' and 'mesa_profile_base_dir' in '{config_file}'.")

        all_profile_paths = sorted(glob.glob(os.path.join(global_mesa_logs_dir, gyre_cfg['mesa_profile_pattern'])))
        logging.info(f"**Run mode: ALL_PROFILES.** Found {len(all_profile_paths)} MESA profile(s) to process based on pattern '{gyre_cfg['mesa_profile_pattern']}'.")
        
        if not all_profile_paths:
            logging.warning(f"**WARNING:** No MESA profile files found to process. Skipping GYRE runs.")
        else:
            for profile_path in all_profile_paths:
                profile_id_from_filename = os.path.basename(profile_path).replace('.data.GYRE', '')
                run_output_dir = os.path.join(output_base_dir, f"all_profiles_run_{profile_id_from_filename}")    
                
                tasks.append((
                    profile_path,
                    gyre_inlist_template_full_path, # Use the validated full path
                    run_output_dir,
                    gyre_executable,
                    gyre_cfg['num_gyre_threads']
                ))

    elif run_mode == 'FILTERED_PROFILES':
        if 'filtered_profiles_csv' not in gyre_cfg:
            raise ValueError(f"Missing required parameter 'filtered_profiles_csv' in the '[gyre_options]' section of '{config_file}' for 'FILTERED_PROFILES' mode.")
            
        filtered_csv_path = os.path.join(output_base_dir, gyre_cfg['filtered_profiles_csv'])    
        
        if not os.path.exists(filtered_csv_path):
            raise FileNotFoundError(f"FILTERED_PROFILES mode selected, but CSV file '{filtered_csv_path}' not found. "
                                    f"Please ensure it exists in your '{output_base_dir}' directory and is specified correctly in '{config_file}'.")

        logging.info(f"**Run mode: FILTERED_PROFILES.** Reading filter criteria from '{filtered_csv_path}'...")
        
        try:
            filter_df = pd.read_csv(filtered_csv_path)
        except Exception as e:
            raise IOError(f"Error reading filtered profiles CSV '{filtered_csv_path}': {e}")

        if filter_df.empty:
            logging.warning(f"**WARNING:** Filtered profiles CSV '{filtered_csv_path}' is empty. No profiles to process for GYRE runs.")
            return    

        # Check for required columns in the DataFrame
        required_cols = ['initial_mass', 'initial_Z', 'min_model_number', 'max_model_number', 'mesa_run_directory']        if not all(col in filter_df.columns for col in required_cols):
            raise ValueError(f"Filtered profiles CSV '{filtered_csv_path}' must contain the columns: {', '.join(required_cols)}")

        for index, row in filter_df.iterrows():
            mass = row['initial_mass']
            Z = row['initial_Z']
            min_model = int(row['min_model_number'])
            max_model = int(row['max_model_number'])
            
            mesa_run_specific_dir = row['mesa_run_directory']    

            current_mesa_run_logs_dir = os.path.join(mesa_run_specific_dir, 'LOGS')
            current_profiles_index_path = os.path.join(current_mesa_run_logs_dir, 'profiles.index')

            logging.info(f"\nProcessing M={mass}, Z={Z} from run directory: {os.path.basename(mesa_run_specific_dir)}")
            logging.info(f"Searching profiles in: {current_mesa_run_logs_dir}")

            model_numbers_in_index = []
            profile_numbers_in_index = []

            if not os.path.exists(current_profiles_index_path):
                logging.warning(f"**WARNING:** profiles.index not found for M={mass}, Z={Z} at: '{current_profiles_index_path}'. Skipping this M-Z combination.")
                continue    

            try:
                with open(current_profiles_index_path, 'r') as f:
                    first_line = f.readline().strip()
                    if not re.match(r'^\d', first_line):    
                        # If the first line doesn't start with a digit, assume it's a header and skip it.
                        pass    
                    else:
                        f.seek(0) # If it's not a header, rewind to the beginning.

                    for line in f:
                        line = line.strip()
                        if not line or line.startswith('#'):    
                            continue
                        
                        parts = line.split()
                        if len(parts) < 2:    
                            continue
                        
                        try:
                            model_num = int(parts[0])
                            profile_num = int(parts[-1])    
                            
                            model_numbers_in_index.append(model_num)
                            profile_numbers_in_index.append(profile_num)
                        except ValueError:
                            logging.debug(f"Skipping malformed line in {current_profiles_index_path}: {line.strip()}")
                            continue
            except Exception as e:
                logging.error(f"**ERROR:** Error reading profiles.index for M={mass}, Z={Z} at '{current_profiles_index_path}': {e}. Skipping.")
                continue

            if not model_numbers_in_index:
                logging.warning(f"**WARNING:** No valid data lines found in '{current_profiles_index_path}' for M={mass}, Z={Z}. Skipping.")
                continue

            model_to_profile_map = dict(zip(model_numbers_in_index, profile_numbers_in_index))

            selected_profile_numbers_for_this_run = set()
            
            models_in_range_for_this_run = [
                model for model in model_numbers_in_index
                if min_model <= model <= max_model
            ]
            
            for model in models_in_range_for_this_run:
                if model in model_to_profile_map:
                    selected_profile_numbers_for_this_run.add(model_to_profile_map[model])

            if not selected_profile_numbers_for_this_run:
                logging.warning(f"**WARNING:** No MESA profiles found for M={mass}, Z={Z} within model range {min_model}-{max_model}. Skipping.")
                continue

            specific_model_output_root = os.path.join(
                output_base_dir,
                os.path.basename(mesa_run_specific_dir)    
            )
            os.makedirs(specific_model_output_root, exist_ok=True)    

            for prof_num in sorted(list(selected_profile_numbers_for_this_run)):
                expected_profile_name = f'profile{prof_num}.data.GYRE'
                profile_path = os.path.join(current_mesa_run_logs_dir, expected_profile_name)    

                if os.path.exists(profile_path):
                    run_output_dir = os.path.join(specific_model_output_root, f'profile{prof_num:05d}')
                    
                    tasks.append((
                        profile_path,
                        gyre_inlist_template_full_path, # Use the validated full path
                        run_output_dir,
                        gyre_executable,
                        gyre_cfg['num_gyre_threads']
                    ))
                else:
                    logging.warning(f"**WARNING:** Filtered profile '{expected_profile_name}' not found for M={mass}, Z={Z} at '{profile_path}'. Skipping.")
        
        logging.info(f"\nTotal GYRE tasks prepared: {len(tasks)}")

    else:
        raise ValueError(f"Invalid 'run_mode' specified in '{config_file}': {gyre_cfg['run_mode']}. "
                         f"Accepted values are 'ALL_PROFILES' or 'FILTERED_PROFILES'.")

    if not tasks:
        logging.warning(f"**WARNING:** No GYRE tasks were prepared. Skipping GYRE runs.")
    else:
        if gyre_cfg['enable_parallel']:
            max_concurrent_runs = gyre_cfg['max_concurrent_gyre_runs']
            if not isinstance(max_concurrent_runs, int) or max_concurrent_runs <= 0:
                raise ValueError(f"Invalid 'max_concurrent_gyre_runs' in config: {max_concurrent_runs}. Must be a positive integer.")
            logging.info(f"**Parallel GYRE execution enabled.** Running {max_concurrent_runs} job(s) concurrently.")
            with multiprocessing.Pool(processes=max_concurrent_runs) as pool:
                pool.starmap(run_single_gyre_model, tasks)
        else:
            logging.info("**Parallel GYRE execution disabled.** Running jobs sequentially.")
            for task in tasks:
                run_single_gyre_model(*task)
        
        logging.info("\n--- GYRE Run Finished ---")

    logging.info("\n**GYRE pipeline execution complete.**")

# --- Standalone execution block ---
# This allows you to run `python gyre_modules.py` directly for testing this module.
if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Run the GYRE pipeline with a specified configuration file.")
    parser.add_argument('config_file', type=str,
                        help="Path to the GYRE specific configuration file (e.g., 'gyre_config.in').")
    args = parser.parse_args()

    try:
        run_gyre_workflow(args.config_file)    
    except FileNotFoundError as e:
        logging.critical(f"\n**[Critical Error]:** {e}")
    except ValueError as e:
        logging.critical(f"\n**[Configuration Error]:** {e}")
    except IOError as e:
        logging.critical(f"\n**[File I/O Error]:** {e}")
    except subprocess.CalledProcessError as e:
        logging.critical(f"\n**[GYRE Execution Error]:** A GYRE process failed. Check the output above for details.")
    except Exception as e:
        logging.critical(f"\n**[Unexpected Error]:** An unexpected error occurred: {e}")
        import traceback
        traceback.print_exc()
