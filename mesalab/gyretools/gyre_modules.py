# mesalab/gyretools/gyre_modules.py

import subprocess
import os
import multiprocessing
import shutil
import glob
import f90nml
import pandas as pd
import h5py
import re
import logging
import numpy as np

# Configure logging for this module.
gyre_logger = logging.getLogger('GYRE_Pipeline')

# --- Core GYRE Execution Function ---
def run_single_gyre_model(
    model_profile_path: str,
    gyre_inlist_template_path: str,
    output_dir: str, # This is the specific directory for THIS profile's GYRE outputs
    gyre_executable: str,
    num_gyre_threads: int
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
    gyre_logger.info(f"Setting up GYRE run for profile: {os.path.basename(model_profile_path)}")
    os.makedirs(output_dir, exist_ok=True)
    
    # 1. Generate the inlist file for the current run
    run_inlist_path = None # Initialize to None
    try:
        # Read the template inlist using f90nml
        nml = f90nml.read(gyre_inlist_template_path)
    except FileNotFoundError:
        gyre_logger.error(f"GYRE inlist template not found: {gyre_inlist_template_path}")
        raise
    except f90nml.fortran_namelist.NamelistError as e:
        gyre_logger.error(f"Error parsing GYRE inlist template {gyre_inlist_template_path}: {e}")
        raise
    except Exception as e:
        gyre_logger.error(f"Error reading GYRE inlist template {gyre_inlist_template_path}: {e}")
        raise

    # Set the MESA profile path in the inlist
    # The 'file' parameter in '&model' block is updated to the absolute path of the profile
    nml['model']['file'] = os.path.abspath(model_profile_path)

    # *** Módosítás itt: GYRE output subdirectory ***
    # GYRE expects 'summary_file = 'gyre_out/summary_...' and writes directly to CWD + 'gyre_out'.
    # We ensure that 'output_dir' is the CWD for GYRE. So, 'gyre_out' will be created INSIDE 'output_dir'.
    # No need to explicitly create `gyre_output_subdir` here, as GYRE will create `gyre_out` itself
    # when run with `cwd=output_dir` and the inlist configured for `gyre_out/`.
    # This also means `gyre_out` will not be empty if GYRE successfully writes results.

    # Generate a unique inlist filename for this run within its specific output_dir
    profile_base_name = os.path.basename(model_profile_path).replace('.data.GYRE', '')
    run_inlist_path = os.path.join(output_dir, f'gyre_inlist_{profile_base_name}.in')
    
    try:
        nml.write(run_inlist_path, force=True) # force=True will overwrite existing files
        gyre_logger.info(f"Generated specific GYRE inlist: {run_inlist_path}")
    except Exception as e:
        gyre_logger.error(f"Failed to write GYRE inlist file {run_inlist_path}: {e}")
        raise

    # 2. Set OpenMP threads for this specific GYRE process
    original_omp_num_threads = os.environ.get('OMP_NUM_THREADS') # Store original value
    os.environ['OMP_NUM_THREADS'] = str(num_gyre_threads)
    gyre_logger.info(f"OMP_NUM_THREADS set to {num_gyre_threads} for this GYRE instance.")

    # 3. Assemble and run the GYRE command
    # Use os.path.basename(run_inlist_path) because cwd is set to output_dir,
    # meaning GYRE will look for the inlist by its name in the current working directory.
    command = [gyre_executable, os.path.basename(run_inlist_path)]

    gyre_logger.info(f"**[{os.path.basename(output_dir)}]** Attempting to run GYRE with command: `{gyre_executable} {os.path.basename(run_inlist_path)}`")

    try:
        # Important: cwd=output_dir, so GYRE reads the inlist and writes all its outputs (like .h5 files) there.
        result = subprocess.run(command, capture_output=True, text=True, check=True, cwd=output_dir)

        gyre_logger.info(f"**[{os.path.basename(output_dir)}] GYRE run SUCCESSFUL**!")
        gyre_logger.debug(f"--- Standard Output (stdout) for {os.path.basename(output_dir)} ---")
        gyre_logger.debug(result.stdout)
        if result.stderr:
            gyre_logger.debug(f"--- Standard Error (stderr) for {os.path.basename(output_dir)} ---")
            gyre_logger.debug(result.stderr)
        gyre_logger.info("--- Run finished ---")

    except FileNotFoundError:
        gyre_logger.error(f"ERROR: GYRE executable not found at '{gyre_executable}'. Please check the path specified in your config.")
        raise # Re-raise for the main error handler
    except subprocess.CalledProcessError as e:
        gyre_logger.error(f"**[{os.path.basename(output_dir)}] ERROR: GYRE run FAILED**, exit code: **{e.returncode}**!")
        gyre_logger.error(f"Command executed: {' '.join(e.cmd)}")
        gyre_logger.error(f"GYRE stdout:\n{e.stdout}")
        gyre_logger.error(f"GYRE stderr:\n{e.stderr}")
        raise # Re-raise the exception for main() to catch
    except Exception as e:
        gyre_logger.error(f"**[{os.path.basename(output_dir)}] UNEXPECTED ERROR occurred during GYRE run:** {e}")
        gyre_logger.error("--- Error during run ---")
        raise # Re-raise the exception
    finally:
        # Restore OMP_NUM_THREADS to its original value or unset if it wasn't set before
        if original_omp_num_threads is not None:
            os.environ['OMP_NUM_THREADS'] = original_omp_num_threads
            gyre_logger.info(f"OMP_NUM_THREADS restored to {original_omp_num_threads}.")
        else:
            if 'OMP_NUM_THREADS' in os.environ:
                del os.environ['OMP_NUM_THREADS']
                gyre_logger.info("OMP_NUM_THREADS unset.")

# --- Main GYRE Pipeline Orchestration Function ---
def run_gyre_workflow(
    gyre_config_path: str,
    filtered_profiles_csv_path: str = None,
    global_mesa_base_dir: str = '.',
    global_output_base_dir: str = './output',
    debug_mode: bool = False
):
    """
    Orchestrates the GYRE runs based on configuration read from gyre_config_path.
    """
    if debug_mode:
        gyre_logger.setLevel(logging.DEBUG)
        gyre_logger.debug("GYRE workflow debug mode enabled.")

    if not os.path.exists(gyre_config_path):
        gyre_logger.critical(f"GYRE configuration file '{gyre_config_path}' not found. Please ensure it exists.")
        raise FileNotFoundError(f"Configuration file '{gyre_config_path}' not found.")

    gyre_logger.info(f"**Reading GYRE specific configuration from '{gyre_config_path}'...**")
    try:
        config = f90nml.read(gyre_config_path)
    except FileNotFoundError:
        gyre_logger.critical(f"Configuration file '{gyre_config_path}' not found.")
        raise
    except f90nml.fortran_namelist.NamelistError as e:
        gyre_logger.critical(f"Error parsing configuration file '{gyre_config_path}': {e}")
        raise
    except Exception as e:
        gyre_logger.critical(f"An unexpected error occurred while reading configuration '{gyre_config_path}': {e}")
        raise

    setup_cfg = config.get('setup', {})
    gyre_cfg = config.get('gyre_options', {})

    required_setup_params = ['gyre_dir']
    for param in required_setup_params:
        if param not in setup_cfg:
            raise ValueError(f"Missing required parameter '{param}' in the '[setup]' section of '{gyre_config_path}'.")

    required_gyre_params = ['run_mode', 'mesa_profile_base_dir', 'mesa_profile_pattern', 'gyre_inlist_template',
                            'num_gyre_threads', 'enable_parallel', 'max_concurrent_gyre_runs']
    for param in required_gyre_params:
        if param not in gyre_cfg:
            raise ValueError(f"Missing required parameter '{param}' in the '[gyre_options]' section of '{gyre_config_path}'.")

    # --- Setup Paths ---
    gyre_install_dir = setup_cfg['gyre_dir']
    
    # *** Módosítás itt: A gyre_session_output_dir beállítása ***
    # Eltávolítjuk a duplázást. A GYRE outputok közvetlenül ide kerülnek.
    gyre_session_output_dir = os.path.join(global_output_base_dir)
    os.makedirs(gyre_session_output_dir, exist_ok=True)
    gyre_logger.info(f"**GYRE specific outputs will be saved in: '{os.path.abspath(gyre_session_output_dir)}'**")

    # Determine GYRE executable path
    gyre_executable = os.path.join(gyre_install_dir, 'bin', 'gyre')
    
    if not (os.path.exists(gyre_executable) and os.path.isfile(gyre_executable) and os.access(gyre_executable, os.X_OK)):
        gyre_executable_fallback = os.path.join(gyre_install_dir, 'gyre')
        if (os.path.exists(gyre_executable_fallback) and os.path.isfile(gyre_executable_fallback) and os.access(gyre_executable_fallback, os.X_OK)):
            gyre_executable = gyre_executable_fallback
        else:
            raise FileNotFoundError(f"GYRE executable not found or not executable at '{gyre_executable}' or '{gyre_executable_fallback}'. "
                                    f"Please check your 'gyre_dir' in '{gyre_config_path}'.")
    gyre_logger.info(f"**GYRE executable found at: '{gyre_executable}'**")
    
    # Validate GYRE inlist template path
    gyre_config_dir = os.path.dirname(os.path.abspath(gyre_config_path))
    gyre_inlist_template_full_path = os.path.abspath(os.path.join(gyre_config_dir, gyre_cfg['gyre_inlist_template']))

    if not os.path.exists(gyre_inlist_template_full_path):
        raise FileNotFoundError(f"GYRE inlist template '{gyre_inlist_template_full_path}' specified in '{gyre_config_path}' not found.")
    gyre_logger.info(f"**GYRE inlist template: '{gyre_inlist_template_full_path}'**")

    gyre_logger.info("\n--- GYRE Run Starting ---")

    tasks = []
    
    run_mode = gyre_cfg['run_mode'].upper()

    if run_mode == 'ALL_PROFILES':
        global_mesa_logs_dir = os.path.join(global_mesa_base_dir, gyre_cfg['mesa_profile_base_dir'])
        
        if not os.path.exists(global_mesa_logs_dir):
            raise FileNotFoundError(f"MESA profile base directory for 'ALL_PROFILES' mode not found: '{global_mesa_logs_dir}'. "
                                    f"Please check 'input_dir' in your main config and 'mesa_profile_base_dir' in '{gyre_config_path}'.")

        all_profile_paths = sorted(glob.glob(os.path.join(global_mesa_logs_dir, gyre_cfg['mesa_profile_pattern'])))
        gyre_logger.info(f"**Run mode: ALL_PROFILES.** Found {len(all_profile_paths)} MESA profile(s) to process based on pattern '{gyre_cfg['mesa_profile_pattern']}'.")
        
        if not all_profile_paths:
            gyre_logger.warning(f"**WARNING:** No MESA profile files found to process. Skipping GYRE runs.")
            return
        else:
            for profile_path in all_profile_paths:
                profile_id_from_filename = os.path.basename(profile_path).replace('.data.GYRE', '')
                # *** Módosítás itt: output_dir strukturálása ALL_PROFILES esetén ***
                # A run_output_dir most közvetlenül a gyre_session_output_dir alatt lesz.
                # Így a kimenet: `gyre_output/all_profiles_run_profileXXXXX/gyre_out`
                run_output_dir = os.path.join(gyre_session_output_dir, f"all_profiles_run_{profile_id_from_filename}")
                
                tasks.append((
                    profile_path,
                    gyre_inlist_template_full_path,
                    run_output_dir,
                    gyre_executable,
                    gyre_cfg['num_gyre_threads']
                ))

    elif run_mode == 'FILTERED_PROFILES':
        if not filtered_profiles_csv_path:
            raise ValueError(f"FILTERED_PROFILES mode selected, but 'filtered_profiles_csv_path' was not provided. "
                             "This path should come from the MESA analysis output.")
        
        if not os.path.exists(filtered_profiles_csv_path):
            raise FileNotFoundError(f"FILTERED_PROFILES mode selected, but CSV file '{filtered_profiles_csv_path}' not found. "
                                    f"Please ensure mesa_analyzer generated it correctly and cli.py passed the correct path.")

        gyre_logger.info(f"**Run mode: FILTERED_PROFILES.** Reading filter criteria from '{filtered_profiles_csv_path}'...")
        
        try:
            filter_df = pd.read_csv(filtered_profiles_csv_path)
        except Exception as e:
            raise IOError(f"Error reading filtered profiles CSV '{filtered_profiles_csv_path}': {e}")

        if filter_df.empty:
            gyre_logger.warning(f"**WARNING:** Filtered profiles CSV '{filtered_profiles_csv_path}' is empty. No profiles to process for GYRE runs.")
            return

        required_cols = ['initial_mass', 'initial_Z', 'min_model_number', 'max_model_number', 'mesa_run_directory']
        if not all(col in filter_df.columns for col in required_cols):
            raise ValueError(f"Filtered profiles CSV '{filtered_profiles_csv_path}' must contain the columns: {', '.join(required_cols)}")
        
        for index, row in filter_df.iterrows():
            mass = row['initial_mass'] if pd.notna(row['initial_mass']) else np.nan
            Z = row['initial_Z'] if pd.notna(row['initial_Z']) else np.nan

            if pd.isna(mass) or pd.isna(Z):
                gyre_logger.warning(f"Skipping row {index} in CSV due to missing initial_mass or initial_Z.")
                continue

            if pd.isna(row['min_model_number']) or pd.isna(row['max_model_number']):
                gyre_logger.warning(f"Skipping row {index} for M={mass}, Z={Z} due to missing (NaN) min_model_number or max_model_number. GYRE requires valid model ranges.")
                continue
            
            min_model = int(row['min_model_number'])
            max_model = int(row['max_model_number'])
            
            mesa_run_specific_dir = row['mesa_run_directory']
            
            current_mesa_run_logs_dir = os.path.join(mesa_run_specific_dir, 'LOGS')
            current_profiles_index_path = os.path.join(current_mesa_run_logs_dir, 'profiles.index')

            gyre_logger.info(f"\nProcessing M={mass}, Z={Z} from run directory: {os.path.basename(mesa_run_specific_dir)}")
            gyre_logger.info(f"Searching profiles in: {current_mesa_run_logs_dir} within model range [{min_model}-{max_model}]")

            model_to_profile_map = {}
            
            if not os.path.exists(current_profiles_index_path):
                gyre_logger.warning(f"**WARNING:** profiles.index not found for M={mass}, Z={Z} at: '{current_profiles_index_path}'. Skipping this M-Z combination.")
                continue

            try:
                with open(current_profiles_index_path, 'r') as f:
                    first_line = f.readline().strip()
                    if not re.match(r'^\d', first_line):
                        pass
                    else:
                        f.seek(0)

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
                            model_to_profile_map[model_num] = profile_num
                        except ValueError:
                            gyre_logger.debug(f"Skipping malformed line in {current_profiles_index_path}: {line.strip()}")
                            continue
            except Exception as e:
                gyre_logger.error(f"**ERROR:** Error reading profiles.index for M={mass}, Z={Z} at '{current_profiles_index_path}': {e}. Skipping.")
                continue

            if not model_to_profile_map:
                gyre_logger.warning(f"**WARNING:** No valid data found in '{current_profiles_index_path}' for M={mass}, Z={Z}. Skipping.")
                continue

            selected_profile_numbers_for_this_run = set()
            for model_num_in_index, profile_num_in_index in model_to_profile_map.items():
                if min_model <= model_num_in_index <= max_model:
                    selected_profile_numbers_for_this_run.add(profile_num_in_index)

            if not selected_profile_numbers_for_this_run:
                gyre_logger.warning(f"**WARNING:** No MESA profiles found for M={mass}, Z={Z} within model range {min_model}-{max_model}. Skipping.")
                continue

            # Create a specific output directory for this MESA run's GYRE results
            # *** Módosítás itt: specific_model_output_root strukturálása FILTERED_PROFILES esetén ***
            # A mesa_run_basename (pl. `run_nad_convos_mid_3.0MSUN_z0.0015`)
            # közvetlenül a gyre_session_output_dir alá kerül.
            # Eredmény: `gyre_output/run_nad_convos_mid_3.0MSUN_z0.0015/profileXXXXX/gyre_out`
            mesa_run_basename = os.path.basename(mesa_run_specific_dir)
            specific_model_output_root = os.path.join(
                gyre_session_output_dir, # Használja a már letisztult gyre_output mappát
                mesa_run_basename
            )
            os.makedirs(specific_model_output_root, exist_ok=True)
            
            for prof_num in sorted(list(selected_profile_numbers_for_this_run)):
                expected_profile_name = f'profile{prof_num}.data.GYRE'
                profile_path = os.path.join(current_mesa_run_logs_dir, expected_profile_name)
                
                if os.path.exists(profile_path):
                    # run_output_dir lesz az aktuális GYRE cwd, így a `gyre_out` mappa ebbe fog kerülni.
                    run_output_dir = os.path.join(specific_model_output_root, f'profile{prof_num:05d}')
                    
                    tasks.append((
                        profile_path,
                        gyre_inlist_template_full_path,
                        run_output_dir,
                        gyre_executable,
                        gyre_cfg['num_gyre_threads']
                    ))
                else:
                    gyre_logger.warning(f"**WARNING:** Filtered profile '{expected_profile_name}' not found for M={mass}, Z={Z} at '{profile_path}'. Skipping.")
            
        gyre_logger.info(f"\nTotal GYRE tasks prepared: {len(tasks)}")

    else:
        raise ValueError(f"Invalid 'run_mode' specified in '{gyre_config_path}': {gyre_cfg['run_mode']}. "
                         f"Accepted values are 'ALL_PROFILES' or 'FILTERED_PROFILES'.")

    if not tasks:
        gyre_logger.warning(f"**WARNING:** No GYRE tasks were prepared. Skipping GYRE runs.")
    else:
        if gyre_cfg['enable_parallel']:
            max_concurrent_runs = gyre_cfg['max_concurrent_gyre_runs']
            if not isinstance(max_concurrent_runs, int) or max_concurrent_runs <= 0:
                raise ValueError(f"Invalid 'max_concurrent_gyre_runs' in config: {max_concurrent_runs}. Must be a positive integer.")
            gyre_logger.info(f"**Parallel GYRE execution enabled.** Running {max_concurrent_runs} job(s) concurrently.")
            with multiprocessing.Pool(processes=max_concurrent_runs) as pool:
                pool.starmap(run_single_gyre_model, tasks)
        else:
            gyre_logger.info("**Parallel GYRE execution disabled.** Running jobs sequentially.")
            for task in tasks:
                run_single_gyre_model(*task)
            
        gyre_logger.info("\n--- GYRE Run Finished ---")

    gyre_logger.info("\n**GYRE pipeline execution complete.**")

if __name__ == "__main__":
    import argparse
    import sys

    parser = argparse.ArgumentParser(description="Run the GYRE pipeline with a specified configuration file.")
    parser.add_argument('gyre_config_path', type=str,
                        help="Path to the GYRE specific configuration file (e.g., 'gyre_config.in').")
    parser.add_argument('--filtered_profiles_csv_path', type=str, default=None,
                        help="Path to the filtered profiles CSV for 'FILTERED_PROFILES' mode (optional for testing).")
    parser.add_argument('--global_mesa_base_dir', type=str, default=os.getcwd(),
                        help="Top-level MESA grid directory (e.g., input_dir from main config).")
    parser.add_argument('--global_output_base_dir', type=str, default='./standalone_gyre_output',
                        help="Top-level output directory (e.g., output_dir from main config).")
    parser.add_argument('--debug', action='store_true', help='Enable debug logging for standalone run.')

    args = parser.parse_args()

    if not gyre_logger.handlers:
        gyre_logger.setLevel(logging.DEBUG if args.debug else logging.INFO)
        gyre_logger.addHandler(logging.StreamHandler(sys.stdout))

    try:
        run_gyre_workflow(
            gyre_config_path=args.gyre_config_path,
            filtered_profiles_csv_path=args.filtered_profiles_csv_path,
            global_mesa_base_dir=args.global_mesa_base_dir,
            global_output_base_dir=args.global_output_base_dir,
            debug_mode=args.debug
        )
    except FileNotFoundError as e:
        gyre_logger.critical(f"\n**[Critical Error]:** {e}")
    except ValueError as e:
        gyre_logger.critical(f"\n**[Configuration Error]:** {e}")
    except IOError as e:
        gyre_logger.critical(f"\n**[File I/O Error]:** {e}")
    except subprocess.CalledProcessError as e:
        gyre_logger.critical(f"\n**[GYRE Execution Error]:** A GYRE process failed. Check the output above for details.")
    except Exception as e:
        gyre_logger.critical(f"\n**[Unexpected Error]:** An unexpected error occurred: {e}")
        import traceback
        traceback.print_exc()
