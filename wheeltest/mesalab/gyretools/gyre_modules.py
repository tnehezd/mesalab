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
import sys
from datetime import datetime

# Configure logging for this module.
gyre_logger = logging.getLogger('GYRE_Pipeline')

# --- Core GYRE Execution Function ---
def run_single_gyre_model(
    model_profile_path: str,
    gyre_inlist_template_path: str,
    output_dir: str, 
    gyre_executable: str,
    num_gyre_threads: int
) -> int:
    """
    Runs a single GYRE model using a specified MESA profile.

    This function dynamically creates the GYRE inlist file from a template.
    It updates the profile path, sets up the output directory, and runs
    the GYRE executable with the given number of OpenMP threads.

    Args:
        model_profile_path (str): Absolute path to the MESA profile file
            (e.g., 'profileXXXX.data.GYRE').
        gyre_inlist_template_path (str): Path to the GYRE inlist template file
            (now more flexible, can be anywhere user specifies).
        output_dir (str): Directory where GYRE outputs and the generated inlist for this run will be saved.
        gyre_executable (str): Absolute path to the GYRE executable binary.
        num_gyre_threads (int): Number of OpenMP threads for the GYRE run.

    Returns:
        int: The exit code of the GYRE process. Zero means success.

    Example:

        >>> from mesalab.gyretools.gyre_modules import run_single_gyre_model
        >>> import os
        >>> 
        >>> # Set the paths for your files
        >>> profile_path = "my_gyre_profile.data.GYRE"
        >>> inlist_template = "inlist_gyre_template"
        >>> output_dir = "gyre_output"
        >>> gyre_executable = "/path/to/your/gyre/bin/gyre" # Replace with your actual path
        >>> 
        >>> # Run the model
        >>> return_code = run_single_gyre_model(
        ...     model_profile_path=profile_path,
        ...     gyre_inlist_template_path=inlist_template,
        ...     output_dir=output_dir,
        ...     gyre_executable=gyre_executable,
        ...     num_gyre_threads=1
        >>> )

    """

    print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Progress: Setting up run for profile: {os.path.basename(model_profile_path)}")

    os.makedirs(output_dir, exist_ok=True)

    # 1. Generate the inlist file for the current run based on the template
    run_inlist_path = None # Initialize to None
    try:
        # Read the template inlist using f90nml
        nml = f90nml.read(gyre_inlist_template_path)
    except FileNotFoundError:
        gyre_logger.error(f"GYRE inlist template not found: {gyre_inlist_template_path}")
        return 1 # Indicate failure during setup
    except f90nml.fortran_namelist.NamelistError as e:
        gyre_logger.error(f"Error parsing GYRE inlist template {gyre_inlist_template_path}: {e}")
        return 1 # Indicate failure during setup
    except Exception as e:
        gyre_logger.error(f"Error reading GYRE inlist template {gyre_inlist_template_path}: {e}")
        return 1 # Indicate failure during setup

    # Set the MESA profile path in the inlist dynamically
    nml['model']['file'] = os.path.abspath(model_profile_path)

    # Generate a unique inlist filename for this run within its specific output_dir
    profile_base_name = os.path.basename(model_profile_path).replace('.data.GYRE', '')
    run_inlist_path = os.path.join(output_dir, f'gyre_inlist_{profile_base_name}.in')

    try:
        nml.write(run_inlist_path, force=True) # force=True will overwrite existing files
        gyre_logger.info(f"Generated specific GYRE inlist: {run_inlist_path}")
    except Exception as e:
        gyre_logger.error(f"Failed to write GYRE inlist file {run_inlist_path}: {e}")
        return 1 # Indicate failure during setup

    # 2. Set OpenMP threads for this specific GYRE process
    original_omp_num_threads = os.environ.get('OMP_NUM_THREADS')
    os.environ['OMP_NUM_THREADS'] = str(num_gyre_threads)
    gyre_logger.debug(f"OMP_NUM_THREADS set to {num_gyre_threads} for this GYRE instance.")

    # 3. Assemble and run the GYRE command
    command = [gyre_executable, os.path.basename(run_inlist_path)]

    try:
        # Important: cwd=output_dir, so GYRE reads the inlist and writes all its outputs (like .h5 files) there.
        # check=True will raise CalledProcessError if GYRE returns non-zero exit code.
        result = subprocess.run(command, capture_output=True, text=True, check=True, cwd=output_dir)

        # ONLY print/log SUCCESS if subprocess.run completed without raising an exception
        print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Progress: **{os.path.basename(output_dir)} - SUCCESS**")
        gyre_logger.info(f"**[{os.path.basename(output_dir)}] GYRE run SUCCESSFUL**!")
        if result.stdout:
            gyre_logger.debug(f"--- Standard Output (stdout) for {os.path.basename(output_dir)} ---")
            gyre_logger.debug(result.stdout)
        if result.stderr:
            gyre_logger.debug(f"--- Standard Error (stderr) for {os.path.basename(output_dir)} ---")
            gyre_logger.debug(result.stderr)
        gyre_logger.info("--- Run finished ---")
        return 0 # Success

    except FileNotFoundError:
        print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Progress: **{os.path.basename(output_dir)} - ERROR: GYRE executable not found!**")
        gyre_logger.error(f"ERROR: GYRE executable not found at '{gyre_executable}'. Please check the path specified in your config.")
        return 1 # Indicate failure

    except subprocess.CalledProcessError as e:
        # This block correctly handles non-zero exit codes from GYRE
        print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Progress: **{os.path.basename(output_dir)} - FAILED with exit code {e.returncode}!**")
        gyre_logger.error(f"**[{os.path.basename(output_dir)}] ERROR: GYRE run FAILED**, exit code: **{e.returncode}**!")
        gyre_logger.error(f"Command executed: {' '.join(e.cmd)}")
        gyre_logger.error(f"GYRE stdout:\n{e.stdout}") # GYRE errors are often in stdout
        gyre_logger.error(f"GYRE stderr:\n{e.stderr}") # Or sometimes in stderr
        return e.returncode # Return GYRE's actual error code

    except Exception as e:
        # Catch any other unexpected Python-level errors
        print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Progress: **{os.path.basename(output_dir)} - UNEXPECTED PYTHON ERROR!**")
        gyre_logger.error(f"**[{os.path.basename(output_dir)}] UNEXPECTED PYTHON ERROR occurred during GYRE run:** {e}", exc_info=True)
        gyre_logger.error("--- Error during run ---")
        return 1 # Indicate general failure

    finally:
        # This block always runs, whether there was an error or not
        if original_omp_num_threads is not None:
            os.environ['OMP_NUM_THREADS'] = original_omp_num_threads
            gyre_logger.debug(f"OMP_NUM_THREADS restored to {original_omp_num_threads}.")
        else:
            if 'OMP_NUM_THREADS' in os.environ:
                del os.environ['OMP_NUM_THREADS']
                gyre_logger.debug("OMP_NUM_THREADS unset.")

# --- Main GYRE Pipeline Management Function ---
def run_gyre_workflow(
    config_data, # The addict.Dict (or similar) containing all resolved config settings
    filtered_profiles_csv_path: str = None, # Path to the filtered profiles CSV, typically generated by mesa_analyzer
    debug_mode: bool = False,
    gyre_output_subdir: str = None # The new dedicated output subdirectory for GYRE
):
    """
    Manages the GYRE pulsation analysis workflow based on the provided configuration data.

    This function performs the following steps:
        1. Sets up logging based on debug_mode.
        2. Extracts GYRE-specific and general settings from the `config_data` object.
        3. Validates paths to the GYRE executable and the GYRE inlist template.
        4. Determines the mode of operation ('ALL_PROFILES' or 'FILTERED_PROFILES').
        5. Creates a list of tasks (MESA profiles) to be analyzed by GYRE.
        6. Runs GYRE, either sequentially or in parallel, using `run_single_gyre_model`.
        7. Organizes outputs into the dedicated `gyre_output_subdir`.

    Args:
        config_data (addict.Dict): A dictionary-like object containing all
            configuration settings (e.g., from config.yaml parsed by mesalab).
        filtered_profiles_csv_path (str, optional): The absolute path to the CSV
            file containing filtered MESA profiles. If None, the function will
            create the path based on `config_data.gyre_workflow.filtered_profiles_csv_name`.
        debug_mode (bool, optional): If True, enables verbose debug logging for this module.
            Defaults to False.
        gyre_output_subdir (str, optional): The absolute path to the dedicated output
            subdirectory for all GYRE run files. If None, the default will be used.
            Defaults to None.

    Returns:
        int: Returns 0 on success, 1 on failure.

    **NOTE:** This function is not designed for simple command-line execution (`python -c "..."`).
    It requires external libraries (e.g., pandas, f90nml) and a specific file/directory structure.
    For proper use, save this code in a Python file and run it as a script.

    Example:
        >>> from addict import Dict
        >>> from mesalab.gyretools.gyre_modules import run_gyre_workflow
        >>> import os
        >>> 
        >>> # 1. Define your configuration object (originally read from the config.yaml)
        >>> config = Dict({
        ...    'general_settings': {
        ...        'gyre_dir': '/path/to/your/gyre-7.0/bin',
        ...        'output_dir': './my_output_dir',
        ...        'input_dir': './my_mesa_runs'
        ...    },
        ...    'gyre_workflow': {
        ...        'run_gyre_workflow': True,
        ...        'run_mode': 'ALL_PROFILES',
        ...        'mesa_profile_pattern': 'profile*.data.GYRE',
        ...        'mesa_profile_base_dir_relative': 'M_2.0_Z_0.014/LOGS',
        ...        'gyre_inlist_template_path': './inlist_gyre_template',
        ...        'enable_gyre_parallel': True,
        ...        'max_concurrent_gyre_runs': 4,
        ...        'num_gyre_threads': 1
        ...    }
        ... })
        >>> 
        >>> # 2. Run the workflow
        >>> return_code = run_gyre_workflow(config_data=config)
        >>> 
        >>> if return_code == 0:
        >>>     print("The GYRE workflow completed successfully!")
        >>> else:
        >>>     print("An error occurred during the GYRE workflow.")

    """
    if debug_mode:
        gyre_logger.setLevel(logging.DEBUG)
        gyre_logger.debug("GYRE workflow debug mode enabled.")
    else:
        gyre_logger.setLevel(logging.WARNING)

    print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Pipeline: Initializing GYRE workflow from mesalab configuration...")

    try:
        gyre_cfg = config_data.gyre_workflow
        general_settings = config_data.general_settings

        run_gyre_workflow_enabled = gyre_cfg.get('run_gyre_workflow', False)
        if not run_gyre_workflow_enabled:
            gyre_logger.info("GYRE workflow is disabled in the configuration. Skipping GYRE run steps.")
            return 0 # Exit the function early

    except AttributeError as e:
        gyre_logger.critical(f"Missing expected configuration section: {e}. Please check your main config.yaml structure.")
        raise ValueError(f"Configuration structure error: {e}")

    # Validate essential general settings (always required)
    required_general_params = ['gyre_dir', 'output_dir', 'input_dir']
    for param in required_general_params:
        if not getattr(general_settings, param, None):
            raise ValueError(f"Missing required parameter '{param}' in the 'general_settings' section of your main config.")

    # Validate essential GYRE workflow settings
    required_gyre_params = ['run_mode', 'num_gyre_threads', 'enable_gyre_parallel', 'max_concurrent_gyre_runs']
    for param in required_gyre_params:
        if getattr(gyre_cfg, param, None) is None:
            if param in ['num_gyre_threads', 'max_concurrent_gyre_runs'] and not isinstance(getattr(gyre_cfg, param), (int, float)):
                raise ValueError(f"Missing or invalid required parameter '{param}' in the 'gyre_workflow' section of your main config.")
            elif param == 'enable_gyre_parallel' and not isinstance(getattr(gyre_cfg, param), bool):
                raise ValueError(f"Missing or invalid required parameter '{param}' in the 'gyre_workflow' section of your main config.")
            else:
                raise ValueError(f"Missing required parameter '{param}' in the 'gyre_workflow' section of your main config.")

    if not getattr(gyre_cfg, 'gyre_inlist_template_path', None):
        raise ValueError("Missing required parameter 'gyre_inlist_template_path' in the 'gyre_workflow' section of your main config. This is required when 'run_gyre_workflow' is true.")

    # --- Setup Paths ---
    gyre_install_dir = general_settings.gyre_dir
    global_output_base_dir = general_settings.output_dir
    global_input_dir = general_settings.input_dir

    # Use the passed gyre_output_subdir parameter
    if gyre_output_subdir is None:
        gyre_session_output_dir = os.path.join(global_output_base_dir, 'gyre_outputs')
    else:
        gyre_session_output_dir = gyre_output_subdir

    os.makedirs(gyre_session_output_dir, exist_ok=True)
    gyre_logger.info(f"**GYRE specific outputs will be saved in: '{os.path.abspath(gyre_session_output_dir)}'**")

    gyre_executable = shutil.which("gyre")
    if not gyre_executable:
        gyre_logger.critical(f"GYRE executable not found in system PATH. Please ensure GYRE_DIR ('{gyre_install_dir}') "
                             f"is correctly set in your main config and GYRE is compiled and its 'bin' folder is in PATH.")
        raise FileNotFoundError("GYRE executable not found.")
    gyre_logger.info(f"**GYRE executable found at: '{gyre_executable}'**")

    gyre_inlist_template_from_config = gyre_cfg.gyre_inlist_template_path
    gyre_logger.debug(f"GYRE inlist template path from config: {gyre_inlist_template_from_config}")

    if not os.path.isabs(gyre_inlist_template_from_config):
        gyre_inlist_template_full_path = os.path.abspath(gyre_inlist_template_from_config)
    else:
        gyre_inlist_template_full_path = gyre_inlist_template_from_config

    if not os.path.exists(gyre_inlist_template_full_path):
        gyre_logger.critical(f"Critical error during GYRE workflow: GYRE inlist template '{gyre_inlist_template_full_path}' specified by 'gyre_inlist_template_path' in your main config not found. Please ensure it exists.")
        raise FileNotFoundError(f"GYRE inlist template '{gyre_inlist_template_full_path}' specified by 'gyre_inlist_template_path' in your main config not found.")
    gyre_logger.info(f"**GYRE inlist template: '{gyre_inlist_template_full_path}'**")

    gyre_logger.info("\n--- Building GYRE Run Tasks ---")
    tasks = []
    run_mode = gyre_cfg.run_mode.upper()

    if run_mode == 'ALL_PROFILES':
        if not getattr(gyre_cfg, 'mesa_profile_pattern', None) or \
           not getattr(gyre_cfg, 'mesa_profile_base_dir_relative', None):
            raise ValueError("Missing 'mesa_profile_pattern' or 'mesa_profile_base_dir_relative' in 'gyre_workflow' section for 'ALL_PROFILES' mode.")

        global_mesa_logs_dir = os.path.join(global_input_dir, gyre_cfg.mesa_profile_base_dir_relative)

        if not os.path.exists(global_mesa_logs_dir):
            raise FileNotFoundError(f"MESA profile base directory for 'ALL_PROFILES' mode not found: '{global_mesa_logs_dir}'. "
                                     f"Please check 'general_settings.input_dir' and 'gyre_workflow.mesa_profile_base_dir_relative' in your main config.")

        all_profile_paths = sorted(glob.glob(os.path.join(global_mesa_logs_dir, gyre_cfg.mesa_profile_pattern)))
        gyre_logger.info(f"**Run mode: ALL_PROFILES.** Found {len(all_profile_paths)} MESA profile(s) to process based on pattern '{gyre_cfg.mesa_profile_pattern}'.")

        if not all_profile_paths:
            gyre_logger.warning(f"**WARNING:** No MESA profile files found to process. Skipping GYRE runs.")
            return 0

        for profile_path in all_profile_paths:
            profile_id_from_filename = os.path.basename(profile_path).replace('.data.GYRE', '')
            run_output_dir = os.path.join(gyre_session_output_dir, f"all_profiles_run_{profile_id_from_filename}")

            tasks.append((
                profile_path,
                gyre_inlist_template_full_path,
                run_output_dir,
                gyre_executable,
                gyre_cfg.num_gyre_threads
            ))

    elif run_mode == 'FILTERED_PROFILES':
        if filtered_profiles_csv_path is None:
            if not getattr(gyre_cfg, 'filtered_profiles_csv_name', None):
                raise ValueError("Missing 'filtered_profiles_csv_name' in 'gyre_workflow' section for 'FILTERED_PROFILES' mode.")

            filtered_profiles_csv_path = os.path.join(
                global_output_base_dir,
                'analysis_results',
                gyre_cfg.filtered_profiles_csv_name
            )

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
            return 0

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

            print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Progress: Processing M={mass}, Z={Z} from run directory: {os.path.basename(mesa_run_specific_dir)}")
            print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Progress: Searching profiles in: {current_mesa_run_logs_dir} within model range [{min_model}-{max_model}]")

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
                            gyre_logger.debug(f"Skipping malformed line (too few columns) in {current_profiles_index_path}: {line.strip()}")
                            continue

                        try:
                            model_num = int(parts[0])
                            profile_num = int(parts[-1])
                            model_to_profile_map[model_num] = profile_num
                        except ValueError:
                            gyre_logger.debug(f"Skipping malformed line (non-integer model/profile num) in {current_profiles_index_path}: {line.strip()}")
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

            mesa_run_basename = os.path.basename(mesa_run_specific_dir)
            specific_model_output_root = os.path.join(
                gyre_session_output_dir,
                mesa_run_basename
            )
            os.makedirs(specific_model_output_root, exist_ok=True)

            for prof_num in sorted(list(selected_profile_numbers_for_this_run)):
                expected_profile_name = f'profile{prof_num}.data.GYRE'
                profile_path = os.path.join(current_mesa_run_logs_dir, expected_profile_name)

                if os.path.exists(profile_path):
                    run_output_dir = os.path.join(specific_model_output_root, f'profile{prof_num:05d}')

                    tasks.append((
                        profile_path,
                        gyre_inlist_template_full_path,
                        run_output_dir,
                        gyre_executable,
                        gyre_cfg.num_gyre_threads
                    ))
                else:
                    gyre_logger.warning(f"**WARNING:** Filtered profile '{expected_profile_name}' not found for M={mass}, Z={Z} at '{profile_path}'. Skipping.")

        gyre_logger.info(f"\nTotal GYRE tasks prepared: {len(tasks)}")

    else:
        raise ValueError(f"Invalid 'run_mode' specified in config: {gyre_cfg.run_mode}. "
                         f"Accepted values are 'ALL_PROFILES' or 'FILTERED_PROFILES'.")

    # --- Execute GYRE Tasks ---
    if not tasks:
        print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Pipeline: No GYRE tasks were prepared. Skipping GYRE runs.")
        return 0
    else:
        max_concurrent_runs = gyre_cfg.max_concurrent_gyre_runs
        all_gyre_return_codes = []

        if gyre_cfg.enable_gyre_parallel:
            if not isinstance(max_concurrent_runs, int) or max_concurrent_runs <= 0:
                raise ValueError(f"Invalid 'max_concurrent_gyre_runs' in config: {max_concurrent_runs}. Must be a positive integer.")
            gyre_logger.info(f"**Parallel GYRE execution enabled.** Running {max_concurrent_runs} job(s) concurrently.")
            with multiprocessing.Pool(processes=max_concurrent_runs) as pool:
                all_gyre_return_codes = pool.starmap(run_single_gyre_model, tasks)
        else:
            gyre_logger.info("**Parallel GYRE execution disabled.** Running jobs sequentially.")
            for task in tasks:
                return_code = run_single_gyre_model(*task)
                all_gyre_return_codes.append(return_code)

        print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Pipeline: All GYRE runs completed.")

        if any(code != 0 for code in all_gyre_return_codes):
            print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Pipeline: **WARNING: One or more GYRE runs FAILED!**")
            gyre_logger.warning("GYRE Pipeline: One or more GYRE runs failed. Check individual profile logs for details.")
            return 1
        else:
            print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Pipeline: **All individual GYRE runs completed successfully.**")
            gyre_logger.info("GYRE Pipeline: All individual GYRE runs completed successfully.")
            return 0

    print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] GYRE Pipeline: **GYRE pipeline execution complete.**")


# --- Standalone execution for testing/debugging ---
if __name__ == "__main__":
    from addict import Dict
    from mesalab.gyretools.gyre_modules import run_single_gyre_model

    # NOTE: You MUST replace this with a valid path for your GYRE installation
    # and a valid inlist template path for this test to work!
    # Mock configuration for testing purposes
    # The `gyre_dir` should point to the root of your GYRE installation
    # The `gyre_inlist_template_path` must point to an actual GYRE inlist file
    # The `input_dir` and `output_dir` must be valid paths
    
    gyre_test_config = Dict({
        'general_settings': {
            'gyre_dir': '/path/to/your/gyre',
            'output_dir': os.path.join(os.getcwd(), 'test_output'),
            'input_dir': os.path.join(os.getcwd(), 'test_input')
        },
        'gyre_workflow': {
            'run_gyre_workflow': True,
            'run_mode': 'ALL_PROFILES', # Or 'FILTERED_PROFILES'
            'mesa_profile_pattern': 'profile*.data.GYRE',
            'mesa_profile_base_dir_relative': 'MESA_grid_output/run_2.0MSUN_z0.014/LOGS',
            'gyre_inlist_template_path': os.path.join(os.getcwd(), 'inlist_gyre_template'),
            'enable_gyre_parallel': True,
            'max_concurrent_gyre_runs': 2,
            'num_gyre_threads': 1
        }
    })

    # Create dummy directories and files for testing
    os.makedirs(gyre_test_config.general_settings.output_dir, exist_ok=True)
    os.makedirs(os.path.join(gyre_test_config.general_settings.input_dir, gyre_test_config.gyre_workflow.mesa_profile_base_dir_relative), exist_ok=True)
    with open(os.path.join(gyre_test_config.general_settings.input_dir, gyre_test_config.gyre_workflow.mesa_profile_base_dir_relative, 'profile1.data.GYRE'), 'w') as f:
        f.write("# Dummy GYRE profile data")
    with open(os.path.join(gyre_test_config.general_settings.input_dir, gyre_test_config.gyre_workflow.mesa_profile_base_dir_relative, 'profile2.data.GYRE'), 'w') as f:
        f.write("# Dummy GYRE profile data")
    with open(gyre_test_config.gyre_workflow.gyre_inlist_template_path, 'w') as f:
        f.write("# Dummy GYRE inlist template")
    
    # Run the workflow
    try:
        run_gyre_workflow(
            config_data=gyre_test_config,
            gyre_output_subdir=os.path.join(gyre_test_config.general_settings.output_dir, 'custom_gyre_test')
        )
    except Exception as e:
        print(f"Test run failed with an exception: {e}")
        exit(1)

    print("\nStandalone GYRE workflow test completed.")
