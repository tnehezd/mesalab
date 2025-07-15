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

# Configure logging for this module.
gyre_logger = logging.getLogger('GYRE_Pipeline')

# --- Core GYRE Execution Function ---
def run_single_gyre_model(
    model_profile_path: str,
    gyre_inlist_template_path: str, # This parameter name is already correct
    output_dir: str, # This is the specific directory for THIS profile's GYRE outputs
    gyre_executable: str,
    num_gyre_threads: int
):
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
    """
    gyre_logger.info(f"Setting up GYRE run for profile: {os.path.basename(model_profile_path)}")
    os.makedirs(output_dir, exist_ok=True)

    # 1. Generate the inlist file for the current run based on the template
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

    # Set the MESA profile path in the inlist dynamically
    # This is a crucial step for templating the GYRE input
    nml['model']['file'] = os.path.abspath(model_profile_path)

    # GYRE outputs (like summary.h5, detail files) are often configured within the GYRE inlist
    # to be placed in a subdirectory (e.g., 'gyre_out/').
    # By setting `cwd=output_dir` for the subprocess, GYRE will create this 'gyre_out' subdirectory
    # directly inside `output_dir`. There's no need to explicitly create `gyre_out` here.

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
    # Store original value to restore it later, preventing interference with other processes
    original_omp_num_threads = os.environ.get('OMP_NUM_THREADS')
    os.environ['OMP_NUM_THREADS'] = str(num_gyre_threads)
    gyre_logger.debug(f"OMP_NUM_THREADS set to {num_gyre_threads} for this GYRE instance.")

    # 3. Assemble and run the GYRE command
    # Use os.path.basename(run_inlist_path) because cwd is set to output_dir,
    # meaning GYRE will look for the inlist by its name in the current working directory.
    command = [gyre_executable, os.path.basename(run_inlist_path)]

    gyre_logger.info(f"**[{os.path.basename(output_dir)}]** Attempting to run GYRE with command: `{gyre_executable} {os.path.basename(run_inlist_path)}`")

    try:
        # Important: cwd=output_dir, so GYRE reads the inlist and writes all its outputs (like .h5 files) there.
        result = subprocess.run(command, capture_output=True, text=True, check=True, cwd=output_dir)

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
        gyre_logger.error(f"ERROR: GYRE executable not found at '{gyre_executable}'. Please check the path specified in your config.")
        raise
    except subprocess.CalledProcessError as e:
        gyre_logger.error(f"**[{os.path.basename(output_dir)}] ERROR: GYRE run FAILED**, exit code: **{e.returncode}**!")
        gyre_logger.error(f"Command executed: {' '.join(e.cmd)}")
        gyre_logger.error(f"GYRE stdout:\n{e.stdout}")
        gyre_logger.error(f"GYRE stderr:\n{e.stderr}")
        raise
    except Exception as e:
        gyre_logger.error(f"**[{os.path.basename(output_dir)}] UNEXPECTED ERROR occurred during GYRE run:** {e}", exc_info=True)
        gyre_logger.error("--- Error during run ---")
        raise
    finally:
        # Restore OMP_NUM_THREADS to its original value or unset if it wasn't set before
        if original_omp_num_threads is not None:
            os.environ['OMP_NUM_THREADS'] = original_omp_num_threads
            gyre_logger.debug(f"OMP_NUM_THREADS restored to {original_omp_num_threads}.")
        else:
            if 'OMP_NUM_THREADS' in os.environ:
                del os.environ['OMP_NUM_THREADS']
                gyre_logger.debug("OMP_NUM_THREADS unset.")
        # Clean up the temporary inlist file
        if run_inlist_path and os.path.exists(run_inlist_path):
            os.remove(run_inlist_path)
            gyre_logger.debug(f"Removed temporary GYRE inlist: {run_inlist_path}")


# --- Main GYRE Pipeline Management Function ---
def run_gyre_workflow(
    config_data, # The addict.Dict (or similar) containing all resolved config settings
    filtered_profiles_csv_path: str = None, # Path to the filtered profiles CSV, typically generated by mesa_analyzer
    debug_mode: bool = False
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

    Args:
        config_data (addict.Dict): A dictionary-like object containing all
            configuration settings (e.g., from config.yaml parsed by mesalab).
        filtered_profiles_csv_path (str, optional): The absolute path to the CSV
            file containing filtered MESA profiles. If None, the function will
            create the path based on `config_data.gyre_workflow.filtered_profiles_csv_name`.
        debug_mode (bool, optional): If True, enables verbose debug logging for this module.
            Defaults to False.
    """
    if debug_mode:
        gyre_logger.setLevel(logging.DEBUG)
        gyre_logger.debug("GYRE workflow debug mode enabled.")
    else:
        gyre_logger.setLevel(logging.WARNING) # Default to INFO if not in debug mode

    gyre_logger.info("Initializing GYRE workflow from mesalab configuration...")

    # Extract settings directly from the config_data object
    try:
        gyre_cfg = config_data.gyre_workflow
        general_settings = config_data.general_settings
    except AttributeError as e:
        gyre_logger.critical(f"Missing expected configuration section: {e}. Please check your main config.yaml structure.")
        raise ValueError(f"Configuration structure error: {e}")

    # Validate essential general settings
    required_general_params = ['gyre_dir', 'output_dir', 'input_dir']
    for param in required_general_params:
        if not getattr(general_settings, param, None):
            raise ValueError(f"Missing required parameter '{param}' in the 'general_settings' section of your main config.")

    # Validate essential GYRE workflow settings
    # !!! FIX THIS LINE !!!
    # Change 'gyre_inlist_template_name' to 'gyre_inlist_template_path'
    required_gyre_params = ['run_mode', 'gyre_inlist_template_path', 'num_gyre_threads', 'enable_parallel', 'max_concurrent_gyre_runs']
    for param in required_gyre_params:
        if getattr(gyre_cfg, param, None) is None: # Check for None explicitly
            # Special handling for boolean/int to allow 0 or False
            if param in ['num_gyre_threads', 'max_concurrent_gyre_runs'] and not isinstance(getattr(gyre_cfg, param), (int, float)):
                raise ValueError(f"Missing or invalid required parameter '{param}' in the 'gyre_workflow' section of your main config.")
            elif param == 'enable_parallel' and not isinstance(getattr(gyre_cfg, param), bool):
                raise ValueError(f"Missing or invalid required parameter '{param}' in the 'gyre_workflow' section of your main config.")
            else: # General check for other string/path params
                raise ValueError(f"Missing required parameter '{param}' in the 'gyre_workflow' section of your main config.")

    # --- Setup Paths ---
    gyre_install_dir = general_settings.gyre_dir
    global_output_base_dir = general_settings.output_dir
    global_input_dir = general_settings.input_dir # Top-level MESA grid directory

    # Create a specific output directory for all GYRE results of this session
    gyre_session_output_dir = os.path.join(global_output_base_dir, 'gyre_outputs')
    os.makedirs(gyre_session_output_dir, exist_ok=True)
    gyre_logger.info(f"**GYRE specific outputs will be saved in: '{os.path.abspath(gyre_session_output_dir)}'**")

    # Determine GYRE executable path using shutil.which, as config_paths.py should have set PATH
    gyre_executable = shutil.which("gyre")
    if not gyre_executable:
        gyre_logger.critical(f"GYRE executable not found in system PATH. Please ensure GYRE_DIR ('{gyre_install_dir}') "
                             f"is correctly set in your main config and GYRE is compiled and its 'bin' folder is in PATH.")
        raise FileNotFoundError("GYRE executable not found.")
    gyre_logger.info(f"**GYRE executable found at: '{gyre_executable}'**")

    # !!! FIX THIS BLOCK !!!
    # This block needs to be updated to directly use gyre_cfg.gyre_inlist_template_path
    # and resolve it, instead of assuming it's in mesalab/config/.
    gyre_inlist_template_from_config = gyre_cfg.gyre_inlist_template_path
    gyre_logger.debug(f"GYRE inlist template path from config: {gyre_inlist_template_from_config}")

    # Resolve the full path for the GYRE inlist template.
    # If the path is relative, assume it's relative to the current working directory
    # where the mesalab command is executed.
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
        # Check for required parameters for ALL_PROFILES mode
        if not getattr(gyre_cfg, 'mesa_profile_pattern', None) or \
           not getattr(gyre_cfg, 'mesa_profile_base_dir_relative', None):
            raise ValueError("Missing 'mesa_profile_pattern' or 'mesa_profile_base_dir_relative' in 'gyre_workflow' section for 'ALL_PROFILES' mode.")

        # Construct the absolute path to the MESA LOGS directory
        global_mesa_logs_dir = os.path.join(global_input_dir, gyre_cfg.mesa_profile_base_dir_relative)

        if not os.path.exists(global_mesa_logs_dir):
            raise FileNotFoundError(f"MESA profile base directory for 'ALL_PROFILES' mode not found: '{global_mesa_logs_dir}'. "
                                    f"Please check 'general_settings.input_dir' and 'gyre_workflow.mesa_profile_base_dir_relative' in your main config.")

        # Find all MESA profile files matching the pattern
        all_profile_paths = sorted(glob.glob(os.path.join(global_mesa_logs_dir, gyre_cfg.mesa_profile_pattern)))
        gyre_logger.info(f"**Run mode: ALL_PROFILES.** Found {len(all_profile_paths)} MESA profile(s) to process based on pattern '{gyre_cfg.mesa_profile_pattern}'.")

        if not all_profile_paths:
            gyre_logger.warning(f"**WARNING:** No MESA profile files found to process. Skipping GYRE runs.")
            return

        for profile_path in all_profile_paths:
            profile_id_from_filename = os.path.basename(profile_path).replace('.data.GYRE', '')
            # Create a unique output directory for each profile's GYRE run
            run_output_dir = os.path.join(gyre_session_output_dir, f"all_profiles_run_{profile_id_from_filename}")

            tasks.append((
                profile_path,
                gyre_inlist_template_full_path, # This is now the flexible full path
                run_output_dir,
                gyre_executable,
                gyre_cfg.num_gyre_threads
            ))

    elif run_mode == 'FILTERED_PROFILES':
        # If filtered_profiles_csv_path is not explicitly provided, create it
        if filtered_profiles_csv_path is None:
            if not getattr(gyre_cfg, 'filtered_profiles_csv_name', None):
                raise ValueError("Missing 'filtered_profiles_csv_name' in 'gyre_workflow' section for 'FILTERED_PROFILES' mode.")

            # Assume the CSV is in the analysis_results subdirectory of the main output_dir
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

            # Convert model numbers to int, handle potential NaN after check
            if pd.isna(row['min_model_number']) or pd.isna(row['max_model_number']):
                gyre_logger.warning(f"Skipping row {index} for M={mass}, Z={Z} due to missing (NaN) min_model_number or max_model_number. GYRE requires valid model ranges.")
                continue

            min_model = int(row['min_model_number'])
            max_model = int(row['max_model_number'])

            # The 'mesa_run_directory' in CSV must be an absolute path to the run's top directory
            mesa_run_specific_dir = row['mesa_run_directory']

            # Create the path to the LOGS directory within this specific MESA run
            current_mesa_run_logs_dir = os.path.join(mesa_run_specific_dir, 'LOGS')
            current_profiles_index_path = os.path.join(current_mesa_run_logs_dir, 'profiles.index')

            gyre_logger.info(f"\nProcessing M={mass}, Z={Z} from run directory: {os.path.basename(mesa_run_specific_dir)}")
            gyre_logger.info(f"Searching profiles in: {current_mesa_run_logs_dir} within model range [{min_model}-{max_model}]")

            # Create a map from MESA model number to GYRE profile number using profiles.index
            model_to_profile_map = {}

            if not os.path.exists(current_profiles_index_path):
                gyre_logger.warning(f"**WARNING:** profiles.index not found for M={mass}, Z={Z} at: '{current_profiles_index_path}'. Skipping this M-Z combination.")
                continue

            try:
                # Read profiles.index, skipping header if present
                with open(current_profiles_index_path, 'r') as f:
                    first_line = f.readline().strip()
                    if not re.match(r'^\d', first_line): # Check if first line starts with a digit (i.e., no header)
                        pass # It has a header, just continue
                    else:
                        f.seek(0) # No header, rewind to start

                    for line in f:
                        line = line.strip()
                        if not line or line.startswith('#'): # Skip empty or comment lines
                            continue

                        parts = line.split()
                        if len(parts) < 2:
                            gyre_logger.debug(f"Skipping malformed line (too few columns) in {current_profiles_index_path}: {line.strip()}")
                            continue

                        try:
                            model_num = int(parts[0])
                            profile_num = int(parts[-1]) # Assuming profile number is the last column
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

            # Select profile numbers that fall within the specified model range
            selected_profile_numbers_for_this_run = set()
            for model_num_in_index, profile_num_in_index in model_to_profile_map.items():
                if min_model <= model_num_in_index <= max_model:
                    selected_profile_numbers_for_this_run.add(profile_num_in_index)

            if not selected_profile_numbers_for_this_run:
                gyre_logger.warning(f"**WARNING:** No MESA profiles found for M={mass}, Z={Z} within model range {min_model}-{max_model}. Skipping.")
                continue

            # Create a specific output directory for this MESA run's GYRE results
            # The structure is: gyre_outputs/mesa_run_name/profileXXXXX/
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
                    # The run_output_dir is where the temporary inlist and GYRE's own outputs will go
                    run_output_dir = os.path.join(specific_model_output_root, f'profile{prof_num:05d}')

                    tasks.append((
                        profile_path,
                        gyre_inlist_template_full_path, # This is now the flexible full path
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
        gyre_logger.warning(f"**WARNING:** No GYRE tasks were prepared. Skipping GYRE runs.")
    else:
        max_concurrent_runs = gyre_cfg.max_concurrent_gyre_runs

        if gyre_cfg.enable_parallel:
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

# --- Standalone execution for testing/debugging ---
if __name__ == "__main__":
    # This block allows running gyre_modules.py directly for testing.
    # It mimics how config_data would be passed from cli.py.
    # For a real mesalab run, cli.py would call run_gyre_workflow with a populated config_data object.

    # We need a dummy config_data object for standalone testing.
    # In a real mesalab run, this would be an addict.Dict or similar from the main config.yaml.
    class DummyConfigDict(dict):
        def __getattr__(self, name):
            try:
                return self[name]
            except KeyError:
                raise AttributeError(f"Config attribute '{name}' not found.")
        def __setattr__(self, name, value):
            self[name] = value

    import argparse
    parser = argparse.ArgumentParser(description="Run the GYRE pipeline with a specified configuration file.")
    parser.add_argument('main_config_path', type=str,
                        help="Path to the main mesalab configuration YAML file (e.g., 'config.yaml').")
    parser.add_argument('--filtered_profiles_csv_path', type=str, default=None,
                        help="Path to the filtered profiles CSV for 'FILTERED_PROFILES' mode (overrides config).")
    parser.add_argument('--debug', action='store_true', help='Enable debug logging for standalone run.')

    args = parser.parse_args()

    # Setup basic logger if not already configured (e.g., by main mesalab application)
    if not gyre_logger.handlers:
        gyre_logger.setLevel(logging.DEBUG if args.debug else logging.WARNING)
        handler = logging.StreamHandler(sys.stdout)
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        handler.setFormatter(formatter)
        gyre_logger.addHandler(handler)

    # Load the main config.yaml using a simple YAML parser
    import yaml
    try:
        with open(args.main_config_path, 'r') as f:
            raw_config = yaml.safe_load(f)
        config_data_for_test = DummyConfigDict(raw_config)
    except FileNotFoundError:
        gyre_logger.critical(f"Main config YAML file '{args.main_config_path}' not found for standalone test.")
        sys.exit(1)
    except yaml.YAMLError as e:
        gyre_logger.critical(f"Error parsing main config YAML file '{args.main_config_path}': {e}")
        sys.exit(1)
    except Exception as e:
        gyre_logger.critical(f"An unexpected error occurred loading main config: {e}")
        sys.exit(1)

    try:
        run_gyre_workflow(
            config_data=config_data_for_test,
            filtered_profiles_csv_path=args.filtered_profiles_csv_path,
            debug_mode=args.debug
        )
    except (FileNotFoundError, ValueError, IOError, subprocess.CalledProcessError) as e:
        gyre_logger.critical(f"\n**[Critical Error]:** {e}")
        sys.exit(1)
    except Exception as e:
        gyre_logger.critical(f"\n**[Unexpected Error]:** An unexpected error occurred: {e}", exc_info=True)
        sys.exit(1)