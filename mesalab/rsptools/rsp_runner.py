import os
import subprocess
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
import time

logger = logging.getLogger(__name__)

def run_mesa_rsp_single(inlist_path: str, mesa_binary_dir: str) -> dict:
    """
    Runs the MESA RSP module with a single inlist file.

    Args:
        inlist_path (str): The full path to the inlist_rsp file.
        mesa_binary_dir (str): The directory where the 'rn' executable is located
                               (e.g., '/path/to/mesa/star/work/').
                               This is crucial for finding the executable.

    Returns:
        dict: A dictionary containing the run status (success/failure) and
              other relevant information.
    """
    run_dir = os.path.dirname(inlist_path) # The directory of the inlist file will be the run directory
    
    # Construct the full path to the MESA 'rn' executable
    mesa_exe_path = os.path.join(mesa_binary_dir, 'rn')

    # Validate that the executable actually exists at the constructed path
    if not os.path.exists(mesa_exe_path):
        logger.error(f"MESA executable ('rn') not found at: {mesa_exe_path}. This is critical for RSP run.")
        return {'status': 'failed', 'inlist': inlist_path, 'error': f"MESA executable 'rn' not found at expected path."}

    logger.info(f"Starting RSP run for: {inlist_path} in directory: {run_dir}. Using executable: {mesa_exe_path}")
    start_time = time.time()
    try:
        # We run MESA from the inlist_path directory.
        # subprocess.run with check=True will raise an error for non-zero exit codes.
        result = subprocess.run(
            [mesa_exe_path, 'inlist_rsp'], # Command: /path/to/star/work/rn inlist_rsp
            cwd=run_dir,                   # The working directory for the command
            capture_output=True,           # Capture stdout and stderr
            text=True,                     # Treat output as text
            check=True,                    # Raise CalledProcessError for non-zero exit codes
            timeout=300                    # 5-minute timeout for each run
        )
        end_time = time.time()
        duration = end_time - start_time
        logger.info(f"Successful RSP run: {inlist_path} (duration: {duration:.2f} sec)")
        return {'status': 'success', 'inlist': inlist_path, 'duration': duration}
    except subprocess.CalledProcessError as e:
        end_time = time.time()
        duration = end_time - start_time
        logger.error(f"RSP run error for {inlist_path} (duration: {duration:.2f} sec). Command: '{' '.join(e.cmd)}'. Return code: {e.returncode}. Stderr: {e.stderr}")
        return {'status': 'failed', 'inlist': inlist_path, 'error': e.stderr, 'duration': duration}
    except subprocess.TimeoutExpired as e:
        end_time = time.time()
        duration = end_time - start_time
        logger.error(f"RSP run timeout for {inlist_path} (duration: {duration:.2f} sec). Command: '{' '.join(e.cmd)}'. Stderr: {e.stderr if e.stderr else 'No stderr output'}")
        return {'status': 'timeout', 'inlist': inlist_path, 'error': "Timeout", 'duration': duration}
    except Exception as e:
        end_time = time.time()
        duration = end_time - start_time
        logger.error(f"Unexpected error during RSP run for {inlist_path} (duration: {duration:.2f} sec): {e}")
        return {'status': 'error', 'inlist': inlist_path, 'error': str(e), 'duration': duration}


def run_mesa_rsp_workflow(
    inlist_paths: list[str],
    config_data: dict, # Now expects the full config_data object
    max_workers: int = os.cpu_count() or 1
) -> dict:
    """
    Runs the MESA RSP workflow on all provided inlist files, in parallel.

    Args:
        inlist_paths (list[str]): A list of full paths to the generated inlist_rsp files.
        config_data (dict): The full configuration object (addict.Dict) containing all paths.
        max_workers (int): The maximum number of parallel threads to use.
                           Defaults to the number of CPU cores.

    Returns:
        dict: A summary dictionary of the run results.
              Example: {'successful': [...], 'failed': [...], 'timeout': [...], 'error': [...]}
    """
    if not inlist_paths:
        logger.warning("No RSP inlist files provided for execution.")
        return {'successful': [], 'failed': [], 'timeout': [], 'error': []}

    # Retrieve mesa_binary_dir from config_data
    mesa_binary_dir = config_data.general_settings.get('mesa_binary_dir')
    if not mesa_binary_dir:
        logger.critical("ERROR: 'mesa_binary_dir' is not set in configuration. Cannot run RSP workflow.")
        return {'successful': [], 'failed': [], 'timeout': [], 'error': [], 'initial_error': 'mesa_binary_dir not set'}
    
    # Basic validation for mesa_binary_dir here, though config_paths.py should have done a more thorough job.
    # This is a final safeguard before spawning threads.
    if not os.path.isdir(mesa_binary_dir) or not os.path.exists(os.path.join(mesa_binary_dir, 'rn')):
        logger.critical(f"ERROR: Configured 'mesa_binary_dir' ('{mesa_binary_dir}') is invalid or 'rn' not found within it. Cannot run RSP workflow.")
        return {'successful': [], 'failed': [], 'timeout': [], 'error': [], 'initial_error': 'invalid_mesa_binary_dir'}


    logger.info(f"Starting MESA RSP workflow for {len(inlist_paths)} inlist files, "
                f"using a maximum of {max_workers} parallel workers. MESA binary directory: {mesa_binary_dir}")
    
    results = {'successful': [], 'failed': [], 'timeout': [], 'error': []}
    
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        # Submit tasks for each inlist file to the thread pool.
        # Pass the correctly resolved mesa_binary_dir to each single run.
        future_to_inlist = {executor.submit(run_mesa_rsp_single, path, mesa_binary_dir): path for path in inlist_paths}

        # Process results as they complete
        for future in as_completed(future_to_inlist):
            inlist_path = future_to_inlist[future]
            try:
                result = future.result()
                if result['status'] == 'success':
                    results['successful'].append(result)
                elif result['status'] == 'failed':
                    results['failed'].append(result)
                elif result['status'] == 'timeout':
                    results['timeout'].append(result)
                else: # 'error' or any other unexpected status
                    results['error'].append(result)
            except Exception as exc:
                logger.error(f'{inlist_path} generated an unexpected exception during execution: {exc}')
                results['error'].append({'status': 'error', 'inlist': inlist_path, 'error': str(exc)})

    total_runs = len(inlist_paths)
    successful_count = len(results['successful'])
    failed_count = len(results['failed'])
    timeout_count = len(results['timeout'])
    error_count = len(results['error'])

    logger.info("--- MESA RSP Workflow Summary ---")
    logger.info(f"Total runs: {total_runs}")
    logger.info(f"Successful runs: {successful_count}")
    logger.info(f"Failed runs: {failed_count}")
    logger.info(f"Timed out runs: {timeout_count}")
    logger.info(f"Runs with unexpected errors: {error_count}")
    logger.info("---------------------------------")

    return results

# This __name__ == "__main__" block is for testing purposes only.
# You can remove or modify it once you integrate the functions into your main script.
if __name__ == "__main__":
    logger.info("Example run from rsp_workflow.py for local testing.")

    # >>> IMPORTANT: REPLACE WITH YOUR ACTUAL INLIST PATHS! <<<
    # These would typically be returned by your generate_mesa_rsp_inlists function.
    # Create some dummy inlist files and directories for this test to work
    base_test_dir = "./temp_rsp_test_runs"
    os.makedirs(base_test_dir, exist_ok=True)
    example_inlist_paths = []
    # Create some dummy run directories and inlist files
    for i in range(2):
        run_dir = os.path.join(base_test_dir, f"test_run_{i}")
        os.makedirs(run_dir, exist_ok=True)
        inlist_file = os.path.join(run_dir, "inlist_rsp")
        with open(inlist_file, "w") as f:
            f.write(f"&inlist_rsp {i+1} /\n") # Minimal content
        example_inlist_paths.append(inlist_file)
    logger.info(f"Created {len(example_inlist_paths)} dummy inlist files for testing.")
    
    # >>> IMPORTANT: REPLACE WITH YOUR ACTUAL MESA INSTALLATION PATH! <<<
    # This is the directory where the MESA 'rn' executable is located (e.g., /path/to/mesa/star/work)
    # Based on your previous output, this should be:
    your_mesa_binary_dir = "/Users/tnehezd/Documents/Munka/MESA230501/mesa-r23.05.01/star/work"

    if not os.path.isdir(your_mesa_binary_dir):
        logger.error(f"ERROR: 'your_mesa_binary_dir' ('{your_mesa_binary_dir}') is not a valid directory. "
                     "Please set it to the correct path of your MESA 'star/work' directory for testing!")
        sys.exit(1)
    
    if not os.path.exists(os.path.join(your_mesa_binary_dir, 'rn')):
        logger.error(f"ERROR: 'rn' executable not found in 'your_mesa_binary_dir' ('{your_mesa_binary_dir}'). "
                     "Please ensure MESA's 'star' module has been compiled and 'rn' exists in this directory for testing.")
        sys.exit(1)

    # Mocking a simplified config_data object for direct testing of this module
    class MockGeneralSettings:
        def __init__(self, mesa_binary_dir):
            self._data = {'mesa_binary_dir': mesa_binary_dir}
        def get(self, key, default=None):
            return self._data.get(key, default)
    
    class MockConfigData:
        def __init__(self, mesa_binary_dir):
            self.general_settings = MockGeneralSettings(mesa_binary_dir)
            self.rsp_workflow = {'run_rsp_workflow': True} # For run_mesa_rsp_workflow validation
    
    mock_config = MockConfigData(your_mesa_binary_dir)

    rsp_results = run_mesa_rsp_workflow(example_inlist_paths, mock_config, max_workers=2) # Use 2 workers for example

    logger.info("\n--- Detailed Results ---")
    for status_type, results_list in rsp_results.items():
        if isinstance(results_list, list): # Ensure it's a list before iterating
            for res in results_list:
                logger.info(f"  {status_type.upper()}: {res.get('inlist', 'N/A')} (Status: {res.get('status', 'N/A')}, Time: {res.get('duration', 'N/A'):.2f} sec, Error: {res.get('error', 'N/A')})")
        else: # For initial_error if present
            logger.info(f"  {status_type.upper()}: {results_list}")

    # Clean up dummy files
    import shutil
    if os.path.exists(base_test_dir):
        logger.info(f"Cleaning up dummy test directory: {base_test_dir}")
        shutil.rmtree(base_test_dir)