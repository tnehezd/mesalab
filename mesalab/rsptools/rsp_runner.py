import os
import subprocess
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
import time
from tqdm import tqdm
import sys
import shutil

# Set logging level at the start of the script.
# This will suppress INFO messages but allow WARNING, ERROR, and CRITICAL
# to be shown, unless overridden by a different logging configuration.
logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

def run_mesa_rsp_single(inlist_path: str, mesa_binary_dir: str, num_threads: int, output_dir: str) -> dict:
    """
    Runs the MESA RSP module with a single inlist file.

    Args:
        inlist_path (str): The full path to the inlist_rsp file.
        mesa_binary_dir (str): The directory where the 'star' executable is located.
        num_threads (int): The number of OpenMP threads to use for this run.
        output_dir (str): The dedicated output directory for this specific run.
                          All log files and outputs will be stored here.

    Returns:
        dict: A dictionary containing the run status (success/failure) and
              other relevant information.
    """
    # Create the run directory based on the provided output_dir
    os.makedirs(output_dir, exist_ok=True)
    
    # MESA 'star' requires the inlist file to be in the current working directory.
    # We copy the inlist to the dedicated output directory for this run.
    try:
        shutil.copy(inlist_path, output_dir)
        run_dir_for_subprocess = output_dir
        inlist_name = os.path.basename(inlist_path)
    except shutil.Error as e:
        logger.error(f"Failed to copy inlist file to {output_dir}: {e}")
        return {'status': 'failed', 'inlist': inlist_path, 'error': f"Failed to copy inlist to output directory.", 'duration': 0}
    
    # Construct the full path to the MESA 'star' executable
    mesa_exe_path = os.path.join(mesa_binary_dir, 'star')

    # Set the OMP_NUM_THREADS environment variable for this subprocess call
    env_vars = os.environ.copy()
    env_vars['OMP_NUM_THREADS'] = str(num_threads)
    logger.debug(f"Setting OMP_NUM_THREADS to {num_threads} for run in {run_dir_for_subprocess}")

    if not os.path.exists(mesa_exe_path):
        logger.error(f"MESA executable ('star') not found at: {mesa_exe_path}. This is critical for RSP run.")
        return {'status': 'failed', 'inlist': inlist_path, 'error': "MESA executable 'star' not found at expected path."}

    start_time = time.time()
    try:
        # Run the MESA 'star' executable within the dedicated output directory.
        result = subprocess.run(
            [mesa_exe_path, inlist_name],
            cwd=run_dir_for_subprocess,
            capture_output=True,
            text=True,
            check=True,
            timeout=900,  # 15 minutes timeout
            env=env_vars  # Pass the modified environment variables
        )
        end_time = time.time()
        duration = end_time - start_time
        return {'status': 'successful', 'inlist': inlist_path, 'duration': duration}
    except subprocess.CalledProcessError as e:
        end_time = time.time()
        duration = end_time - start_time
        logger.error(f"RSP run error for {inlist_path} (duration: {duration:.2f} sec). Return code: {e.returncode}. Stderr: {e.stderr}")
        return {'status': 'failed', 'inlist': inlist_path, 'error': e.stderr, 'duration': duration}
    except subprocess.TimeoutExpired as e:
        end_time = time.time()
        duration = end_time - start_time
        logger.warning(f"RSP run timeout for {inlist_path} (duration: {duration:.2f} sec). Stderr: {e.stderr if e.stderr else 'No stderr output'}")
        return {'status': 'timeout', 'inlist': inlist_path, 'error': "Timeout", 'duration': duration}
    except Exception as e:
        end_time = time.time()
        duration = end_time - start_time
        logger.error(f"Unexpected error during RSP run for {inlist_path} (duration: {duration:.2f} sec): {e}")
        return {'status': 'error', 'inlist': inlist_path, 'error': str(e), 'duration': duration}

def run_mesa_rsp_workflow(
    inlist_paths: list[str],
    config_data: dict,
    rsp_output_subdir: str
) -> dict:
    """
    Runs the MESA RSP workflow on all provided inlist files, in parallel or sequentially.

    Args:
        inlist_paths (list[str]): A list of full paths to the generated inlist_rsp files.
        config_data (dict): The full configuration object (addict.Dict) containing all paths and settings.
        rsp_output_subdir (str): The base output directory for all RSP runs.

    Returns:
        dict: A summary dictionary of the run results.
              Example: {'successful': [...], 'failed': [...], 'timeout': [...], 'error': [...]
    """
    if not inlist_paths:
        logger.warning("No RSP inlist files provided for execution.")
        return {'successful': [], 'failed': [], 'timeout': [], 'error': []}

    mesa_binary_dir = config_data.general_settings.get('mesa_binary_dir')
    if not mesa_binary_dir or not os.path.isdir(mesa_binary_dir) or not os.path.exists(os.path.join(mesa_binary_dir, 'star')):
        logger.critical(f"ERROR: Configured 'mesa_binary_dir' ('{mesa_binary_dir}') is invalid or 'star' not found within it. Cannot run RSP workflow.")
        return {'successful': [], 'failed': [], 'timeout': [], 'error': [], 'initial_error': 'invalid_mesa_binary_dir'}

    enable_parallel = config_data.rsp_workflow.get('enable_rsp_parallel', False)
    max_workers = config_data.rsp_workflow.get('max_concurrent_rsp_runs', os.cpu_count() or 1)
    num_threads = config_data.rsp_workflow.get('num_rsp_threads', 1)
    total_runs = len(inlist_paths)
    
    results = {'successful': [], 'failed': [], 'timeout': [], 'error': []}

    print(f"Starting MESA RSP workflow for {total_runs} inlist files.")
    
    # Generate unique output directories for each run
    tasks = []
    for path in inlist_paths:
        # Create a unique directory name based on the inlist file name
        base_name = os.path.basename(path).replace('inlist_rsp', '').replace('.data', '').replace('__', '_').strip('_')
        if not base_name:
            # Fallback for simple 'inlist_rsp' filename
            base_name = f"run_{os.path.basename(os.path.dirname(path))}"
        
        output_dir = os.path.join(rsp_output_subdir, base_name)
        tasks.append((path, mesa_binary_dir, num_threads, output_dir))
    
    if enable_parallel:
        print(f"Parallel mode enabled. Using a maximum of {max_workers} concurrent processes, each with {num_threads} thread(s).")
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            future_to_inlist = {executor.submit(run_mesa_rsp_single, *task): task[0] for task in tasks}

            for future in tqdm(as_completed(future_to_inlist), total=total_runs, desc="MESA RSP Workflow"):
                inlist_path = future_to_inlist[future]
                try:
                    result = future.result()
                    if isinstance(result, dict) and 'status' in result:
                        results[result['status']].append(result)
                    else:
                        logger.error(f"Invalid result format from {inlist_path}: {result}")
                        results['error'].append({'status': 'error', 'inlist': inlist_path, 'error': 'Invalid result format', 'raw_result': str(result)})
                except Exception as exc:
                    logger.error(f'Unexpected exception during execution for {inlist_path}: {exc}')
                    results['error'].append({'status': 'error', 'inlist': inlist_path, 'error': str(exc)})
    else:
        print(f"Sequential mode enabled. Using {num_threads} thread(s) for each run.")
        for task in tqdm(tasks, total=total_runs, desc="MESA RSP Workflow"):
            try:
                result = run_mesa_rsp_single(*task)
                if isinstance(result, dict) and 'status' in result:
                    results[result['status']].append(result)
                else:
                    logger.error(f"Invalid result format from {task[0]}: {result}")
                    results['error'].append({'status': 'error', 'inlist': task[0], 'error': 'Invalid result format', 'raw_result': str(result)})
            except Exception as exc:
                logger.error(f'Unexpected exception during sequential execution for {task[0]}: {exc}')
                results['error'].append({'status': 'error', 'inlist': task[0], 'error': str(exc)})

    successful_count = len(results['successful'])
    failed_count = len(results['failed'])
    timeout_count = len(results['timeout'])
    error_count = len(results['error'])
    
    print("--- MESA RSP Workflow Summary ---")
    print(f"Total runs: {total_runs}")
    print(f"Successful runs: {successful_count}")
    print(f"Failed runs: {failed_count}")
    print(f"Timed out runs: {timeout_count}")
    print(f"Runs with unexpected errors: {error_count}")
    print("---------------------------------")
    
    return results

# This __name__ == "__main__" block is for testing purposes only.
if __name__ == "__main__":
    logger.warning("Example run from rsp_workflow.py for local testing.")
    
    from addict import Dict
    
    base_test_dir = "./temp_rsp_test_runs"
    os.makedirs(base_test_dir, exist_ok=True)
    example_inlist_paths = []
    
    for i in range(5):
        run_dir = os.path.join(base_test_dir, f"test_run_{i}")
        os.makedirs(run_dir, exist_ok=True)
        inlist_file = os.path.join(run_dir, "inlist_rsp")
        with open(inlist_file, "w") as f:
            f.write(f"&inlist_rsp {i+1} /\n")
        example_inlist_paths.append(inlist_file)
    logger.warning(f"Created {len(example_inlist_paths)} dummy inlist files for testing.")
    
    your_mesa_binary_dir = "/path/to/your/mesa/star/work"

    if not os.path.isdir(your_mesa_binary_dir):
        logger.error(f"ERROR: 'your_mesa_binary_dir' ('{your_mesa_binary_dir}') is not a valid directory. "
                     "Please set it to the correct path of your MESA 'star/work' directory for testing!")
        sys.exit(1)
    
    if not os.path.exists(os.path.join(your_mesa_binary_dir, 'star')):
        logger.error(f"ERROR: 'star' executable not found in 'your_mesa_binary_dir' ('{your_mesa_binary_dir}'). "
                     "Please ensure MESA's 'star' module has been compiled and 'star' exists in this directory for testing.")
        sys.exit(1)
    
    class MockGeneralSettings:
        def __init__(self, mesa_binary_dir, output_dir):
            self._data = {'mesa_binary_dir': mesa_binary_dir, 'output_dir': output_dir}
        def get(self, key, default=None):
            return self._data.get(key, default)
    
    class MockRSPWorkflow:
        def __init__(self, run_rsp_workflow, enable_rsp_parallel, max_concurrent_rsp_runs, num_rsp_threads):
            self._data = {
                'run_rsp_workflow': run_rsp_workflow,
                'enable_rsp_parallel': enable_rsp_parallel,
                'max_concurrent_rsp_runs': max_concurrent_rsp_runs,
                'num_rsp_threads': num_rsp_threads
            }
        def get(self, key, default=None):
            return self._data.get(key, default)

    class MockConfigData:
        def __init__(self, mesa_binary_dir, output_dir, enable_parallel, max_workers, num_threads):
            self.general_settings = MockGeneralSettings(mesa_binary_dir, output_dir)
            self.rsp_workflow = MockRSPWorkflow(True, enable_parallel, max_workers, num_threads)

    rsp_output_dir_test = os.path.join(base_test_dir, 'rsp_outputs_test')
    os.makedirs(rsp_output_dir_test, exist_ok=True)

    # Example 1: Parallel run with 2 workers
    print("\n--- Running Parallel Test (2 workers) ---")
    mock_config_parallel = MockConfigData(your_mesa_binary_dir, base_test_dir, enable_parallel=True, max_workers=2, num_threads=1)
    rsp_results_parallel = run_mesa_rsp_workflow(example_inlist_paths, mock_config_parallel, rsp_output_subdir=rsp_output_dir_test)
    
    # Example 2: Sequential run with 1 thread
    print("\n--- Running Sequential Test (1 worker, 1 thread) ---")
    mock_config_sequential = MockConfigData(your_mesa_binary_dir, base_test_dir, enable_parallel=False, max_workers=1, num_threads=1)
    rsp_results_sequential = run_mesa_rsp_workflow(example_inlist_paths, mock_config_sequential, rsp_output_subdir=rsp_output_dir_test)

    if os.path.exists(base_test_dir):
        logger.warning(f"Cleaning up dummy test directory: {base_test_dir}")
        shutil.rmtree(base_test_dir)