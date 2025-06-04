#mesa_tools/data_reader.py

import pandas as pd
import numpy as np
import re
import os
import logging # Import the logging module

# Configure logging for this module.
# In a real application, you'd usually configure logging once in your main script (e.g., cli.py)
# to apply across all modules. For demonstration, it's here.
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')


def extract_params_from_inlist(inlist_path):
    """
    Extract initial_mass and initial_Z from a MESA inlist file.

    Args:
        inlist_path (str): The absolute path to the MESA inlist file.

    Returns:
        tuple: A tuple containing (mass, z) as floats. Returns (None, None)
               if parameters cannot be extracted or file is not found.
    """
    mass = None
    z = None

    try:
        with open(inlist_path, 'r') as f:
            content = f.read()
            # Regex to find initial_mass (allows integers and floats)
            mass_match = re.search(r'initial_mass\s*=\s*(\d+\.?\d*)', content)
            if mass_match:
                mass = float(mass_match.group(1))
            
            # Regex to find initial_Z (allows integers, floats, and scientific notation)
            z_match = re.search(r'initial_Z\s*=\s*(\d+\.?\d*(?:e[+\-]?\d+)?)', content)
            if z_match:
                z = float(z_match.group(1))
    except FileNotFoundError:
        # Using logging.error for critical issues
        logging.error(f"Inlist file not found: {inlist_path}")
    except Exception as e:
        logging.error(f"Error reading inlist file {inlist_path}: {e}")

    return mass, z


def scan_mesa_runs(input_dir, inlist_name):
    """
    Scan the input directory for MESA run subdirectories.
    This function looks for any direct subdirectories within `input_dir`
    that contain both the specified inlist file and the history.data file.

    Args:
        input_dir (str): The absolute path to the main directory containing MESA run subdirectories.
        inlist_name (str): The filename of the inlist (e.g., 'inlist_project') expected within each run directory.

    Returns:
        list: A list of dictionaries, where each dictionary contains:
              - 'history_file_path': Path to history.data
              - 'run_dir_path': Path to the MESA run directory
              - 'mass': Extracted initial_mass
              - 'z': Extracted initial_Z
              Returns an empty list if no valid runs are found.
    """
    mesa_run_infos = []
    
    # Get all direct subdirectories within the input_dir.
    # This makes the scanning more flexible, not tied to a specific prefix like 'run_*'.
    # It only considers directories, not files.
    potential_run_dir_names = [d for d in os.listdir(input_dir)
                               if os.path.isdir(os.path.join(input_dir, d))]

    if not potential_run_dir_names:
        logging.warning(f"No subdirectories found directly in '{input_dir}'. "
                        "Ensure your MESA runs are in individual folders within this input directory.")
        return []

    logging.info(f"Scanning '{input_dir}' for MESA run directories...")

    for run_dir_name in potential_run_dir_names:
        run_dir_path = os.path.join(input_dir, run_dir_name)
        inlist_path = os.path.join(run_dir_path, inlist_name)
        history_file_path = os.path.join(run_dir_path, 'LOGS', 'history.data')

        # Check for the existence of both required files: inlist and history.data
        if os.path.exists(inlist_path) and os.path.exists(history_file_path):
            mass, z = extract_params_from_inlist(inlist_path)
            if mass is not None and z is not None:
                mesa_run_infos.append({
                    'history_file_path': history_file_path,
                    'run_dir_path': run_dir_path,
                    'mass': mass,
                    'z': z
                })
                logging.info(f"Found valid MESA run in '{run_dir_path}'.")
            else:
                logging.warning(f"Could not extract mass/Z from inlist '{inlist_path}'. Skipping this run.")
        else:
            # Provide specific reasons why a directory is being skipped
            if not os.path.exists(inlist_path):
                logging.info(f"Inlist file '{inlist_name}' not found in '{run_dir_path}'. Skipping this directory.")
            if not os.path.exists(history_file_path):
                logging.info(f"history.data not found at '{history_file_path}'. Skipping this directory.")
            
    logging.info(f"Finished scanning. Found {len(mesa_run_infos)} valid MESA runs.")

    return mesa_run_infos


def get_data_from_history_file(history_file_path):
    """
    Reads a MESA history.data file into a pandas DataFrame using np.genfromtxt.

    This function is designed to handle the structure of MESA history.data files,
    including comments and header rows, and converts data to appropriate types.

    Args:
        history_file_path (str): The absolute path to the MESA history.data file.

    Returns:
        pandas.DataFrame: A DataFrame containing the history data.

    Raises:
        Exception: If there's an error loading or processing the history file.
    """
    try:
        # Use np.genfromtxt for robust parsing of MESA history files.
        # names=True reads column headers from the file.
        # comments='#' ignores lines starting with #.
        # skip_header=5 skips initial descriptive lines before headers.
        # dtype=None infers data types.
        # encoding='utf-8' for universal compatibility.
        data = np.genfromtxt(history_file_path, names=True, comments="#", skip_header=5,
                             dtype=None, encoding='utf-8')

        # Handle edge case where genfromtxt reads a single row as a 0-D array
        if data.ndim == 0:
            df = pd.DataFrame([data.tolist()], columns=data.dtype.names)
        else:
            df = pd.DataFrame(data)

        # Convert all columns to numeric, coercing unconvertible values to NaN.
        # This is a robust way to ensure numerical operations don't fail on mixed data.
        for col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')

        # Ensure 'model_number' is an integer type if present and valid.
        if 'model_number' in df.columns:
            # Drop rows where model_number might be NaN (e.g., due to parsing errors)
            df.dropna(subset=['model_number'], inplace=True)
            # Convert to integer type only if no NaNs remain, otherwise it raises an error
            if not df['model_number'].isnull().any(): 
                df['model_number'] = df['model_number'].astype(int)

        return df

    except Exception as e:
        # Re-raise with more context to help debugging upstream
        raise type(e)(f"Error loading or processing {history_file_path} using np.genfromtxt: {e}") from e