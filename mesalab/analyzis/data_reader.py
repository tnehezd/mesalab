import pandas as pd
import numpy as np # Import numpy for genfromtxt
import re
import os
import logging
# from mesalab.io.inlist_parser import get_mesa_params_from_inlist # This might be used if 'extract_params_from_inlist' is replaced

logger = logging.getLogger(__name__) # Use a logger specific to this module

def extract_params_from_inlist(inlist_path):
    """
    Extract `initial_mass` and `initial_Z` values from a MESA inlist file.

    Handles Fortran-style scientific notation (e.g., `1.0d-2`, `2.5D+1`) by
    converting it to Python-compatible form. Returns `None` for missing values,
    with warnings logged.

    Args:
        inlist_path (str): Absolute path to the MESA inlist file.

    Returns:
        tuple: A tuple (mass, z) where both elements are floats or `None`
               if the corresponding parameter is not found.

    Example:
        >>> from mesalab.analyzis import extract_params_from_inlist
        >>> extract_params_from_inlist("mesa_runs/z0.006_m3.0/inlist_project")
        (3.0, 0.006)

        >>> extract_params_from_inlist("inlist_with_missing_z")
        (2.5, None)
    """
    mass = None
    z = None

    try:
        with open(inlist_path, 'r') as f:
            content = f.read()

            # Regex for initial_mass: allows integers, floats, and scientific notation (e or d)
            # Example: initial_mass = 1.0, initial_mass = 1.0d0, initial_mass = 20.5e-1
            # Using re.MULTILINE to match '^' at the start of each line
            # Using re.IGNORECASE for case-insensitivity (initial_mass vs initial_Mass)
            mass_match = re.search(r'^\s*initial_mass\s*=\s*(\d+\.?\d*(?:[deDE][+\-]?\d+)?)\s*(?:!.*)?$', content, re.MULTILINE | re.IGNORECASE)
            if mass_match:
                # Replace 'd' or 'D' with 'e' before converting to float to handle Fortran-style notation
                mass = float(mass_match.group(1).replace('d', 'e').replace('D', 'E'))

            # Regex for initial_Z: allows integers, floats, and scientific notation (e or d)
            # Example: initial_Z = 0.02, initial_Z = 2.0d-2, initial_Z = 1.0e-4
            z_match = re.search(r'^\s*initial_Z\s*=\s*(\d+\.?\d*(?:[deDE][+\-]?\d+)?)\s*(?:!.*)?$', content, re.MULTILINE | re.IGNORECASE)
            if z_match:
                z = float(z_match.group(1).replace('d', 'e').replace('D', 'E'))

    except FileNotFoundError:
        logger.error(f"Inlist file not found: {inlist_path}")
    except Exception as e:
        logger.error(f"Error reading inlist file {inlist_path}: {e}")

    # Log warnings if parameters are not found, even if no file error occurred
    if mass is None:
        logger.warning(f"Could not find 'initial_mass' in {inlist_path}.")
    if z is None:
        logger.warning(f"Could not find 'initial_Z' in {inlist_path}.")

    return mass, z


def scan_mesa_runs(input_dir, inlist_name):
    """
    Scan a directory for MESA run subdirectories that contain both an inlist file and history.data.

    Each valid subdirectory must:
      - Be a direct (non-hidden) subdirectory of `input_dir`
      - Contain the specified `inlist_name` file
      - Contain a LOGS/history.data file

    The function attempts to extract `initial_mass` and `initial_Z` from each inlist file.
    Only runs with both values present are included in the output.

    Args:
        input_dir (str): Absolute path to the main directory containing MESA run subdirectories.
        inlist_name (str): Name of the inlist file expected in each subdirectory (e.g., 'inlist_project').

    Returns:
        list of dict: Each dictionary represents a valid MESA run and contains:
            - 'history_file_path' (str): Full path to the history.data file
            - 'run_dir_path' (str): Full path to the MESA run directory
            - 'mass' (float): Extracted initial_mass
            - 'z' (float): Extracted initial_Z

        Returns an empty list if no valid runs are found.

    Example:
        >>> from mesalab.analyzis import scan_mesa_runs 
        >>> scan_mesa_runs("/home/user/mesa_runs", "inlist_project")
        [
            {
                'history_file_path': '/home/user/mesa_runs/z0.006_m3.0/LOGS/history.data',
                'run_dir_path': '/home/user/mesa_runs/z0.006_m3.0',
                'mass': 3.0,
                'z': 0.006
            },
            {
                'history_file_path': '/home/user/mesa_runs/z0.014_m2.5/LOGS/history.data',
                'run_dir_path': '/home/user/mesa_runs/z0.014_m2.5',
                'mass': 2.5,
                'z': 0.014
            }
        ]
    """
    mesa_run_infos = []

    # Get all direct subdirectories, filtering out hidden ones (like .mesa_temp_cache)
    potential_run_dir_names = [d for d in os.listdir(input_dir)
                               if os.path.isdir(os.path.join(input_dir, d))
                               and not d.startswith('.')] # Filter out hidden directories

    if not potential_run_dir_names:
        logger.warning(f"No non-hidden subdirectories found directly in '{input_dir}'. "
                        "Ensure your MESA runs are in individual folders within this input directory.")
        return []

    logger.info(f"Scanning '{input_dir}' for MESA run directories...")

    for run_dir_name in potential_run_dir_names:
        run_dir_path = os.path.join(input_dir, run_dir_name)
        inlist_path = os.path.join(run_dir_path, inlist_name)
        history_file_path = os.path.join(run_dir_path, 'LOGS', 'history.data')

        # Check for existence of both inlist and history.data
        if os.path.exists(inlist_path) and os.path.exists(history_file_path):
            mass, z = extract_params_from_inlist(inlist_path) # Using the internal function here
            if mass is not None and z is not None:
                mesa_run_infos.append({
                    'history_file_path': history_file_path,
                    'run_dir_path': run_dir_path,
                    'mass': mass,
                    'z': z
                })
            else:
                logger.warning(f"Could not extract mass/Z from inlist '{inlist_path}'. Skipping this run.")
        else:
            # Log reasons for skipping
            if not os.path.exists(inlist_path):
                logger.info(f"Inlist file '{inlist_name}' not found in '{run_dir_path}'. Skipping this directory.")
            if not os.path.exists(history_file_path):
                logger.info(f"history.data not found at '{history_file_path}'. Skipping this directory.")

    logger.info(f"Finished scanning. Found {len(mesa_run_infos)} valid MESA runs.")

    return mesa_run_infos


def get_data_from_history_file(history_file_path):
    """
    Reads a MESA history.data file into a pandas DataFrame using NumPy's genfromtxt.

    This function handles the specific structure of MESA `history.data` files, which contain
    a few descriptive lines followed by column headers and numerical data. It attempts to
    parse all columns as numeric values and ensures that 'model_number' is an integer column,
    if present.

    Args:
        history_file_path (str): The absolute path to the MESA history.data file.

    Returns:
        pandas.DataFrame: A DataFrame containing the parsed history data.
                          Returns an empty DataFrame if the file is missing or cannot be parsed.

    
    Exception: 
        If there's an error loading or processing the file, it is logged and an empty DataFrame is returned instead.

    Example:
        >>> from mesalab.analyzis import data_reader
        >>> df = data_reader.get_data_from_history_file('/path/to/LOGS/history.data')
        >>> if not df.empty:
        ...     print(df.head())
        ...     print(f"Total models: {len(df)}")
        ...     print(f"Columns available: {list(df.columns)}")
        ... else:
        ...     print("Failed to load history.data or file was empty.")
    """
    if not os.path.exists(history_file_path):
        logger.error(f"History file not found: {history_file_path}")
        return pd.DataFrame() # Return empty DataFrame on file not found

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
            # If a single row, genfromtxt returns a 0-D array; convert to list of lists for DataFrame
            df = pd.DataFrame([data.tolist()], columns=data.dtype.names)
        else:
            df = pd.DataFrame(data)

        # Convert all columns to numeric, coercing unconvertible values to NaN.
        # This is a robust way to ensure numerical operations don't fail on mixed data.
        for col in df.columns:
            # Check if column contains any non-numeric data before conversion to avoid warnings on already numeric columns
            # Also, some columns might be strings representing specific flags, converting them to numeric may not be desired.
            # However, for MESA history, most columns are expected to be numeric.
            df[col] = pd.to_numeric(df[col], errors='coerce')

        # Ensure 'model_number' is an integer type if present and valid.
        if 'model_number' in df.columns:
            # Drop rows where model_number might be NaN (e.g., due to parsing errors or missing values)
            df.dropna(subset=['model_number'], inplace=True)
            # Convert to integer type only if no NaNs remain after dropping, otherwise it raises an error
            if not df['model_number'].isnull().any():
                df['model_number'] = df['model_number'].astype(int)

        return df

    except Exception as e:
        logger.error(f"Error loading or processing {history_file_path} using np.genfromtxt: {e}")
        return pd.DataFrame() # Return empty DataFrame on error
