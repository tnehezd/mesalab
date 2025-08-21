import pandas as pd
import numpy as np
import re
import os
import logging

logger = logging.getLogger(__name__)

def extract_params_from_inlist(inlist_path):
    """
    Extract `initial_mass`, `initial_Z`, and `initial_Y` values from a MESA inlist file.

    Handles Fortran-style scientific notation (e.g., `1.0d-2`, `2.5D+1`) by
    converting it to Python-compatible form. Returns `None` for missing values,
    with warnings logged.

    Args:
        inlist_path (str): Absolute path to the MESA inlist file.

    Returns:
        tuple: A tuple (mass, z, y) where elements are floats or `None`
               if the corresponding parameter is not found.

    Example: 
        Assuming 'my_run/inlist_project' contains:
        initial_mass = 1.0
        initial_Z = 0.014
        initial_Y = 0.28
        
        >>> from mesalab.analyzis import data_reader
        >>> data_reader.extract_params_from_inlist("my_run/inlist_project")
        (1.0, 0.014, 0.28)
    """
    mass = None
    z = None
    y = None

    try:
        with open(inlist_path, 'r') as f:
            content = f.read()

            # Regex for initial_mass: allows integers, floats, and scientific notation (e or d)
            mass_match = re.search(r'^\s*initial_mass\s*=\s*(\d+\.?\d*(?:[deDE][+\-]?\d+)?)\s*(?:!.*)?$', content, re.MULTILINE | re.IGNORECASE)
            if mass_match:
                mass = float(mass_match.group(1).replace('d', 'e').replace('D', 'E'))

            # Regex for initial_Z: allows integers, floats, and scientific notation (e or d)
            z_match = re.search(r'^\s*initial_Z\s*=\s*(\d+\.?\d*(?:[deDE][+\-]?\d+)?)\s*(?:!.*)?$', content, re.MULTILINE | re.IGNORECASE)
            if z_match:
                z = float(z_match.group(1).replace('d', 'e').replace('D', 'E'))

            # Regex for initial_Y: allows integers, floats, and scientific notation (e or d)
            y_match = re.search(r'^\s*initial_Y\s*=\s*(\d+\.?\d*(?:[deDE][+\-]?\d+)?)\s*(?:!.*)?$', content, re.MULTILINE | re.IGNORECASE)
            if y_match:
                y_value_str = y_match.group(1).replace('d', 'e').replace('D', 'E')
                y = float(y_value_str)
            else:
                logger.warning(f"Could not find 'initial_Y' in {inlist_path}. Setting to None for this run's parsing.")

    except FileNotFoundError:
        logger.error(f"Inlist file not found: {inlist_path}")
    except Exception as e:
        logger.error(f"Error reading inlist file {inlist_path}: {e}")

    # Log warnings if mass or Z parameters are not found
    if mass is None:
        logger.warning(f"Could not find 'initial_mass' in {inlist_path}.")
    if z is None:
        logger.warning(f"Could not find 'initial_Z' in {inlist_path}.")

    return mass, z, y


def scan_mesa_runs(input_dir, inlist_name):
    """
    Scan a directory for MESA run subdirectories that contain both an inlist file and history.data.

    Each valid subdirectory must:
      - Be a direct (non-hidden) subdirectory of `input_dir`
      - Contain the specified `inlist_name` file
      - Contain a LOGS/history.data file

    The function attempts to extract `initial_mass`, `initial_Z`, and `initial_Y` from each inlist file.
    Only runs with both `mass` and `z` values present are included in the output.

    Args:
        input_dir (str): Absolute path to the main directory containing MESA run subdirectories.
        inlist_name (str): Name of the inlist file expected in each subdirectory (e.g., 'inlist').

    Returns:
        list of dict: Each dictionary represents a valid MESA run and contains:
            - 'history_file_path' (str): Full path to the history.data file
            - 'run_dir_path' (str): Full path to the MESA run directory
            - 'mass' (float): Extracted initial_mass
            - 'z' (float): Extracted initial_Z
            - 'y' (float or None): Extracted initial_Y

        Returns an empty list if no valid runs are found.

    Example:
        Given a directory structure like::

            /path/to/mesa_grid/
            ├── run_M1.0_Z0.014_Y0.28
            │   ├── inlist
            │   └── LOGS
            │       └── history.data
            └── run_M2.0_Z0.006_Y0.25
                ├── inlist
                └── LOGS
                    └── history.data
        
        >>> from mesalab.analyzis import data_reader
        >>> data_reader.scan_mesa_runs("/path/to/mesa_grid", "inlist")
        [
            {
                'history_file_path': '/path/to/mesa_grid/run_M1.0_Z0.014_Y0.28/LOGS/history.data',
                'run_dir_path': '/path/to/mesa_grid/run_M1.0_Z0.014_Y0.28',
                'mass': 1.0,
                'z': 0.014,
                'y': 0.28
            },
            {
                'history_file_path': '/path/to/mesa_grid/run_M2.0_Z0.006_Y0.25/LOGS/history.data',
                'run_dir_path': '/path/to/mesa_grid/run_M2.0_Z0.006_Y0.25',
                'mass': 2.0,
                'z': 0.006,
                'y': 0.25
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
            mass, z, y = extract_params_from_inlist(inlist_path)
            if mass is not None and z is not None:
                mesa_run_infos.append({
                    'history_file_path': history_file_path,
                    'run_dir_path': run_dir_path,
                    'mass': mass,
                    'z': z,
                    'y': y
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
        Assuming a valid 'history.data' file exists at the given path:
        
        >>> from mesalab.analyzis import data_reader       
        >>> df = data_reader.get_data_from_history_file('/path/to/some_mesa_run/LOGS/history.data')
        >>> if not df.empty:
        ...     print(df.head())
        ...     print(f"Total models: {len(df)}")
        ...     print(f"Columns available: {list(df.columns)}")
        ... else:
        ...     print("Failed to load history.data or file was empty.")
    """
    if not os.path.exists(history_file_path):
        logger.error(f"History file not found: {history_file_path}")
        return pd.DataFrame()

    try:
        data = np.genfromtxt(history_file_path, names=True, comments="#", skip_header=5,
                             dtype=None, encoding='utf-8')

        if data.ndim == 0:
            df = pd.DataFrame([data.tolist()], columns=data.dtype.names)
        else:
            df = pd.DataFrame(data)

        for col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')

        if 'model_number' in df.columns:
            df.dropna(subset=['model_number'], inplace=True)
            if not df['model_number'].isnull().any():
                df['model_number'] = df['model_number'].astype(int)

        return df

    except Exception as e:
        logger.error(f"Error loading or processing {history_file_path} using np.genfromtxt: {e}")
        return pd.DataFrame()