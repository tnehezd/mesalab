# mesalab/analyzis/data_reader.py

import pandas as pd
import numpy as np
import re
import os
import logging

logger = logging.getLogger(__name__) # Use a logger specific to this module

def extract_params_from_inlist(inlist_path):
    """
    Extract initial_mass and initial_Z from a MESA inlist file.
    Handles 'd' or 'D' in scientific notation (e.g., 1.0d-2).

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

            # Regex for initial_mass: allows integers, floats, and scientific notation (e or d)
            # Example: initial_mass = 1.0, initial_mass = 1.0d0, initial_mass = 20.5e-1
            mass_match = re.search(r'^\s*initial_mass\s*=\s*(\d+\.?\d*(?:[deDE][+\-]?\d+)?)\s*(?:!.*)?$', content, re.MULTILINE | re.IGNORECASE)
            if mass_match:
                # Replace 'd' or 'D' with 'e' before converting to float
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

    if mass is None:
        logger.warning(f"Could not find 'initial_mass' in {inlist_path}.")
    if z is None:
        logger.warning(f"Could not find 'initial_Z' in {inlist_path}.")

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

    potential_run_dir_names = [d for d in os.listdir(input_dir)
                               if os.path.isdir(os.path.join(input_dir, d))]

    if not potential_run_dir_names:
        logger.warning(f"No subdirectories found directly in '{input_dir}'. "
                       "Ensure your MESA runs are in individual folders within this input directory.")
        return []

    logger.info(f"Scanning '{input_dir}' for MESA run directories...")

    for run_dir_name in potential_run_dir_names:
        run_dir_path = os.path.join(input_dir, run_dir_name)
        inlist_path = os.path.join(run_dir_path, inlist_name)
        history_file_path = os.path.join(run_dir_path, 'LOGS', 'history.data')

        if os.path.exists(inlist_path) and os.path.exists(history_file_path):
            mass, z = extract_params_from_inlist(inlist_path)
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
            if not os.path.exists(inlist_path):
                logger.info(f"Inlist file '{inlist_name}' not found in '{run_dir_path}'. Skipping this directory.")
            if not os.path.exists(history_file_path):
                logger.info(f"history.data not found at '{history_file_path}'. Skipping this directory.")

    logger.info(f"Finished scanning. Found {len(mesa_run_infos)} valid MESA runs.")

    return mesa_run_infos


def get_data_from_history_file(history_file_path):
    """
    Reads a MESA history.data file into a pandas DataFrame using pd.read_csv.
    This version is generally more robust and flexible than np.genfromtxt for MESA's format.
    """
    if not os.path.exists(history_file_path):
        logger.error(f"History file not found: {history_file_path}")
        return pd.DataFrame()
    try:
        # MESA history.data files often have 2 header lines after &history_data
        # (column names and units). Let's find the column names line.
        with open(history_file_path, 'r') as f:
            lines = f.readlines()
            column_names_line = -1
            for i, line in enumerate(lines):
                # Look for 'model_number' as a reliable indicator of the column header line
                if 'model_number' in line and not line.strip().startswith('#') and not line.strip().startswith('&'):
                    column_names_line = i
                    break

            if column_names_line != -1:
                # Read the actual data, skipping lines up to and including the header line
                df = pd.read_csv(history_file_path,
                                 sep=r'\s+',      # Columns are separated by one or more whitespace
                                 comment='#',      # Lines starting with # are comments
                                 header=column_names_line, # Use this line as header
                                 skipinitialspace=True, # Skip whitespace after delimiter
                                 engine='python' # 'python' engine is more flexible with regex separators
                                )
                # Remove any empty columns that might result from extra whitespace
                df = df.loc[:, ~df.columns.str.contains('^Unnamed')]

                # Ensure 'model_number' is an integer type if present and valid.
                if 'model_number' in df.columns:
                    df['model_number'] = pd.to_numeric(df['model_number'], errors='coerce')
                    df.dropna(subset=['model_number'], inplace=True)
                    if not df['model_number'].isnull().any():
                        df['model_number'] = df['model_number'].astype(int)

                return df
            else:
                logger.error(f"Could not find column names in {history_file_path}. Data reading might be incorrect.")
                return pd.DataFrame()

    except Exception as e:
        logger.error(f"Error reading history.data from {history_file_path}: {e}")
        return pd.DataFrame()

# No read_profile_data in your provided data_reader, so not included here.
# If you need it, add it here.
