# mesalab/io/inlist_parser.py

import os
import re
import logging

# Configure logger for this module
logger = logging.getLogger(__name__)

def get_mesa_params_from_inlist(run_path,
                                 inlist_filename="inlist",
                                 inlist_alternatives=None):
    """
    Extracts 'initial_mass', 'initial_z', and 'initial_y' from a MESA inlist file
    in a given run directory.

    This function looks for the specified inlist file or alternatives in the provided directory,
    and parses the initial mass, metallicity, and helium abundance values using regular expressions.

    Parameters:
        run_path (str): Path to the MESA run directory.
        inlist_filename (str): Primary inlist filename to search for (default: "inlist").
        inlist_alternatives (list, optional): Additional filenames to try.
                                                Defaults to an empty list.

    Returns:
        dict or None: Dictionary with keys 'initial_mass', 'initial_z', and 'initial_y' (if found).
                      Returns None if critical parameters like mass or Z are not found,
                      or if no inlist file is found.

    Example:
        >>> from mesalab.io import inlist_parser
        >>> result = inlist_parser.get_mesa_params_from_inlist("runs/model_001", inlist_filename=["inlist"])
        >>> print(result)
        {'initial_mass': 5.0, 'initial_z': 0.0152, 'initial_y': 0.28}
    """
    if inlist_alternatives is None:
        inlist_alternatives = []

    # Check the primary filename first, then the alternatives.
    filenames_to_check = [inlist_filename] + inlist_alternatives

    inlist_file_to_check = None
    
    # Iterate through potential inlist filenames to find the first existing one
    for current_filename in filenames_to_check:
        full_path = os.path.join(run_path, current_filename)
        if os.path.exists(full_path):
            inlist_file_to_check = full_path
            logger.debug(f"INLIST_PARSER: Found inlist file: '{inlist_file_to_check}' in run '{run_path}'")
            break # Found one, stop checking

    if inlist_file_to_check is None:
        logger.warning(f"INLIST_PARSER: No inlist file found in '{run_path}' with names: {', '.join(filenames_to_check)}")
        return None # No inlist file found at all, cannot proceed

    params = {}
    try:
        logger.debug(f"INLIST_PARSER: Attempting to parse '{inlist_file_to_check}'")
        with open(inlist_file_to_check, 'r') as f:
            content = f.read()

        # Regular expression to find 'initial_mass'. It's case-insensitive and handles floats.
        mass_match = re.search(r'initial_mass\s*=\s*([0-9.]+)', content, re.IGNORECASE)
        if mass_match:
            params['initial_mass'] = float(mass_match.group(1))
            logger.debug(f"INLIST_PARSER: Extracted initial_mass = {params['initial_mass']}")
        else:
            logger.warning(f"INLIST_PARSER: 'initial_mass' not found in '{inlist_file_to_check}'. Returning None.")
            return None # Initial mass is a critical parameter, so return None if not found

        # Regular expression to find 'initial_z'. It handles standard scientific notation (e, E),
        # and MESA's 'd' notation for exponents (e.g., 1.25d-2).
        z_match = re.search(r'initial_z\s*=\s*([0-9.eE-]+(?:[dD][-+]?\d+)?)', content, re.IGNORECASE)
        if z_match:
            # Replace 'd' or 'D' with 'e' for Python's float conversion if MESA's 'd' notation is used
            z_value_str = z_match.group(1).replace('d', 'e').replace('D', 'E')
            params['initial_z'] = float(z_value_str)
            logger.debug(f"INLIST_PARSER: Extracted initial_z = {params['initial_z']}")
        else:
            logger.warning(f"INLIST_PARSER: 'initial_z' not found in '{inlist_file_to_check}'. Returning None.")
            return None # Initial Z is a critical parameter, so return None if not found

        # Regular expression for 'initial_y'. It handles floats, scientific notation (e, E),
        # and MESA's 'd' notation for exponents.
        y_match = re.search(r'initial_y\s*=\s*([0-9.eE-]+(?:[dD][-+]?\d+)?)', content, re.IGNORECASE)
        if y_match:
            # Replace 'd' or 'D' with 'e' for Python's float conversion if MESA's 'd' notation is used
            y_value_str = y_match.group(1).replace('d', 'e').replace('D', 'E')
            params['initial_y'] = float(y_value_str)
            logger.debug(f"INLIST_PARSER: Extracted initial_y = {params['initial_y']}")
        else:
            # It's acceptable if initial_y is not found, as a default might be used elsewhere.
            logger.warning(f"INLIST_PARSER: 'initial_y' not found in '{inlist_file_to_check}'. Setting to None for this run.")
            params['initial_y'] = None # Explicitly set to None if not found

        logger.debug(f"INLIST_PARSER: Final parsed parameters from '{inlist_file_to_check}': {params}")
        return params

    except FileNotFoundError:
        # This case should be caught by the file existence check, but included for robustness.
        logger.error(f"INLIST_PARSER: Inlist file not found (unexpectedly): {inlist_file_to_check}")
        return None
    except Exception as e:
        # Catch any other unexpected errors during file reading or parsing
        logger.error(f"INLIST_PARSER: Error reading or parsing inlist file '{inlist_file_to_check}': {e}")
        return None


# --- Example of how to use it (for local testing of this module) ---
if __name__ == "__main__":
    # Configure logging for standalone testing of this module
    logging.basicConfig(level=logging.DEBUG, format='%(asctime)s - %(levelname)s - %(name)s: %(message)s')
    
    # IMPORTANT: Replace this with an actual path to a MESA run directory
    # that contains an inlist file for testing.
    # Example: test_run_path = "/Users/YourUser/MESA_runs/your_mesa_project/LOGS"
    test_run_path = "/path/to/a/single/mesa/run_directory" 
    
    logger.info(f"--- Testing get_mesa_params_from_inlist for '{test_run_path}' ---")

    # Test with a specific inlist filename and alternatives if applicable
    # Example: if your inlist is named 'inlist_1.0M_0.02Z' try that.
    # Or if your config uses 'inlist_project', use that here.
    params = get_mesa_params_from_inlist(test_run_path,
                                         inlist_filename="inlist_project", # Adjust this to your actual inlist name
                                         inlist_alternatives=["inlist", "inlist_other"])
    if params:
        logger.info(f"Successfully extracted parameters:")
        logger.info(f"  Mass (initial_mass): {params.get('initial_mass')}")
        logger.info(f"  Z (initial_z): {params.get('initial_z')}")
        logger.info(f"  Y (initial_y): {params.get('initial_y')}")
    else:
        logger.error("Failed to extract parameters. See warnings/errors above for details.")
    
    logger.info("--- Testing complete ---")
