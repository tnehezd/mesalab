# mesalab/io/inlist_parser.py
import os
import re

def get_mesa_params_from_inlist(run_path,
                                inlist_filename="inlist",
                                inlist_alternatives=None):
    """
    Extracts `initial_mass` and `initial_z` from a MESA inlist file in a given run directory.

    This function looks for the specified inlist file or alternatives in the provided directory,
    and parses the initial mass and metallicity values using regular expressions.

    Parameters:
        run_path (str): Path to the MESA run directory.
        inlist_filename (str): Primary inlist filename to search for (default: "inlist").
        inlist_alternatives (list, optional): Additional filenames to try before the primary one.

    Returns:
        dict or None: Dictionary with keys `'initial_mass'` and `'initial_z'`, or `None` if parsing fails.

    Example:
        >>> get_mesa_params_from_inlist("runs/model_001", inlist_alternatives=["inlist_project"])
        {'initial_mass': 5.0, 'initial_z': 0.0152}
    """
    if inlist_alternatives is None:
        inlist_alternatives = []

    # Create a list of filenames to check, prioritizing alternatives
    # If inlist_alternatives contains ["inlist_project", "inlist_1.0"] and inlist_filename is "inlist",
    # the order will be "inlist_project", "inlist_1.0", then "inlist".
    filenames_to_check = inlist_alternatives + [inlist_filename]

    inlist_file_to_check = None
    found_inlist = False

    for current_filename in filenames_to_check:
        full_path = os.path.join(run_path, current_filename)
        if os.path.exists(full_path):
            inlist_file_to_check = full_path
            found_inlist = True
            print(f"Found inlist file: '{inlist_file_to_check}'") # Added for debugging
            break # Found one, stop checking

    if not found_inlist:
        print(f"No inlist file found in '{run_path}' with names: {', '.join(filenames_to_check)}") # Added for debugging
        return None # No inlist file found at all

    params = {}
    try:
        with open(inlist_file_to_check, 'r') as f:
            content = f.read()

        # Regular expression to find 'initial_mass' (now case-insensitive)
        mass_match = re.search(r'initial_mass\s*=\s*([0-9.]+)', content, re.IGNORECASE)
        if mass_match:
            params['initial_mass'] = float(mass_match.group(1))
        else:
            print(f"Warning: initial_mass not found in '{inlist_file_to_check}'.")
            return None # Return None if parameter is not found

        # Regular expression to find 'initial_z' (now case-insensitive)
        z_match = re.search(r'initial_z\s*=\s*([0-9.eE-]+)', content, re.IGNORECASE) # Handles scientific notation
        if z_match:
            params['initial_z'] = float(z_match.group(1))
        else:
            print(f"Warning: initial_z not found in '{inlist_file_to_check}'.")
            return None # Return None if parameter is not found

        return params

    except Exception as e:
        print(f"Error reading or parsing inlist file '{inlist_file_to_check}': {e}")
        return None

# --- Example of how to use it (for local testing of this module) ---
if __name__ == "__main__":
    # Replace with a real path to a MESA run directory for testing
    test_run_path = "/path/to/a/single/mesa/run"
    
    # Test with default and alternative inlist names
    params = get_mesa_params_from_inlist(test_run_path, inlist_filename="inlist", inlist_alternatives=["inlist_project", "inlist_other"])
    if params:
        print(f"Extracted parameters: Mass={params.get('initial_mass')}, Z={params.get('initial_z')}")
    else:
        print("Failed to extract parameters.")