# mesa_tools/grid_analyzer.py
import os
from mesa_tools.inlist_parser import get_mesa_params_from_inlist

def analyze_mesa_grid_directory(grid_root_path,
                                mesa_output_subdir="LOGS",
                                inlist_filename="inlist",
                                inlist_alternatives=None):
    """
    Searches for MESA run directories within a given grid root path
    and extracts mass and metallicity from their inlist files.

    Args:
        grid_root_path (str): The root directory where MESA grid runs are located.
        mesa_output_subdir (str): The name of the subdirectory within each MESA run
                                  where output files (like history.data) are found.
        inlist_filename (str): The primary filename for the MESA inlist file.
        inlist_alternatives (list): A list of alternative inlist filenames to check if the primary is not found.

    Returns:
        list: A list of dictionaries, each containing 'path', 'mass', and 'z'
              for a found MESA run.
    """
    found_runs = []
    
    if inlist_alternatives is None:
        inlist_alternatives = []

    # Iterate through direct subdirectories of the grid_root_path only
    # We assume each direct subdirectory is a MESA run if it contains 'LOGS'
    for item_name in os.listdir(grid_root_path):
        current_run_path = os.path.join(grid_root_path, item_name)
        
        # Check if it's a directory and contains the mesa_output_subdir (e.g., LOGS)
        if os.path.isdir(current_run_path) and \
           os.path.exists(os.path.join(current_run_path, mesa_output_subdir)):
            
            print(f"Checking MESA run directory: '{current_run_path}'") # For debugging
            
            # Pass the config values to get_mesa_params_from_inlist
            params = get_mesa_params_from_inlist(current_run_path, inlist_filename, inlist_alternatives)
            
            if params:
                found_runs.append({
                    'path': current_run_path,
                    'mass': params.get('initial_mass'),
                    'z': params.get('initial_z')
                })
            else:
                print(f"  Could not extract parameters from '{current_run_path}'. Skipping.") # For debugging

    return found_runs

# --- Example of how to use it (for local testing of this module) ---
if __name__ == "__main__":
    # Replace with a real path to your MESA grid for testing
    test_grid_path = "/home/tnehezd/workdir/mesa-r23.05.1/STRANGE/nad_convos_mid/"
    
    # Test with explicit values (as if loaded from config.toml)
    results = analyze_mesa_grid_directory(
        test_grid_path,
        mesa_output_subdir="LOGS",
        inlist_filename="inlist",
        inlist_alternatives=["inlist_project", "inlist_1.0"]
    )
    
    if results:
        for run in results:
            print(f"Found: {run['path']} - Mass: {run['mass']}, Z: {run['z']}")
    else:
        print("No MESA runs found.")