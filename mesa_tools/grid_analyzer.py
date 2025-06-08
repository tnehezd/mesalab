# mesa_tools/grid_analyzer.py
import os
import logging # ADDED: Import logging
from mesa_tools.inlist_parser import get_mesa_params_from_inlist

# Configure logging for this module if it's run independently,
# but main cli.py will set global config.
# logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')

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

    if not os.path.isdir(grid_root_path):
        logging.error(f"Error: Grid root path '{grid_root_path}' is not a valid directory.")
        return []

    # Iterate through direct subdirectories of the grid_root_path only
    # We assume each direct subdirectory is a MESA run if it contains 'LOGS'
    for item_name in os.listdir(grid_root_path):
        current_run_path = os.path.join(grid_root_path, item_name)
        
        # Check if it's a directory and contains the mesa_output_subdir (e.g., LOGS)
        if os.path.isdir(current_run_path) and \
           os.path.exists(os.path.join(current_run_path, mesa_output_subdir)):
            
            logging.debug(f"Checking MESA run directory: '{current_run_path}'") # Changed print to logging.debug
            
            # Pass the config values to get_mesa_params_from_inlist
            params = get_mesa_params_from_inlist(current_run_path, inlist_filename, inlist_alternatives)
            
            if params and 'initial_mass' in params and 'initial_z' in params: # Ensure both keys exist
                found_runs.append({
                    'path': current_run_path,
                    'mass': params['initial_mass'], # Using direct access now that we check for existence
                    'z': params['initial_z']      # Using direct access now that we check for existence
                })
            else:
                logging.warning(f"Could not extract 'initial_mass' or 'initial_z' from inlist in '{current_run_path}'. Skipping.") # Changed print to logging.warning

    return found_runs

# --- Example of how to use it (for local testing of this module) ---
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s') # For independent testing

    # IMPORTANT: For this example to work, you NEED a mesa_tools/inlist_parser.py
    # with a working get_mesa_params_from_inlist function.
    # If you don't have it, create dummy MESA directories with inlist files.
    # test_grid_path = "/home/tnehezd/workdir/mesa-r23.05.1/STRANGE/nad_convos_mid/"
    
    # Example for creating dummy structure for testing this module if real MESA data is not present
    test_grid_path = "temp_test_mesa_grid"
    os.makedirs(os.path.join(test_grid_path, "run_1.0MSUN_z0.0140", "LOGS"), exist_ok=True)
    os.makedirs(os.path.join(test_grid_path, "run_3.0MSUN_z0.0070", "LOGS"), exist_ok=True)
    
    # Create dummy inlist files if you're using the dummy 'inlist_parser' or no real inlists
    # You would need to add content that get_mesa_params_from_inlist can parse
    with open(os.path.join(test_grid_path, "run_1.0MSUN_z0.0140", "inlist"), "w") as f:
        f.write("&star_job\n  initial_mass = 1.0\n  initial_z = 0.0140\n/\n")
    with open(os.path.join(test_grid_path, "run_3.0MSUN_z0.0070", "inlist"), "w") as f:
        f.write("&star_job\n  initial_mass = 3.0\n  initial_z = 0.0070\n/\n")


    results = analyze_mesa_grid_directory(
        test_grid_path,
        mesa_output_subdir="LOGS",
        inlist_filename="inlist",
        inlist_alternatives=["inlist_project", "inlist_1.0"]
    )
    
    if results:
        for run in results:
            logging.info(f"Found: {run['path']} - Mass: {run['mass']}, Z: {run['z']}")
    else:
        logging.info("No MESA runs found.")