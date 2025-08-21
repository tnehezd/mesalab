#mesalab/analyzis/grid_analyzer.py

import os
import logging
from ..io.inlist_parser import get_mesa_params_from_inlist

def analyze_mesa_grid_directory(grid_root_path,
                                mesa_output_subdir="LOGS",
                                inlist_filename="inlist",
                                inlist_alternatives=None):
    """
    Searches for MESA run directories within a given grid root path
    and extracts stellar mass and metallicity from their inlist files.

    A valid MESA run directory is defined as a subdirectory of `grid_root_path`
    that contains the `mesa_output_subdir` (default: 'LOGS'), and an inlist file
    from which initial_mass and initial_z can be extracted.

    Args:
        grid_root_path (str): The root directory where multiple MESA runs are located.
        mesa_output_subdir (str, optional): Subdirectory within each run directory
                                            that indicates a valid MESA run (default: "LOGS").
        inlist_filename (str, optional): Primary inlist filename to look for (default: "inlist").
        inlist_alternatives (list of str, optional): Alternative inlist filenames to try if
                                                     the primary is not found (default: None).

    Returns:
        list of dict: A list of dictionaries. Each dictionary has the following keys:
            - 'path': full path to the MESA run directory
            - 'mass': float, the initial_mass from the inlist
            - 'z': float, the initial_z from the inlist

    Example:
        >>> from mesalab.analyzis import grid_analyzer
        >>> results = grid_analyzer.analyze_mesa_grid_directory(
        ...     "/home/user/mesa_runs/",
        ...     mesa_output_subdir="LOGS",
        ...     inlist_filename="inlist",
        ...     inlist_alternatives=["inlist_project"]
        ... )
        >>> for run in results:
        ...     print(f"Path: {run['path']}, Mass: {run['mass']}, Z: {run['z']}")
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
            
            logging.debug(f"Checking MESA run directory: '{current_run_path}'")
            
            # Pass the config values to get_mesa_params_from_inlist
            params = get_mesa_params_from_inlist(current_run_path, inlist_filename, inlist_alternatives)
            
            if params and 'initial_mass' in params and 'initial_z' in params:
                # Add initial_y to the dictionary if it's found in params, otherwise use None or a default
                initial_y_val = params.get('initial_y') # .get() allows a default if key not present

                found_runs.append({
                    'path': current_run_path,
                    'mass': params['initial_mass'],
                    'z': params['initial_z'],
                    'initial_y': initial_y_val
                })


                # --- ADD THESE LINES TO DEBUG THE EXTRACTED PARAMS ---
                logging.debug(f"DEBUGGING GRID_ANALYZER: Parameters for run '{item_name}':")
                logging.debug(f"  Path: {current_run_path}")
                logging.debug(f"  Mass: {params.get('initial_mass', 'N/A')}")
                logging.debug(f"  Z: {params.get('initial_z', 'N/A')}")
                logging.debug(f"  Initial Y: {params.get('initial_y', 'N/A')}")
                # --- END OF ADDED LINES ---




            else:
                logging.warning(f"Could not extract 'initial_mass' or 'initial_z' from inlist in '{current_run_path}'. Skipping.")

    return found_runs

# --- Example of how to use it (for local testing of this module) ---
if __name__ == "__main__":
    logging.basicConfig(level=logging.WARNING, format='%(levelname)s: %(message)s') # For independent testing

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
        f.write("&star_job\n   initial_mass = 1.0\n   initial_z = 0.0140\n/\n")
    with open(os.path.join(test_grid_path, "run_3.0MSUN_z0.0070", "inlist"), "w") as f:
        f.write("&star_job\n   initial_mass = 3.0\n   initial_z = 0.0070\n/\n")


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
