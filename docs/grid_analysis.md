# MESA Grid Analysis
This module, located at `mesalab/analysis/grid_analyzer.py`, provides functionality to scan and analyze directories containing multiple MESA simulation runs, typically organized as a grid. It helps in automatically discovering and cataloging these runs based on their `inlist` files and `LOGS` directories.

---

## `analyze_mesa_grid_directory(grid_root_path, mesa_output_subdir="LOGS", inlist_filename="inlist", inlist_alternatives=None)`
Purpose: This function searches for MESA run directories within a specified grid root path, then extracts the initial mass and metallicity parameters from their `inlist` files. It's designed to automate the discovery of individual MESA simulations within a larger grid structure.

*Parameters:*

* `grid_root_path` (`str`): 
The absolute path to the root directory where your MESA grid runs are located. The function expects each direct subdirectory within this path to represent an individual MESA run.

* `mesa_output_subdir` (`str`, `optional`): 
The name of the subdirectory within each MESA run where output files (like `history.data`) are found. Defaults to `LOGS`.

* `inlist_filename` (`str`, `optional`): 
The primary filename for the MESA inlist file that the function will try to find in each run directory. Defaults to `inlist`.

* `inlist_alternatives` (`list`, `optional`): 
A list of alternative `inlist` filenames to check if the `inlist_filename` is not found. Defaults to None (no alternatives).

*Returns:*

* `list`: A list of dictionaries. Each dictionary represents a discovered MESA run and contains:

    * `'path'` (`str`): The absolute path to the MESA run directory.

    * `'mass'` (`float`): The `initial_mass` extracted from the `inlist` file.

    * `'z'` (`float`): The `initial_z` (metallicity) extracted from the `inlist` file.

Returns an empty list if no valid runs are found or if `grid_root_path` is not a valid directory.

*Details:*
The function iterates through direct subdirectories of the `grid_root_path`. For each subdirectory, it verifies the presence of the `mesa_output_subdir` (e.g., `LOGS`) to confirm it's a potential MESA run. It then attempts to read the `initial_mass` and `initial_z` from the `inlist` file(s) using the `get_mesa_params_from_inlist` helper function. Directories where parameters cannot be successfully extracted are skipped, and a warning is logged.

*Example Usage:*
Let's assume you have a directory structure like this:
```text
/path/to/my_mesa_grids/
├── solar_mass_grid/
│   ├── M1.0_Z0.014/
│   │   ├── inlist
│   │   └── LOGS/
│   │       └── history.data
│   ├── M2.0_Z0.014/
│   │   ├── inlist
│   │   └── LOGS/
│   │       └── history.data
│   └── M1.0_Z0.007/
│       ├── inlist_project
│       └── LOGS/
│           └── history.data
└── another_grid/
```

```Python
import os
import logging
from mesalab.analysis.grid_analyzer import analyze_mesa_grid_directory

# Configure basic logging to see messages
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')

# --- Create a dummy structure for demonstration purposes ---
# In a real scenario, this 'temp_test_mesa_grid' would be your actual MESA grid root path
test_grid_path = "temp_test_mesa_grid"
os.makedirs(os.path.join(test_grid_path, "run_1.0MSUN_z0.0140", "LOGS"), exist_ok=True)
os.makedirs(os.path.join(test_grid_path, "run_3.0MSUN_z0.0070", "LOGS"), exist_ok=True)

# Create dummy inlist files with content parsable by get_mesa_params_from_inlist
with open(os.path.join(test_grid_path, "run_1.0MSUN_z0.0140", "inlist"), "w") as f:
    f.write("&star_job\n   initial_mass = 1.0\n   initial_z = 0.0140\n/\n")
with open(os.path.join(test_grid_path, "run_3.0MSUN_z0.0070", "inlist"), "w") as f:
    f.write("&star_job\n   initial_mass = 3.0\n   initial_z = 0.0070\n/\n")

# --- Call the function ---
results = analyze_mesa_grid_directory(
    test_grid_path,
    mesa_output_subdir="LOGS",
    inlist_filename="inlist",
    inlist_alternatives=["inlist_project"] # Check 'inlist_project' if 'inlist' isn't found
)

if results:
    for run in results:
        logging.info(f"Found MESA run: Path='{run['path']}' | Mass={run['mass']} | Z={run['z']}")
else:
    logging.info("No valid MESA runs found in the specified grid directory.")

# --- Clean up dummy structure (uncomment to enable) ---
# import shutil
# if os.path.exists(test_grid_path):
#     shutil.rmtree(test_grid_path)

```
