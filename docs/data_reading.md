# MESA Data Reading and Handling

This module is responsible for scanning and reading MESA simulation runs, and
preparing the data for further analysis.

## `extract_params_from_inlist(inlist_path)`

*Purpose:* This helper function extracts the `initial_mass` and `initial_Z` parameters from a MESA `inlist` file.

*Parameters:*
* `inlist_path` (`str`): The absolute path to the MESA `inlist` file.

*Returns:*
* `tuple`: A tuple `(mass, z)` as floats. Returns `(None, None)` if parameters cannot be found or an error occurs during reading.

*Details:*
The function handles Fortran-style scientific notation (e.g., `1.0d-2` is converted to `1.0e-2`) to ensure correct float parsing. It gracefully handles missing parameters by logging warnings.

## `scan_mesa_runs(input_dir, inlist_name)`

*Purpose:* This function scans a specified input directory for valid MESA run subdirectories, identifying simulations ready for processing.

*Parameters:*
* `input_dir` (`str`): The absolute path to the main directory containing MESA run subdirectories.
* `inlist_name` (`str`): The filename of the inlist (e.g., `'inlist_project'`) expected within each run directory.

*Returns:*
* `list`: A list of dictionaries, where each dictionary represents a valid MESA run. Each dictionary contains:
    * `'history_file_path'` (`str`): Path to the `history.data` file.
    * `'run_dir_path'` (`str`): Path to the MESA run directory.
    * `'mass'` (`float`): Extracted `initial_mass`.
    * `'z'` (`float`): Extracted `initial_Z`.

*Details:*
The function automatically filters out hidden directories (e.g., `.mesa_temp_cache`). It considers a directory a valid run only if it contains both the specified `inlist` file and a `LOGS/history.data` file, and `initial_mass`/`initial_Z` can be successfully extracted. Internally, it uses `extract_params_from_inlist`.

*Example Usage:*
```python
import os
from mesalab.data_reader import scan_mesa_runs # Assuming its path

# Example directory structure:
# /path/to/my_mesa_sims/
# ├── run_01/
# │   ├── inlist_project
# │   └── LOGS/
# │       └── history.data
# └── run_02/
#     ├── inlist_project
#     └── LOGS/
#         └── history.data

input_directory = "/path/to/my_mesa_sims"
inlist_filename = "inlist_project"

found_runs = scan_mesa_runs(input_directory, inlist_filename)
for run_info in found_runs:
    print(f"Found run: Mass={run_info['mass']}, Z={run_info['z']}, Path={run_info['run_dir_path']}")
