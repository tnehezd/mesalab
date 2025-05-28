# MESA Blue Loop Analyzer

This project provides a command-line tool (`mesa_grid_analyzer`) to easily extract and summarize stellar evolution parameters (like mass and metallicity) from a grid of MESA (Modules for Experiments in Stellar Astrophysics) simulation runs.

---

## Features

* **Automated Parameter Extraction**: Scans through a specified directory to find MESA run folders.
* **Mass and Metallicity Detection**: Identifies mass (`M`) and metallicity (`Z`) parameters from `inlist` files.
* **Command-Line Interface**: Easy to use via the `mesa_grid_analyzer` command in your terminal.
* **Python Package**: Designed as a reusable Python package (`mesa_tools`).

---

## Installation

To get started with the `mesa_tools` package and the `mesa_grid_analyzer` tool, follow these steps:

1.  **Clone the repository (if applicable):**
    ```bash
    git clone [https://github.com/your-username/mesa_blue_loop.git](https://github.com/your-username/mesa_blue_loop.git) # Replace with your repo URL
    cd mesa_blue_loop
    ```

2.  **Create and activate a Conda environment (recommended):**
    It's best practice to use a dedicated environment to avoid conflicts.
    ```bash
    conda create -n mesa_loop_py37 python=3.7
    conda activate mesa_loop_py37
    ```

3.  **Install the package in editable mode:**
    This allows you to make changes to the source code and see them reflected without re-installation.
    ```bash
    python -m pip install -e .
    ```

---

## Usage

Once installed, you can use the `mesa_grid_analyzer` command to process your MESA data.

### Running the Analyzer

Execute the tool from your terminal, specifying the input MESA grid directory and an output file for results:

```bash
mesa_grid_analyzer -i /path/to/your/mesa/grid/directory -o results.txt
```





---
## Grid Analysis with `mesa_grid_analyzer`

This project includes a command-line tool, `mesa_grid_analyzer`, designed to automate the analysis of MESA stellar evolution grids. It scans specified directories for MESA run outputs, extracts key parameters (like initial mass and metallicity) from run paths, and applies custom analysis functions (e.g., for blue loop characteristics).

### Usage

To run the grid analyzer, navigate to the root of your `mesa_blue_loop` project and execute:

```bash
conda activate mesa_loop_py37 # Or your MESA analysis environment
mesa_grid_analyzer -i /path/to/your/mesa/runs -o /path/to/output/results --inlist-name inlist_project --analyze-blue-loop
```



## Key Arguments:

-i or --input-dir: (Required) Path to the root directory containing your MESA run subdirectories.
-o or --output-dir: (Required) Path to the directory where the analysis results (e.g., CSV files) will be saved.
--inlist-name: The name of the inlist file (e.g., inlist_project) used to identify valid MESA run directories. (Default: inlist_project)
--analyze-blue-loop: A flag to enable the blue loop analysis. When set, the tool will calculate metrics like the number of blue loop crossings and key evolutionary ages.
--blue-loop-output-type: (Choices: summary, all) Specifies the detail level for blue loop output.
summary: Outputs only the blue loop crossing count.
all: Outputs the crossing count along with detailed age information (e.g., ms_end_age, first_is_entry_age, etc.). (Default: all)
Robust history.data Reading

The mesa_grid_analyzer utilizes a robust mesa_reader module. This module is designed to handle common variations in MESA's history.data file format, including different header line structures. Crucially, it incorporates error handling to gracefully skip malformed or incomplete data lines within history.data files, preventing crashes during large grid analyses where individual runs might have terminated unexpectedly or produced corrupted output. This ensures that the analysis can proceed even if some history files are not perfectly formed.