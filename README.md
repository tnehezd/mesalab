# MESA Grid Analyzer

This project provides a command-line tool (`mesa_grid_analyzer`) designed to analyze and visualize results from a grid of MESA (Modules for Experiments in Stellar Astrophysics) stellar evolution simulations. It helps automate the extraction of key parameters, perform blue loop analysis, and generate plots like HR Diagrams and heatmaps across your simulation grid.

---

## Features

* **Automated Parameter Extraction**: Scans through specified directories to find MESA run folders and extract initial mass (`M`) and metallicity (`Z`) from `inlist` files.
* **Blue Loop Analysis**: Identifies and quantifies blue loop characteristics, such as crossing counts and key evolutionary ages (e.g., blue loop entry/exit, instability strip crossing).
* **Detailed Output**: Generates summary CSVs for the entire grid (`mesa_grid_analysis_summary.csv`, `mesa_grid_cross.csv`) and detailed CSVs for blue loop phases per metallicity group (`detail_z*.csv`).
* **Heatmap Generation**: Creates visual heatmaps of blue loop crossing counts across the M-Z grid, providing a quick overview of blue loop presence.
* **HR Diagram (HRD) Plotting**: Generates high-quality Hertzsprung-Russell Diagrams for individual stellar evolution tracks, grouped by metallicity, with optional instability strip visualization.
* **Robust Data Reading**: Utilizes a robust data reader designed to handle common variations in MESA's `history.data` file format and gracefully skip malformed or incomplete data lines, ensuring analysis can proceed even with imperfect outputs.
* **Command-Line Interface**: Easy to use via the `mesa_grid_analyzer` command in your terminal.
* **Python Package**: Designed as a reusable Python package (`mesa_tools`).

---

## Installation

To get started with the `mesa_tools` package and the `mesa_grid_analyzer` tool, follow these steps:

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/tnehezd/mesa_blue_loop.git](https://github.com/tnehezd/mesa_blue_loop.git) # Replace with your actual repository URL
    cd mesa_blue_loop
    ```

2.  **Create and activate a Conda environment (recommended):**
    Using a dedicated environment is best practice to avoid dependency conflicts.
    ```bash
    conda create -n mesa_loop_env python=3.7 pandas numpy matplotlib pyyaml tqdm
    conda activate mesa_loop_env
    ```
    *(Note: You can adjust `python=3.7` to your preferred Python version, ensuring it's compatible with your MESA setup.)*

3.  **Install the package in editable mode:**
    This allows you to make changes to the source code and see them reflected without re-installation.
    ```bash
    python -m pip install -e .
    ```

---

## Usage

Once installed and your Conda environment is active, you can use the `mesa_grid_analyzer` command to process and visualize your MESA data.

### Basic Command Structure

```bash
mesa_grid_analyzer -i <input_mesa_grid_dir> -o <output_results_dir> [options]
-i or --input-dir (Required): Path to the root directory containing your MESA run subdirectories (e.g., run_M2.0_Z0.01).
-o or --output-dir (Required): Path to the directory where all analysis results (CSVs, plots, heatmaps) will be saved.
--inlist-name (Default: inlist_project): The name of the inlist file used to identify valid MESA run directories.
Common Use Cases
```


Here are some common ways to run the mesa_grid_analyzer tool:

1. Perform a Full Grid Analysis

This command will scan all MESA runs, perform the blue loop analysis, generate summary CSVs, create detailed blue loop CSVs per Z-value, generate heatmaps of blue loop crossing counts, and produce HR Diagrams for each metallicity group.

Bash
mesa_grid_analyzer \
    -i /path/to/your/mesa/grid/runs \
    -o /path/to/output/results \
    --inlist-name inlist_project \
    --analyze-blue-loop \
    --generate-heatmaps \
    --generate-plots \
    --force-reanalysis # Use this to re-run all analysis even if summary files exist
2. Generate Only HR Diagrams (from existing data)

If you've already run the analysis and your summary/cross CSVs exist, you can efficiently regenerate just the HRD plots without re-running the full blue loop analysis. The tool will simply load the necessary history.data files for plotting.

Bash
```

mesa_grid_analyzer \
    -i /path/to/your/mesa/grid/runs \
    -o /path/to/output/results \
    --inlist-name inlist_project \
    --generate-plots
```

3. Generate Only Heatmaps (from existing summary data)

Similarly, if your summary.csv and cross.csv files are already generated, you can recreate just the heatmaps.

Bash
```
mesa_grid_analyzer \
    -i /path/to/your/mesa/grid/runs \
    -o /path/to/output/results \
    --inlist-name inlist_project \
    --generate-heatmaps
Advanced Options

--blue-loop-output-type (Choices: summary, all; Default: all):
When --analyze-blue-loop is enabled, this controls the detail level for the detail_z*.csv files:
summary: Outputs only selected key columns relevant to the blue loop phase.
all: Outputs all columns from the history.data for the identified blue loop phase.
```





## Installation

To get started with the `mesa_tools` package and the `mesa_grid_analyzer` tool, follow these steps:

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/tnehezd/mesa_blue_loop.git](https://github.com/tnehezd/mesa_blue_loop.git) # Replace with your actual repository URL
    cd mesa_blue_loop
    ```

2.  **Create and activate a Conda environment (recommended):**
    Using a dedicated environment is best practice to avoid dependency conflicts.
    ```bash
    conda create -n mesa_loop_env python=3.7 pandas numpy matplotlib pyyaml tqdm scipy astropy beautifulsoup4 requests isochrones
    conda activate mesa_loop_env
    ```
    *(Note: You can adjust `python=3.7` to your preferred Python version, ensuring it's compatible with your MESA setup.)*

3.  **Install the package in editable mode:**
    This allows you to make changes to the source code and see them reflected without re-installation.
    ```bash
    python -m pip install -e .
    ```
    
    idna, scipy, astropy,beautifulsoup4
    holoview, pymultinest
    
    
    
    ## Installation

mesalab requires Python 3.x and the following packages:

- f90nml: `pip install f90nml`
- (If you use numpy, scipy, matplotlib later: `pip install numpy scipy matplotlib`)

You will also need a working MESA and GYRE installation, with the `GYRE_DIR` environment variable set up correctly in your shell.