.. _mesa_analysis_workflow:

MESA Run Analysis Workflow
==========================

The MESA Run Analysis workflow is the first step in the `mesalab` pipeline. Its primary purpose is to systematically process the raw output data from your MESA stellar evolution simulations, extracting key information and structuring it for subsequent analysis and visualization.

----

Purpose
-------

This workflow performs the following key functions:

* **Data Aggregation:** Collects ``history.data``, ``inlist``, ``profileX.data`` (and ``profiles.index``) files from multiple MESA runs in a given directory  .
* **Parameter Extraction:** Then, it extracts data (e.g., ``initial_mass``, ``initial_Z``, ``log_L``, ``log_Teff``, ``log_g``, ``star_age``, ``model_number``, ``log_R``, ``center_h1`` and ``center_he4``) from each MESA ``history.data`` and ``inlist`` file.
* **Evolutionary Phase Identification:** Automatically identifies significant evolutionary events or phases, such as:
    * **Hydrogen depletion and core helium burning**: Detects when a star depletes its central hydrogen and subsequently ignites helium in its core. This is a prerequisite for identifying blue loops.
    * **Blue Loops**: Identifies the presence of a blueward excursion (*blue loop*) in the HR diagram. This detection is only performed **after the star has reached the core helium burning phase**.
    * **Instability Strip (IS) Crossings**: Detects when a star's evolutionary track enters and exits the classical instability strip, and tracks the number of such crossings.
* **Profile Data Selection:** It automatically selects specific stellar profiles for models that takes `blue loops` and cross the instability strip. These selected profiles are useful for subsequent pulsation analysis with GYRE.
* **Data Summarization:** It writes out all extracted information into comprehensive summary files (i.e., CSV formatted files).

----

Input
-----

The MESA Run Analysis workflow requires your MESA simulation output directories.

* ``mesa_runs_base_dir``: The top-level directory containing all your MESA run folders. The workflow will scan all direct (non-hidden) subdirectories within this path for valid MESA runs.
* ``inlist`` **file**: You can specify your ``inlist`` file that contains the configuration of your MESA runs. If not set, `mesalab` searches for ``inlist_project`` files in the run folders.

----

Output
------

Upon successful completion, the MESA Run Analysis workflow generates several key output files within your session's ``analysis_results`` directory (which is typically located inside your main ``output_dir``). This directory serves as the direct input for the :doc:`plotting_workflow` and :doc:`gyre_workflow`.

* ``summary_results.csv``: A comprehensive CSV file containing a row for each MESA run analyzed, summarizing all extracted history data, detected evolutionary events (like IS crossings, blue loop properties), and global parameters. This file is the primary input for the :doc:`plotting_workflow`.
* ``sorted_mass_Z_min_max.csv``: This CSV file lists the paths to specific MESA runs (blue looper crossers, or optionally for the whole grid) along with their corresponding physical stellar parameters and ``model_numer`` paramters. This file serves as the direct input for the :doc:`gyre_workflow`.
* ``crossing_count_grid.csv``: It lists the number of instabilty edge crossing along the blue loop phase. It serves as an input for the :doc:`plotting_workflow` for optionally plot a heatmap.
* ``mesa_grid_time_differences.csv``: It summarizes the initial mass and metallicity for each MESA run, alongside the start age, end age, and calculated duration for detected blue loops and instability strip crossings.
* ``processed_runs_overview.yaml``: This YAML file maps stellar metallicities (Z) and masses (M) to their corresponding MESA run directories and history file names, detailing the organized structure of processed MESA data.

----

Configuration Parameters
------------------------

MESA Run Analysis workflow is controlled by parameters within your ref:`Understanding the YAML configuration file :ref:<understanding_yaml_config>` file. These parameters are organized under different top-level sections, as shown in the provided ``example.yaml``:

* `general_settings` section:
   * ``input_dir``: (String) **REQUIRED!**
     The path to the top-level directory containing your MESA run folders (e.g., your MESA grid directory).
     Default: "/path/to/your/mesa_runs_grid".
   * ``output_dir``: (String)
     The base directory where all `mesalab` results will be stored.
     Default: "./mesalab_output".
   * ``inlist_name``: (String)
     The filename used to identify MESA runs within their directories (e.g., "inlist_project", "inlist").
     Default: "inlist_project" (based on the example).
   * ``force_reanalysis``: (Boolean)
     If `true`, the workflow will re-run the analysis even if existing summary files are found.
     Default: `false`.
* `blue_loop_analysis` section:
   * ``analyze_blue_loop``: (Boolean)
     Set to `true` to enable blue loop detection and analysis.
     Default: `true`.
   * ``blue_loop_output_type``: (String)
     Controls the level of detail for blue loop output files. Options: `"summary"` or `"all"`.
     Default: `"all"`.

For a complete list of parameters and their detailed descriptions, please refer to the :ref:`Understanding the YAML Configuration <understanding_yaml_config>` section.

----

Running this Workflow Independently
-----------------------------------

You can choose to run *only* the MESA Run Analysis workflow if you already have your MESA simulation outputs and wish to generate the summary data without immediately proceeding to plotting or GYRE analysis. This is useful for debugging or for multi-stage workflows.

To run only this part, ensure your YAML configuration file has settings similar to this:

.. code-block:: yaml

    general_settings:
      input_dir: "/path/to/your/mesa_runs_grid"
      output_dir: "./mesalab_output"
      inlist_name: "inlist_project"
      force_reanalysis: false

    blue_loop_analysis:
      analyze_blue_loop: true
      blue_loop_output_type: "all"              # or "summary"

    plotting_settings:
      # To disable plotting, ensure relevant plotting flags are false or "none"
      generate_heatmaps: false
      generate_hr_diagrams: "none"
      generate_blue_loop_plots_with_bc: false

    gyre_workflow:
      run_gyre_workflow: false # Disable the GYRE workflow


Then, execute ``mesalab`` as usual:

.. code-block:: bash

    mesalab --config path/to/your_config_settings.yaml

The output will be saved to the ``analysis_results`` directory within your specified `output_dir`.