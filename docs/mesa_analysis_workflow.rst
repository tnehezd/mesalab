.. _mesa_analysis_workflow:

MESA Run Analysis Workflow
==========================

The MESA Run Analysis workflow is the foundational step in the ``mesalab`` pipeline. Its primary purpose is to systematically process the raw output data from your MESA stellar evolution simulations, extracting key information and structuring it for subsequent analysis and visualization.

Purpose
-------

This workflow performs the following key functions:

* **Data Aggregation:** Collects `history` and `profile` data from multiple MESA runs (e.g., from a grid of models).
* **Parameter Extraction:** Extracts crucial global parameters (e.g., initial mass, initial metallicity, final mass, lifetime, luminosity, effective temperature, core properties, etc.) from each MESA history file.
* **Evolutionary Phase Identification:** Automatically identifies significant evolutionary events or phases, such as:
    * **Instability Strip (IS) Crossings:** Detects when a star's evolutionary track enters and exits the classical instability strip, and tracks the number of such crossings.
    * **Blue Loops:** Identifies the presence and characteristics of blue loops in the HR diagram.
* **Profile Data Selection (Optional):** Based on user-defined criteria, it can select specific stellar profiles from certain evolutionary stages (e.g., at maximum luminosity, specific ages, or within the instability strip). This is particularly useful for subsequent pulsation analysis with GYRE.
* **Data Summarization:** Consolidates all extracted information into comprehensive summary files, typically in a tabular format (e.g., CSV), making it easy for further programmatic or manual inspection.

Input
-----

The MESA Run Analysis workflow requires access to your MESA simulation output directories. You specify the base directory where your MESA run folders are located, and an optional glob pattern to filter which runs should be processed.

* **``mesa_runs_base_dir``**: The top-level directory containing all your MESA run folders.
* **``mesa_runs_glob_pattern``** (Optional): A pattern (e.g., ``M*/*``) to specifically select which subdirectories (MESA runs) within ``mesa_runs_base_dir`` should be included in the analysis. If not specified, all subdirectories will be processed.

Output
------

Upon successful completion, the MESA Run Analysis workflow generates several key output files within your session's **``analysis_results``** directory (which is typically located inside your main ``output_dir``). This directory serves as the direct input for the Plotting and GYRE workflows.

* **``summary_data.csv`` (or custom name):** A comprehensive CSV file containing a row for each MESA run analyzed, summarizing all extracted history data, detected evolutionary events (like IS crossings, blue loop properties), and global parameters. This file is the primary input for the `Plotting Workflow <./plotting_workflow.rst>`_.
* **``filtered_profiles.csv`` (Optional):** If `profile_selection_criteria` are defined and `gyre_workflow.run_mode` is set to `FILTERED_PROFILES`, this CSV will list the paths to the selected MESA `profile.data` files along with their corresponding global parameters. This serves as the direct input for the `GYRE Workflow <./gyre_workflow.rst>`_.
* **Other supplementary files:** Depending on the configuration, additional files related to profile extraction or specific analysis steps might be generated.

Configuration Parameters
------------------------

The behavior of the MESA Run Analysis workflow is controlled by parameters within the `mesa_analysis_workflow` section of your `YAML configuration file <./yaml_config.rst>`_. Key parameters include:

* **``run_mesa_analysis_workflow``**: (Boolean) Set to `true` to enable this workflow. Default: `true`.
* **``mesa_runs_base_dir``**: (String) The path to the directory containing your MESA run folders.
* **``mesa_runs_glob_pattern``**: (String, Optional) A glob pattern to filter MESA run subdirectories.
* **``process_profiles``**: (Boolean) If `true`, ``mesalab`` will also process `profile.data` files according to `profile_selection_criteria`. Default: `false`.
* **``profile_selection_criteria``**: (Dictionary) Defines the rules for selecting specific profiles, e.g., based on model number, age, or location within the instability strip.
* **``instability_strip_detection``**: (Dictionary) Settings for how the instability strip crossings are identified.
* **``blue_loop_detection``**: (Dictionary) Settings for identifying and characterizing blue loops.
* **``summary_output_filename``**: (String) The desired filename for the main summary CSV output. Default: `summary_data.csv`.

For a complete list of parameters and their detailed descriptions, please refer to the `Understanding the YAML Configuration <./yaml_config.rst>`_ section.

Running this Workflow Independently
-----------------------------------

You can choose to run *only* the MESA Run Analysis workflow if you already have your MESA simulation outputs and wish to generate the summary data without immediately proceeding to plotting or GYRE analysis. This is useful for debugging or for multi-stage workflows.

To run only this part, ensure your YAML configuration file has:

.. code-block:: yaml

    mesa_analysis_workflow:
      run_mesa_analysis_workflow: true
      # ... (other analysis settings)

    plotting_workflow:
      run_plotting_workflow: false # Disable plotting
    
    gyre_workflow:
      run_gyre_workflow: false # Disable GYRE

Then, execute ``mesalab`` as usual:

.. code-block:: bash

    mesalab --config path/to/your_config_settings.yaml

The output will be saved to the ``analysis_results`` directory within your specified `output_dir`.