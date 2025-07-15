Getting Started
===============

``mesalab`` is designed to as a command-line driven pipeline for the end-to-end analysis of MESA stellar evolution grid of simulations. The main goal is to simplify and automate post-processing tasks, allowing you to go from raw MESA output to the results with minimal manual intervention. 

This means: you can initiate the entire workflow from your terminal with all the necessary instructions. For ease of use, you can also lunch `mesalab` with all its input parameters and configuration settings from a YAML-formatted configuration file. 

---


Running ``mesalab``
-------------------

Once ``mesalab`` is installed, and you have your grid of MESA simulations ready, the pipeline can be run easily. The most easy way is to interact the pipeline with the yaml configuration file.  **It's important to note that any options provided directly via command-line flags will always override settings defined in your configuration file.**



.. code-block:: console

 $ mesalab --config /path/to/your/my_config_settings.yaml


``mesalab``
	**Description**: This is the **main executable** for the pipeline. Make sure it's accessible in your system's PATH after installation.

``--config``
	**Description**: This is the **primary flag** that tells mesalab to load all its operational parameters and analysis instructions from the **specified YAML file**.

path/to/your/``my_config_settings.yaml``
	**Description**: This should be the **full or relative path to your custom YAML configuration file**.

This runs the entire analytical workflow as defined by your configuration.

---


Command-line Flags
------------------

Beyond the essential ``--config`` flag, ``mesalab`` offers a few other command-line flags to control execution behavior or retrieve information. These are typically optional.

``--debug`` (or ``-d``)
   **Description**: **Enables verbose debugging output.** When this flag is present, `mesalab` prints more detailed messages to the console, which can be useful for troubleshooting issues or understanding the execution flow.
   
   **Example**: ``mesalab --config my_config.yaml --debug``

``--version`` (or ``-v``)
   **Description**: **Displays the current version of `mesalab`** and then exits.
   
   **Example**: ``mesalab --version``

``--help`` (or ``-h``)
   **Description**: **Shows a help message** with all available command-line options and their descriptions, then exits.
   
   **Example**: ``mesalab --help``


---


I/O Configuration
~~~~~~~~~~~~~~~~~



``--input-dir`` (or ``-i``)
	**Description**: This contains the **base directory that contains the MESA grid**.

	**Example**: /path/to/your/MESA_project

``--output-dir`` (o ``-o``)
   **Description**: This specifies the **base directory where all** ``mesalab`` **analysis results will be saved.** This includes generated summary tables (e.g., ``summary_metrics.csv``), detailed individual run analysis files, plots (HR diagrams, heatmaps), and any GYRE input/output files if the GYRE workflow is enabled. If the specified directory does not exist, ``mesalab`` will attempt to create it.

   **Example**: ``/path/to/your/mesalab_results``
   
   **Default**: ``./mesalab_output``

``--inlist_-name``
   **Description**: Name of your MESA ``inlist`` files that helps ``mesalab`` identify associated runs. This should typically be the base name of the main inlist file, without the full path or extension, unless your inlist names explicitly include them.
   
   **Example**: If your MESA runs use ``inlist_1.0M_Z0.02`` or ``inlist_project``, you would specify ``inlist_1.0M_Z0.02`` or ``inlist_project``.
   
   **Default**: ``inlist_project``	


---


Blue Loop Analyzis Settings
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This section of the configuration is dedicated to identifying and characterizing stellar blue loops within your MESA simulation data. A blue loop refers to a phase in the evolution of intermediate-mass stars (typically after the Main Sequence) where they temporarily move to hotter (bluer) and brighter regions of the Hertzsprung-Russell diagram before resuming their redward evolution. When enabled, ``mesalab`` performs a specialized analysis to detect these blueward excursions in the Hertzsprung-Russell (HR) diagram.

The blue loop analysis process involves several key steps:

* **Mass Filtering:** It first checks if the initial mass of the star meets a minimum threshold, as classic blue loops are typically found in intermediate-mass stars (the threshold is set to 2.0 :math:`\text{M}_\odot`).
* **Evolutionary Phase Identification:** ``mesalab`` then precisely identifies key evolutionary points, such as the end of the Main Sequence (hydrogen exhaustion) and the coolest (reddest) point on the Red Giant Branch (RGB) before any blueward movement. This RGB tip must be on the red side of the Instability Strip.
* **Blue Loop Candidate Window:** A specific time window for the blue loop candidate is defined, generally spanning from the RGB tip through the significant core helium burning phase.
* **Instability Strip Crossings:** Within this window, the code tracks the star's trajectory in the HR diagram, counting how many times it enters and exits the predefined Instability Strip. This helps in confirming and characterizing the blue loop.

These flags control the execution and output of this detailed blue loop analysis.

``--analyze-blue-loop``
   **Description**: Toggle this switch to **enable the analysis of stellar blue loops**. When this flag is enabled, ``mesalab`` will detect and extract data for these blue loop phases from your MESA simulation outputs, making the results available for further processing and plotting.

   **Example**: ``mesalab --analyze-blue-loop True/False``

   **Default**: ``True``

``--blue-loop-output-type``
   **Description**: Specifies the **content of the detailed blue loop analysis output files**. These files are named following the pattern ``detail_zX.XXXX.csv`` (where zX.XXXX represents the metallicity) and are saved into a ``/detail_files`` subdirectory within your specified output directory. Only stars that are identified as undergoing a blue loop will have their data included in these output files.


   * ``summary``: When enabled, each csv file will include: ``initial_mass``, ``initial_Z``, ``star_age``, ``model_number``, ``log_Teff``, ``log_L``, and ``log_g``. This option is useful for quickly comparing essential blue loop properties across different simulations without loading the full history.

   * ``all``: When enabled, the csv files will contain **all MESA history data columns** available from the input ``history.data`` file. This provides the most comprehensive dataset for in-depth, point-by-point analysis and custom plotting of individual blue loop trajectories.
 
   **Example**: To generate detailed files with a summary set of columns: ``mesalab --blue-loop-output-type summary``
               
               To generate detailed files with all history columns: ``mesalab --blue-loop-output-type all``
  
   **Default**: ``summary``

---


Plot Settings
~~~~~~~~~~~~~

``--generate-heatmaps``
	**Description**: Creates a heatmap that visualizes the **number of Instability Strip crossings during the blue loop phase** across your stellar evolution grid saved into the ``/plots`` subdirectory. This heatmap is plotted in the **initial mass (M) and metallicity (Z) parameter space**, offering a quick visual overview of how the crossing count changes across your simulations.

	**Example**: To generate heatmaps: ``mesalab --generate-heatmaps``

	**Default**: ``False``



``--generate-hr-diagrams``
   **Description**: Controls the generation of Hertzsprung-Russell (HR) diagrams for each MESA simulation run. These HR diagrams are saved into the ``/plots`` subdirectory, organized by metallicity (Z), and are generated **regardless of whether a blue loop is detected** for a specific run. Each plot currently displays **four key columns** for visual inspection. This feature is primarily for a **quick visual check** of the evolutionary tracks.

   * ``'none'``: No HR diagrams will be generated.

   * ``'all'``: Generates HR diagrams showing the **full evolutionary track** from the pre-Main Sequence phase to the end of the simulation for each run.

   * ``'drop_zams'``: Generates HR diagrams starting **after the Zero-Age Main Sequence (ZAMS)**, focusing on the post-main sequence evolution.

   **Example**: To generate full HR diagrams: ``mesalab --generate-hr-diagrams all``

   **Default**: ``none``


``--generate-blue-loop-plots-with-bc``
   **Description**: When enabled, this flag generates specialized plots for **all blue loop models identified after the analysis, incorporating bolometric corrections (BCs)**. These plots are saved directly into the ``/plots`` output directory, providing a comprehensive visual overview of the blue loop phase across your grid. The following combined plots are generated:

   * **HR Diagram** (``HRD_all_blue_loop_data.png``): An HR diagram showing all blue loop models, with points colored by metallicity (Z).
   * **Color-Magnitude Diagram** (``CMD_Gaia_all_blue_loop_data.png``): A CMD, currently using Gaia's :math:`G_{BP}-G_{RP}` color, with points colored by metallicity (Z).
   * **Log L - Log g Diagram** (``LogL_LogG_all_blue_loop_data.png``): A diagram plotting :math:`\log(L/L_{\odot})` against :math:`\log g`, with points colored by metallicity (Z).

   The bolometric corrections used for these plots are calculated from the `MIST bolometric correction tables <https://waps.cfa.harvard.edu/MIST/model_grids.html#bolometric>`_, requiring a correctly set up MIST grid in your environment.
   
   **Example**: ``mesalab --config my_config.yaml --generate-blue-loop-plots-with-bc``
   
   **Default**: ``False``

---


GYRE Workflow Settings
~~~~~~~~~~~~~~~~~~~~~~

The ``mesalab`` `GYRE <https://gyre.readthedocs.io/>`_ workflow module provides tools to automate the execution of GYRE for stellar pulsation analysis. Before running any of the GYRE workflow commands in mesalab, **ensure that your MESA simulations have generated the necessary** ``profiles.data.GYRE`` **profile files!** These files are produced when ``write_pulse_data_with_profile = .true.`` and ``pulse_data_format = 'GYRE'`` are set in your MESA ``inlist_project`` configuration.

``--run-gyre-workflow``
    **Description**: Enables the full GYRE workflow within ``mesalab``. When this flag is active, ``mesalab`` will identify specific MESA stellar models suitable for pulsation analysis, generate the necessary GYRE v7.0 input files (.GYRE files), and optionally execute GYRE (assuming GYRE is properly installed and accessible in your system's PATH environment variable). If this flag is ``False``, no GYRE-related files or processes will be initiated.
  
    **Example**: To activate the GYRE workflow: ``mesalab --config my_config.yaml --run-gyre-workflow``
  
    **Default**: ``True``

``--gyre-inlist-template-path``
    **Description**: Specifies the **absolute or relative path to your GYRE inlist template file** (e.g., ``gyre.in``). This template is read by ``mesalab``, which then inserts the specific MESA profile path for each GYRE run, creating a temporary inlist for the calculation. This provides flexibility, allowing your GYRE template to be stored anywhere on your system.
   
    **Example**: ``mesalab --config my_config.yaml --gyre-inlist-template-path /home/user/my_templates/gyre.in``
   
    **Default**: ``config/gyre.in`` (This default assumes ``gyre.in`` is in a ``config`` sub-directory relative to where ``mesalab`` is run).

``--run-mode``
    **Description**: Determines which MESA profiles are processed by GYRE.
    * ``ALL_PROFILES``: GYRE will be run for every single MESA profile available that matches the ``mesa_profile_pattern``, across **all MESA run directories identified within the `input_dir`**.
    * ``FILTERED_PROFILES``: GYRE will only be run for profiles identified in the ``filtered_profiles_csv_name`` CSV file, typically generated by ``mesa_analyzer`` during the blue loop analysis.
    
    **Example**: To run GYRE on all profiles: ``mesalab --run-mode ALL_PROFILES``
    
    **Default**: ``FILTERED_PROFILES``


``--num-gyre-threads``
    **Description**: The number of OpenMP threads that each *individual* GYRE process should use. This affects the performance of a single GYRE calculation.
 
    **Example**: ``mesalab --num-gyre-threads 4``
 
    **Default**: ``1``

``--enable-parallel``
    **Description**: Set to ``true`` to enable parallel execution of multiple GYRE runs simultaneously. This is highly recommended for large grids to speed up the workflow. If ``false``, GYRE runs will be executed sequentially.
  
    **Example**: ``mesalab --enable-parallel True``
  
    **Default**: ``False``

``--max-concurrent-gyre-runs``
    **Description**: When ``enable_parallel`` is ``true``, this specifies the maximum number of concurrent GYRE processes that ``mesalab`` will launch at any given time. Adjust this based on your system's CPU core count and available RAM.
  
    **Example**: ``mesalab --max-concurrent-gyre-runs 8``
  
    **Default**: ``4``

``--filtered-profiles-csv-name``
    **Description**: The name of the CSV file that ``mesa_analyzer`` generates containing the filtered MESA profiles (e.g., for blue loop analysis). This file is used as input for GYRE when ``run_mode`` is set to ``FILTERED_PROFILES``. By default, it is saved in the ``analysis_results`` directory within your ``output_dir``.
 
    **Example**: ``mesalab --filtered-profiles-csv-name my_gyre_candidates.csv``
 
    **Default**: ``sorted_mass_Z_min_max.csv`` (expected in ``output_dir/analysis_results/``)

``--mesa-profile-pattern``
    **Description**: This pattern defines how MESA profile filenames are matched and expected. For example, ``"profile*.data.GYRE"`` will match files like ``"profile00042.data.GYRE"``. The ``*`` wildcard is used for discovery in ``ALL_PROFILES`` mode and will be automatically replaced with the appropriate profile number (e.g., ``'00042'``) when constructing filenames for ``FILTERED_PROFILES`` mode.
   
    **Example**: ``mesalab --mesa-profile-pattern "profile*.data.GYRE"``
   
    **Default**: ``"profile*.data.GYRE"``

``--mesa-profile-base-dir-relative``
    **Description**: The relative path from a MESA run's top directory to its ``LOGS`` folder. This is where MESA profiles (e.g., ``profile*.data.GYRE``) are typically located.
  
    **Example**: ``mesalab --mesa-profile-base-dir-relative "LOGS"``
  
    **Default**: ``"LOGS"``

---

Understanding the YAML Configuration
------------------------------------

The base of the ``mesalab`` pipeline lies in its YAML configuration file. This file coordinates everything from input data locations to specific analyses, plotting options, and GYRE workflow settings. It uses a **nested structure** where settings are organized under logical headings, making it easy to read and manage.

Below is a commented example of a typical ``mesalab`` configuration file (``my_config_settings.yaml``). Each parameter is explained inline to help you tailor it to your specific research needs.

.. code-block:: yaml

    # my_config.yaml - Example Configuration for mesalab
    # --- General Settings ---
    general_settings: # General settings for the MesaLab run
      input_dir: /path/to/your/mesa_runs_grid # REQUIRED: Base directory for MESA simulation subdirectories.
      output_dir: ./mesalab_output # Directory where all mesalab outputs will be saved.
      inlist_name: "inlist_project" # Name of the MESA inlist file (e.g., 'inlist_project').
      force_reanalysis: false # Set to 'true' to force re-analysis even if outputs exist.
      debug: false # Set to 'true' for verbose debug logging.
    # mesasdk_root: /path/to/your/mesasdk_root # Optional: Override MESASDK_ROOT env variable.
    # gyre_dir: /path/to/your/gyre_installation_bin_directory # Optional: Override GYRE_DIR env variable.

    # --- Analysis Options ---
    blue_loop_analysis: # Settings for blue loop analysis
      analyze_blue_loop: true # Set to 'true' to enable blue loop analysis.
      blue_loop_output_type: "summary" # Type of blue loop data to output: 'summary' or 'all'.

    # --- Plotting Settings ---
    plotting_settings: # Settings for plotting
      generate_heatmaps: true # Set to 'true' to generate heatmaps.
      generate_hr_diagrams: "all" # Type of HR diagrams to generate: 'none', 'all', or 'drop_zams'.
      generate_blue_loop_plots_with_bc: true # Set to 'true' to generate blue loop plots with bolometric corrections.

    # --- GYRE Workflow Settings ---
    gyre_workflow: # Settings for GYRE workflow integration
      run_gyre_workflow: true # Set to 'true' to enable the GYRE workflow.
      gyre_inlist_template_path: "/path/to/your/mesalab/config/gyre.in" # Path to your GYRE inlist template file.
      run_mode: FILTERED_PROFILES # Which MESA profiles to process: 'ALL_PROFILES' or 'FILTERED_PROFILES'.
      num_gyre_threads: 4 # Number of OpenMP threads for each GYRE process.
      enable_parallel: true # Enable/disable parallel GYRE runs.
      max_concurrent_gyre_runs: 8 # Maximum concurrent GYRE processes.
      filtered_profiles_csv_name: "sorted_mass_Z_min_max.csv" # CSV file listing filtered MESA profiles for GYRE.
      mesa_profile_pattern: "profile*.data.GYRE" # Pattern for MESA profile filenames (e.g., "profile*.data.GYRE").
      mesa_profile_base_dir_relative: "LOGS" # Relative path from MESA run's top directory to its 'LOGS' folder.