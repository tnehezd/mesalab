Getting Started
===============

``mesalab`` is designed to as a command-line driven pipeline for the end-to-end analysis of MESA stellar evolution grid of simulations. The main goal is to simplify and automate post-processing tasks, allowing you to go from raw MESA output to the results with minimal manual intervention. 

This means: you can initiate the entire workflow from your terminal with all the necessary instructions. For ease of use, you can also lunch `mesalab` with all its input parameters and configuration settings from a YAML-formatted configuration file. 


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


GYRE Workflow Settings
~~~~~~~~~~~~~~~~~~~~~~

The ``mesalab`` `GYRE <https://gyre.readthedocs.io/>`_ workflow module provides tools to automate the execution of GYRE for stellar pulsation analysis. Before running any of the GYRE workflow commands in mesalab, **ensure that your MESA simulations have generated the necessary** ``profiles.data.GYRE`` **profile files!** These files are produced when ``write_pulse_data_with_profile = .true.`` and ``pulse_data_format = 'GYRE'`` are set in your MESA ``inlist_project`` configuration.

``--run-gyre-workflow``

	**Description**: Enables the full GYRE workflow within mesalab. When this flag is active, mesalab will identify specific MESA stellar models suitable for pulsation analysis, generate the necessary `GYRE <https://gyre.readthedocs.io/en/v7.0/>`_ v7.0 input files (.GYRE files), and optionally execute GYRE (assuming GYRE is properly installed and accessible in your system's PATH environment variable). If this flag is False, no GYRE-related files or processes will be initiated.

	**Example**: To activate the GYRE workflow: ``mesalab --config my_config.yaml --run-gyre-workflow``
	
	**Default**: True

``--gyre-config-path``
	**Description**: Specifies the path to your GYRE-specific configuration file (e.g., ``gyre_config.in``).

	**Example**: ``mesalab --config my_config.yaml --gyre-config-path ./custom_gyre_settings.in``

	**Default**: gyre_config.in

``--filtered-profiles-csv-name``
	**Description**: Defines the filename for the CSV file that lists the MESA profiles selected for GYRE analysis. By default, it is saved in the ``analysis_results`` directory in the blue loop analyzis step. , acts as a critical index, detailing which MESA models mesalab has chosen to generate GYRE inputs from. It typically includes information like the MESA profile number, key stellar parameters (e.g., mass, metallicity, effective temperature, luminosity), and the path to the corresponding MESA profile file.

	**Example**: ``mesalab --config my_config.yaml --filtered-profiles-csv-name my_gyre_candidates.csv``

	**Default**: ``/output/analysis_results/sorted_mass_Z_min_max.csv``


Understanding the YAML Configuration
------------------------------------

The heart of the ``mesalab`` pipeline lies in its YAML configuration file. This file acts as a blueprint, dictating everything from input data locations to the specific analyses, plotting options, and GYRE workflow settings.

Below is a heavily commented example of a typical ``mesalab`` configuration file (``my_config_settings.yaml``). Each section and parameter is explained to help you tailor it to your specific research needs.

.. code-block:: yaml

   # my_config_settings.yaml

   # General settings for the MesaLab run
   general_settings:
     input_dir: /path/to/your/mesa_runs/output_dir # <--- REQUIRED: Path to your MESA LOGS directory
                                                  #      e.g., /path/to/your/MESA_project
     output_dir: ./mesalab_output                 # <--- REQUIRED: Directory where MesaLab will save all its results
                                                  #      (e.g., summary tables, detail files, plots, GYRE inputs).
                                                  #      This will be created if it doesn't exist.
     force_reanalysis: False                      # Set to 'True' to force MesaLab to re-run all analyses
                                                  #      even if previous results are found in output_dir.
                                                  #      Set to 'False' to skip re-analysis if outputs exist.
     debug: False                                 # Set to 'True' to enable verbose debug logging for troubleshooting.
     inlist_name: inlist_project                  # The name of the MESA inlist file used for your simulations
                                                  #      (e.g., 'inlist_project', 'inlist_1.0M_Z0.02').
                                                  #      Used for identifying MESA runs and associated data.

   # Settings for blue loop analysis
   blue_loop_analysis:
     analyze_blue_loop: True                      # Set to 'True' to enable blue loop analysis.
                                                  #      If 'False', no blue loop specific data will be generated.
     blue_loop_output_type: 'summary'             # Type of blue loop data to output:
                                                  #      'summary': Generates aggregated metrics for each run (e.g., lifetime, min/max Teff/logL).
                                                  #      'detailed': Generates a CSV with all individual star_age points during the blue loop.

   # Settings for GYRE workflow integration
   gyre_workflow:
     run_gyre_workflow: True                      # Set to 'True' to enable the GYRE input generation and optional workflow.
                                                  #      Requires MESA to have generated profile data.
     gyre_config_path: gyre_config.in             # Path to your GYRE configuration file. This will be copied to the
                                                  #      GYRE output directory if the workflow is enabled.
     filtered_profiles_csv_name: sorted_mass_Z_min_max.csv # Name of the CSV file that will list the selected
                                                          #      MESA profiles for GYRE analysis.

   # Settings for plotting
   plotting_settings:
     generate_plots: True                         # Set to 'True' to enable the generation of all specified plots.
                                                  #      If 'False', no plot files will be created.
     generate_hr_diagrams: 'drop_zams'            # Type of HR diagrams to generate:
                                                  #      'drop_zams': HR diagram starting after ZAMS.
                                                  #      'full_hrd': Full HR diagram from pre-MS to termination.
                                                  #      'none': Do not generate HR diagrams.
     generate_heatmaps: True                      # Set to 'True' to generate heatmaps of blue loop properties
                                                  #      (e.g., lifetime, effective temperature ranges across the grid).
     generate_blue_loop_plots_with_bc: True       # Set to 'True' to generate specific blue loop plots using
                                                  #      bolometric corrections (requires MIST BC grid setup).