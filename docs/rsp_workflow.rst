.. _rsp_workflow:

RSP Workflow
=============

The RSP Workflow in `mesalab` integrates the RSP module of `MESA <https://https://docs.mesastar.org//>`_ to perform asteroseismic analysis on stellar models for MESA simulations. 

----

Purpose
-------

The RSP workflow within `mesalab` automates the execution of the RSP module across your MESA stellar evolution models. Its primary steps include:

* **MESA Model Selection:** The RSP workflow requires a prior selection of stellar models. It performs pulsation analysis exclusively on a pre-filtered subset of models, ensuring that computational resources aren't wasted on models that fall outside the instability strip. Currently, this workflow must be run together with the `mesalab` run analysis workflow, as it is not yet designed to read a standalone input CSV file. While the current analysis is focused on models that exhibit blue loop crossings, future versions of the code will offer the flexibility to run the analysis on all available stellar profiles, regardless of their evolutionary characteristics.
* **RSP Inlist Generation:** For each selected MESA model, a unique RSP inlist file (``inlist_rsp``) is created. This involves reading a user-defined template and programmatically setting the correct absolute path to the stellar profile within the inlist.
* **RSP Execution:** Runs the MESA RSP module for each generated inlist.

.. note::
    Currently, this workflow must be run together with the `mesalab` run analysis workflow, as it is not designed to be run as a stand-alone subprocess.  Due to this dependency, the RSP workflow is **currently only applicable to models identified as blue loop crossers**.

.. note::
    This workflow is responsible for *running* RSP and managing its direct outputs, and it does **not** include post-processing steps. The implementation of post-processing is planned for future development.

----

Input
-----

The RSP Workflow requires the **RSP inlist template** as an input.

**RSP Inlist Template**
The :code:`rsp_inlist_template_path` parameter, typically set to :code:`config/rsp.inlist_template`, specifies the absolute or relative path to an RSP inlist file. This file serves as a **template**; `mesalab` dynamically modifies it before each individual RSP run.

A typical GYRE template inlist should follow the conventional GYRE 7.0 setup, like::


&star_job

      show_log_description_at_start = .false.

      create_RSP_model = .true.

      save_model_when_terminate = .true.
      save_model_filename = 'rsp_final_M5.0Z0.0090Mod2073.mod'

      initial_zfracs = 6

      color_num_files=2
      color_file_names(2)='blackbody_johnson.dat'
      color_num_colors(2)=5

      set_initial_age = .true.
      initial_age = 0

      set_initial_model_number = .true.
      initial_model_number = 0
      
      set_initial_cumulative_energy_error = .true.
      new_cumulative_energy_error = 0d0
      
      pgstar_flag = .false.
   
/ ! end of star_job namelist

&eos
/ ! end of eos namelist

&kap
   Zbase = 0.003d0

      kap_file_prefix = 'a09'
      kap_lowT_prefix = 'lowT_fa05_a09p'
      kap_CO_prefix =   'a09_co'

/ ! end of kap namelist


&controls

    ! limit max_model_number as part of test_suite
      max_model_number = 1192 ! 16000

! RSP controls

      x_integer_ctrl(1) = 20 ! which period to check
   !  x_ctrl(1) = 18.2974 ! expected period (in days) 

      RSP_mass = 6d0
      RSP_Teff = 4892
      RSP_L = 4660
      RSP_X = 0.730d0
      RSP_Z = 0.003d0
   
      RSP_nmodes = 15

      
! solver
      use_gold2_tolerances = .true.

! output controls
   
      terminal_show_age_units = 'days'
      terminal_show_timestep_units = 'secs'
      terminal_show_log_dt = .false.
      terminal_show_log_age = .false.

      !num_trace_history_values = 2
      trace_history_value_name(1) = 'rel_E_err'
      trace_history_value_name(2) = 'log_rel_run_E_err'

      RSP_work_period = 5
      RSP_work_filename = 'work.data'

      photo_interval = 1000
      profile_interval = 4000
      history_interval = 10
      terminal_interval = 4000

/ ! end of controls namelist



&pgstar



/ ! end of pgstar namelist



.. note::
    The provided example ``gyre.in`` template is based on practices and examples demonstrated during the MESA Summer School 2022, led by Earl Bellinger. You can find more details at the `Asteroseismology Across the HRD <https://earlbellinger.com/mesa-summer-school-2022/index.html>`_ tutorial.


You can read more details about GYRE inlists on the `official documentation <https://gyre.readthedocs.io/>`_.


----

Output
------

All GYRE-related output files are saved to the ``gyre_output`` subdirectory within your `mesalab` session's main ``output_dir``. The output files are organized by creating subdirectories within the ``gyre_outputs``  dir. Subdirectories follow the naming convention of the original MESA model directories (e.g., ``run_5.0MSUN_z0.0090``). Within these subdirectories, further subdirectories are created based on the **profile numbers** corresponding to each pulsation run (e.g., ``profile00018``, ``profile00019``). Within each profile directory, you can find:

* **GYRE Inlist Files:** ``gyre_inlist_profileXX.in`` files (generated inlists) for each GYRE run.
* **Generated GYRE Data:** ``detailXXX.txt`` and ``summary.h5`` files, as configured in the ``gyre.in`` template.


Based on your ``gyre.in`` template, the final output directory structure follows the scheme below::

    output_dir/
    ├── gyre_outputs/
    │   ├── run_5.0MSUN_z0.0090/
    │   │   ├── profile00018/
    │   │   │   ├── gyre_inlist_profile18.in
    │   │   │   ├── summary.h5
    │   │   │   └── detail.txt
    │   │   ├── profile00019/
    │   │   │   ├── gyre_inlist_profile19.in
    │   │   │   ├── summary.h5
    │   │   │   └── detail.txt
    │   │   └── ... (additional profile directories as per the run)
    │   ├── run_5.0MSUN_z0.0100/
    │   │   ├── profile00018/
    │   │   │   ├── gyre_inlist_profile18.in
    │   │   │   ├── summary.h5
    │   │   │   └── detail.txt
    │   │   ├── profile00019/
    │   │   │   ├── gyre_inlist_profile19.in
    │   │   │   ├── summary.h5
    │   │   │   └── detail.txt
    │   │   └── ... (additional profile directories as per the run)

----

Configuration Parameters
------------------------

GYRE Workflow is controlled by parameters within the :ref:`YAML configuration <understanding_yaml_config>` file and the ``gyre.in`` template file.

* ``run_gyre_workflow``: (Boolean) Set to `true` to enable the execution of the full GYRE workflow. Default: `false`.
* ``gyre_inlist_template_path``: (String) The absolute or relative path to the GYRE inlist template file (e.g., ``config/gyre.in``). This template defines the general GYRE calculation settings, which `mesalab` then customizes for each specific stellar profile.

* ``run_mode``: (String) Specifies which MESA profiles the GYRE workflow should analyze:
    * ``ALL_PROFILES``: Processes all available profiles matching the configured :code:`mesa_profile_pattern`.
    * ``FILTERED_PROFILES``: Uses a subset of profiles identified by the `MESA Run Analysis Workflow` and listed in the file specified by :code:`filtered_profiles_csv_name`.

* ``enable_parallel``: (Boolean) If set to `true`, multiple GYRE runs will be executed concurrently, utilizing the available computational resources more efficiently. Default: `true`.
* ``num_gyre_threads``: (Integer) Specifies the number of OpenMP threads that each individual GYRE instance will utilize during its run. Default: `1`.
* ``max_concurrent_gyre_runs``: (Integer) When :code:`enable_parallel` is `true`, this parameter defines the maximum number of GYRE instances that can run simultaneously. Default: `4`.
* ``mesa_profile_pattern``: (String) A wildcard pattern (e.g., ``profile*.data.GYRE``) used by `mesalab` to identify MESA profile files within the relevant directories for processing. Default: ``profile*.data.GYRE``.
* ``mesa_profile_base_dir_relative``: (String) The relative path from a MESA run's top directory (e.g., ``/path/to/your/mesa_runs_grid/run_X.XMSUN_Z.XXXX``) to its specific LOGS folder where the profiles are located (e.g., ``LOGS``). Default: `LOGS`.


For a complete list of all `mesalab` parameters, including those in `general_settings` (e.g., `gyre_dir` which points to your GYRE installation), please refer to the :ref:`understanding_yaml_config` section.

----

Pre-requisites
--------------

.. note::
    The `mesalab` GYRE Workflow relies on a correct installation and configuration of **both** the external GYRE software and the MESA SDK. **It is ESSENTIAL to install these separately** before attempting to run this workflow. This version of `mesalab` is configured to run with GYRE version **7.0**.

**MESA SDK Installation**
    
GYRE Workflow of `mesalab` relies on the `MESA SDK` to provide the necessary compilers (like `gfortran`), libraries, and utilities that MESA uses to generate stellar profiles. It also ensures compatibility for reading MESA output files. Therefore, a working installation of the MESA SDK is necessary.

You can find detailed installation instructions on the `official MESA SDK website <http://user.astro.wisc.edu/~townsend/static.php?ref=mesasdk>`_.

* **Setting the** ``MESASDK_ROOT`` **Environment Variable (OR specifying path in YAML):**
    After successfully installing the MESA SDK, you **must** configure `mesalab` to find its root directory. This can be done in one of two ways:

    1.  **Recommended: Set the** ``ESASDK_ROOT`` **Environment Variable:**
        Set the ``MESASDK_ROOT`` environment variable to the root directory of your MESA SDK installation.

        * **On Linux/macOS (bash/zsh):**
            Add the following line to your `~/.bashrc`, `~/.zshrc`, or `~/.profile` file:

            .. code-block:: console
            
                $ export MESASDK_ROOT="/path/to/your/mesa_sdk_installation_root"

        Replace the example path with the actual, full path to your MESA SDK root directory and start a **new** terminal, or type `source ~/.bashrc`, `source ~/.zshrc`, or `source ~/.profile`.

    2.  **Alternative: Specify `mesasdk_root` Directly in the YAML Configuration:**
        You can also explicitly provide the full path to your MESA SDK root directory within the `general_settings` section of your `mesalab` configuration YAML file.
       
        .. code-block:: yaml

            general_settings:
                mesasdk_root: "/path/to/your/mesasdk" # Overrides MESASDK_ROOT environment variable
                # ...

**GYRE Installation**

You *must* have the GYRE pulsation code installed separately on your system. `mesalab` does not install GYRE for you; it only interacts with an existing GYRE installation. For GYRE **v7.0**, the official and comprehensive installation guide (including compilation steps) is available `here <https://gyre.readthedocs.io/en/v7.0/ref-guide/installation.html>`_.

Follow these instructions carefully to compile and install GYRE on your system.

* **Setting the** `GYRE_DIR` **Environment Variable (OR specifying paths in YAML):**
    After successfully installing GYRE, you **must** configure `mesalab` to find the GYRE executables. This can be done in one of two ways, with the `GYRE_DIR` environment variable being the most common and recommended:

    1.  **Recommended: Set the `GYRE_DIR` Environment Variable:**
        Set the `GYRE_DIR` environment variable to point to your GYRE installation's root directory and add its `bin` subdirectory to your system's `PATH` environment variable.

        * **On Linux/macOS (bash/zsh):**
            Add the following lines to your `~/.bashrc`, `~/.zshrc`, or `~/.profile` file:

            .. code-block:: console

                $ export GYRE_DIR="/path/to/your/gyre_installation_root"
                $ export PATH="$GYRE_DIR/bin:$PATH" # Add GYRE executables to your PATH

            Replace the example path with the actual, full path to your GYRE installation directory and start a **new** terminal, or type `source ~/.bashrc`, `source ~/.zshrc`, or `source ~/.profile`.

    2.  **Alternative: Specify** ``gyre_dir`` **Directly in the YAML Configuration:**
        As an alternative to setting environment variables, you can explicitly provide the full path to GYRE's `bin` directory within the `general_settings` section of your `mesalab` configuration YAML file. This is useful if you have multiple GYRE installations or prefer not to modify your system's environment variables.

        .. code-block:: yaml

            general_settings:
                gyre_dir: "/path/to/your/gyre/install/bin" # Points to GYRE's 'bin' directory
                # ...

        Replace the example path with the actual, full path to your GYRE `bin` directory.

----

**Troubleshooting**

* For more detailed information on diagnosing and resolving common GYRE-related issues (e.g., "command not found" errors, or unexpected workflow skips), please refer to the :ref:`trouble_shooting_gyre` entry in the Troubleshooting section, or consult the `official GYRE documentation <https://gyre.readthedocs.io/en/v7.0/index.html>`_.

----

Running this Workflow Independently
-----------------------------------

GYRE workflow can be run independently if you have already performed the MESA Run Analysis in a previous `mesalab` run, and you simply wish to run or re-run the pulsation analysis. This is particularly useful for, e.g., trying different GYRE ``inlist`` templates.

To run only this part, ensure your YAML configuration file has the following settings:

.. code-block:: yaml

    # Minimal configuration to run only the GYRE workflow
    general_settings:
        # Optional: Explicitly specify SDK and GYRE binary paths here
        # if you are NOT using environment variables (MESASDK_ROOT, GYRE_DIR)
        # mesasdk_root: "/path/to/your/mesasdk"
        # gyre_dir: "/path/to/your/gyre/install/bin"
        force_reanalysis: false

    blue_loop_analysis:
        analyze_blue_loop: false

    plotting_settings:
        generate_heatmaps: false                    
        generate_hr_diagrams: "none"                
        generate_blue_loop_plots_with_bc: false     

    gyre_workflow:
        run_gyre_workflow: true 
        gyre_inlist_template_path: "config/gyre.in"                                       
        run_mode: "FILTERED_PROFILES"               
        num_gyre_threads: 1                         # Number of OpenMP threads for each individual GYRE run
        enable_parallel: true                       # Enable/disable parallel execution of multiple GYRE runs
        max_concurrent_gyre_runs: 4                 # Maximum number of concurrent GYRE runs if enable_parallel is true
        mesa_profile_pattern: "profile*.data.GYRE"  # Wildcard pattern for MESA profile files (e.g., "profile*.data.GYRE")
        mesa_profile_base_dir_relative: "LOGS"      # Relative path from a MESA run directory to its LOGS folder (e.g., "LOGS")


Then, execute mesalab as usual:

.. code-block:: console

    $ mesalab --config path/to/your_config_settings.yaml

`mesalab` will look for the necessary MESA profile input (e.g., ``analysis_results/sorted_blue_loop_profiles.csv``) in the analysis_results directory relative to your specified ``output_dir`` from the previous analysis run.