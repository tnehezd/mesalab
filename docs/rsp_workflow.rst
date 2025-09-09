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

A typical RSP template inlist should follow the conventional MESA RSP setup, like::



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
    The provided example ``rsp.inlist_template`` template is based on the `Cepheid MESA test suite <https://docs.mesastar.org/en/latest/test_suite/rsp_Cepheid.html>`_.


----

Output
------

All RSP-related output files are saved to the ``rsp_output_subdir`` (e.g., ``rsp_outputs``) subdirectory within your `mesalab` session's main ``output_dir``. Subdirectories follow the naming convention of the original MESA model directories (e.g., ``run_5.0MSUN_z0.0090``). Within these subdirectories, further subdirectories are created based on the **model numbers** corresponding to each pulsation run (e.g., ``model2073``). Within each profile directory, you can find:

* **RSP Inlist Files:** ``inlist_rsp`` file (generated inlists) for MESA RSP run.
* **Final model:** The final model of the MESA RSP run (``rsp_final_M<mass>Z<metallicity>.mod``).
* **LOGS dir:** This directory contains RSP output files for each modes: ``history.data``, ``LINA_eigen<mode>.data``, ``LINA_work<mode>.data``, ``LINA_period_growth.data``, ``profile1.data``, ``profiles.index``
* **photos dir:**


Based on your ``rsp.inlist_template``, the final output directory structure follows the scheme below::

    example/MESA_grid_output/
    └── rsp_outputs/ # Example MESA run directory for profile00030
        ├── run_5.0MSUN_z0.0090/
        │   ├── model2073
        │   │   ├── LOGS
        │   │   │    ├── LINA_eigen1.data
        │   │   │    ├── LINA_work1.data
        │   │   │    ├── LINA_period_growth.data
        │   │   │    ├── history.data
        │   │   │    ├── profile1.data
        │   │   │    ├── profiles.index
        │   │   │    └── ... (additonal eigen and work data files)
        │   │   ├── photos
        │   │   │    ├── 1000
        │   │   │    └── x200
        │   │   ├── inlist_rsp
        │   │   └── rsp_final_M5.0Z0.0090Mod2073.mod                        
        │   └── ... (additional model directories as per the run)
        └── ... (additional run directories as per the run)

----

Configuration Parameters
------------------------

RSP Workflow is controlled by parameters within the :ref:`YAML configuration <understanding_yaml_config>` file and the ``rsp.inlist_template`` file.

* ``run_rsp_workflow``: (Boolean) Set to `true` to enable MESA RSP run. Default: `false`.
* ``rsp_inlist_template_path``: (String) The absolute or relative path to the RSP inlist template file (e.g., ``config/rsp.inlist_template``). This template defines the general RSP settings.
* ``rsp_output_subdir``: Specifies the subdirectory for MESA RSP outputs, ``./rsp_outputs`` by deafault.
* ``enable_rsp_parallel``: (Boolean) If set to `true`, multiple RSP runs will be executed concurrently, utilizing the available computational resources more efficiently. Default: `true`.
* ``num_rsp_threads``: (Integer) Specifies the number of OpenMP threads that each individual RSP instance will utilize during its run. Default: `1`.
* ``max_concurrent_rsp_runs``: (Integer) When :code:`enable_parallel` is `true`, this parameter defines the maximum number of RSP instances that can run simultaneously. Default: `4`.


For a complete list of all `mesalab` parameters, including those in `general_settings`, please refer to the :ref:`understanding_yaml_config` section.

----

Pre-requisites
--------------

.. warning::
    The `mesalab` RSP Workflow relies on a correct installation and configuration **both** of MESA SDK and MESA. **It is ESSENTIAL to install these separately** before attempting to run this workflow. This version of `mesalab` is **tested on MESA version 23.05.1**.

**MESA SDK Installation**
    
RSP Workflow of `mesalab` relies on the `MESA SDK` to provide the necessary compilers (like `gfortran`), libraries, and utilities that MESA uses to generate stellar profiles. It also ensures compatibility for reading MESA output files. Therefore, a working installation of the MESA SDK is necessary.

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

**MESA Installation**

You *must* have MESA installed separately on your system. `mesalab` does not install MESA for you; it only interacts with an existing MESA installation. For MESA **23.05.1**, the official and comprehensive installation guide (including compilation steps) is available `here <https://docs.mesastar.org/en/23.05.1/>`_.

Follow these instructions carefully to compile and install MESA on your system.

* **Setting the** `MESA_DIR` **Environment Variable (OR specifying ``mesa_dir`` path in YAML):**
    After successfully installing MESA, you **must** configure `mesalab` to find the MESA ``star`` executable. This can be done in one of two ways, with using the environment variable being the most common and recommended.

    1.  **Recommended: Set the `MESA_DIR` Environment Variable:**
        Set the `MESA_DIR` environment variable to point to your MESA installation's root directory. This is the standard convention for all MESA-related software.

        * **On Linux/macOS (bash/zsh):**
            Add the following lines to your `~/.bashrc`, `~/.zshrc`, or `~/.profile` file:

            .. code-block:: console

                $ export MESA_DIR="/path/to/your/mesa_installation_root"

            Replace the example path with the actual, full path to your MESA installation directory and start a **new** terminal, or type `source ~/.bashrc`, `source ~/.zshrc`, or `source ~/.profile`.



    2.  **Alternative: Specify** ``mesa_binary_dir`` **Directly in the YAML Configuration:**
        As an alternative to setting an environment variable, you can explicitly provide the full path to the directory containing the ``rn`` executable within the ``general_settings`` section of your `mesalab` configuration YAML file. This is useful if you have multiple MESA installations or prefer not to modify your system's environment variables.

        .. code-block:: yaml

            general_settings:
                mesa_binary_dir: "/path/to/your/mesa/star/work" # Points to MESA executable (`rn`).
                # ...

        Replace the example path with the actual, full path to your MESA `star/work` directory.

----


**Troubleshooting**

* For more detailed information on diagnosing and resolving common MESA-related issues (e.g., "command not found" errors, or unexpected workflow skips), please refer to the :ref:`RSP Workflow Skipped or Failed <trouble_shooting_mesa>` entry in the Troubleshooting section, or consult the `official MESA documentation <https://docs.mesastar.org/en/23.05.1/>`_.

----

Running the Workflow
--------------------

The current version of `mesalab` is designed as RSP workfloe cannot be run independently and is only designed to function as an integrated part of a preceding analysis. However, future development aims to enable it to run as a standalone process.

To run RSP workflow, ensure your YAML configuration file has the following settings:

.. code-block:: yaml

    # Minimal configuration to run only the RSP workflow
    general_settings:
        input_dir: /path/to/your/MESA_grid/
        output_dir: MESA_grid_output
        # Optional: Explicitly specify SDK and MESA binary paths here
        # if you are NOT using environment variables (MESASDK_ROOT, MESA_DIR)
        # mesasdk_root: "/path/to/your/mesasdk"
        # mesa_bin_dir: "/path/to/your/mesa/star/work"
        force_reanalysis: True

    blue_loop_analysis:
      analyze_blue_loop: True
      blue_loop_output_type: "summary"

    plotting_settings:
        generate_heatmaps: false                    
        generate_hr_diagrams: "none"                
        generate_blue_loop_plots_with_bc: false     

    gyre_workflow:
      run_gyre_workflow: False

    rsp_workflow:
      run_rsp_workflow: true
      rsp_inlist_template_path: "config/rsp.inlist_template"
      rsp_run_timeout: 300
      enable_rsp_parallel: true
      num_rsp_threads: 1
      max_concurrent_rsp_runs: 4 # Typically 1-2x your number of physical CPU cores


Then, execute `mesalab` as usual:

.. code-block:: console

    $ mesalab --config path/to/your_config_settings.yaml


`mesalab` will search for the defined input data, perform the blue loop analysis, and then store the resulting output in the specified `output_dir` directory.