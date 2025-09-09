.. _gyre_workflow:

GYRE Workflow
=============

The GYRE Workflow in `mesalab` integrates the `GYRE <https://gyre.readthedocs.io/>`_ pulsation code to perform asteroseismic analysis on stellar models for MESA simulations. This workflow is crucial for understanding the pulsational properties and internal structures of stars.

----

Purpose
-------

The GYRE workflow within `mesalab` automates the execution of the GYRE pulsation code across your MESA stellar evolution models. Its primary steps include:

* **MESA Profile Selection:** Identifies and selects MESA stellar profiles (``profile*.data.GYRE`` files) for GYRE analysis. This can be configured to process either all available profiles from your MESA runs or the filtered subset (blue loop crossers) based on previous `mesalab` analysis results (e.g., from ``FILTERED_PROFILES`` mode).
* **GYRE Inlist Generation:** For each selected MESA profile, a unique GYRE inlist file (``profile_``) is created. This involves reading a user-defined template and programmatically setting the correct absolute path to the stellar profile within the inlist.
* **GYRE Execution:** Runs the GYRE for each generated inlist. The workflow supports both sequential and parallel execution of multiple GYRE runs to utilize available system resources efficiently.

.. note::
    This workflow is responsible for *running* GYRE and managing its direct outputs, and it does **not** include post-processing steps. The implementation of post-processing is planned for future development.

----

Input
-----

The GYRE Workflow requires two primary inputs: **MESA stellar profile data** and a **GYRE inlist template**.

**GYRE Inlist Template**
The :code:`gyre_inlist_template_path` parameter, typically set to :code:`config/gyre.in`, specifies the absolute or relative path to a GYRE inlist file. This file serves as a **template**; `mesalab` dynamically modifies it before each individual GYRE run to substitute the correct MESA profile filename and other run-specific parameters defined within the workflow.

**MESA Profile Selection** (:code:`run_mode`)
The :code:`gyre_workflow.run_mode` parameter in the configuration YAML file sets how MESA stellar profiles are selected for GYRE analysis. Profiles are located by matching the :code:`mesa_profile_pattern` (e.g., :code:`profile*.data.GYRE`) within the :code:`mesa_profile_base_dir_relative` (e.g., :code:`LOGS`) directory of each discovered MESA run.

* :code:`FILTERED_PROFILES`: In this mode, `mesalab` expects the ``sorted_blue_loop_profiles.csv`` file to be present in the ``analysis_results`` directory (generated in the previus steps). Only profiles listed in this file will be processed.
* :code:`ALL_PROFILES`: When set to :code:`ALL_PROFILES`, `mesalab` will process every MESA profile file it finds that matches the configured :code:`mesa_profile_pattern` within the specified :code:`mesa_profile_base_dir_relative` for each discovered MESA run directory.

A typical GYRE template inlist should follow the conventional GYRE 7.0 setup, like::

    &constants
    /

    &model
        model_type = 'EVOL'
        file = 'profile10.data.GYRE'
        file_format = 'MESA'
    /


    &mode
        l = 0
        n_pg_max = 20
    /


    &osc
        outer_bound = 'JCD'
    /

    &rot
    /

    &num
        diff_scheme = 'COLLOC_GL4' ! 4th-order collocation scheme for difference equations
    /   

    &scan
        grid_type = 'LINEAR'
        freq_min = 0.2
        freq_max = 1
        n_freq = 100
        freq_units = 'ACOUSTIC_CUTOFF'
    /

    &grid
        w_osc = 10 
        w_exp = 2 
        w_ctr = 10 
    /

    &tides_output
    /

    &ad_output
        summary_file = 'summary.h5'               
        summary_item_list = 'l,n_p,n_g,n_pg,freq,omega,E_norm,eta' ! Items to appear in summary file
        detail_file_format = 'TXT'
        detail_template = 'detail.l%l.n%n.TXT'     
        detail_item_list = 'id,l,n_pg,omega,x,xi_r,xi_h,eta,freq,W,dW_dx,kap_T,kap_rho,P,rho,T,Gamma_1'
        freq_units = 'CYC_PER_DAY'
    /

    &nad_output
    /


.. note::
    The provided example ``gyre.in`` template is based on practices and examples demonstrated during the MESA Summer School 2022, led by Earl Bellinger. You can find more details at the `Asteroseismology Across the HRD <https://earlbellinger.com/mesa-summer-school-2022/index.html>`_ tutorial.


You can read more details about GYRE inlists on the `official documentation <https://gyre.readthedocs.io/>`_.


----

Output
------

All GYRE-related output files are saved to an output directory defined in the YAML file (``gyre_output_subdir``, ``gyre_outputs`` by default) within your `mesalab` session's main ``output_dir``. Subdirectories follow the naming convention of the original MESA model directories (e.g., ``run_5.0MSUN_z0.0090``). Within these subdirectories, further subdirectories are created based on the **profile numbers** corresponding to each pulsation run (e.g., ``profile00018``, ``profile00019``). Within each profile directory, you can find:

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
* ``gyre_inlist_template_path``: (String) The absolute or relative path to the GYRE inlist template file (e.g., ``config/gyre.in``). This template defines the general GYRE settings.

* ``run_mode``: (String) Specifies which MESA profiles the GYRE workflow should analyze:
    * ``ALL_PROFILES``: Processes all available profiles matching the configured :code:`mesa_profile_pattern`.
    * ``FILTERED_PROFILES``: Uses a subset of profiles identified by the `MESA Run Analysis Workflow` and listed in the file specified by :code:`filtered_profiles_csv_name`.

* ``gyre_output_subdir``: Specifies the subdirectory where all the output files from the GYRE runs will be saved, ``./gyre_outputs`` by deafault.
* ``enable_gyre_parallel``: (Boolean) If set to `true`, multiple GYRE runs will be executed concurrently, utilizing the available computational resources more efficiently. Default: `true`.
* ``num_gyre_threads``: (Integer) Specifies the number of OpenMP threads that each individual GYRE instance will utilize during its run. Default: `1`.
* ``max_concurrent_gyre_runs``: (Integer) When :code:`enable_gyre_parallel` is `true`, this parameter defines the maximum number of GYRE instances that can run simultaneously. Default: `4`.
* ``mesa_profile_pattern``: (String) A wildcard pattern (e.g., ``profile*.data.GYRE``) used by `mesalab` to identify MESA profile files within the relevant directories for processing. Default: ``profile*.data.GYRE``.
* ``mesa_profile_base_dir_relative``: (String) The relative path from a MESA run's top directory (e.g., ``/path/to/your/mesa_runs_grid/run_X.XMSUN_Z.XXXX``) to its specific LOGS folder where the profiles are located (e.g., ``LOGS``). Default: `LOGS`.


For a complete list of all `mesalab` parameters, including those in `general_settings`, please refer to the :ref:`understanding_yaml_config` section.

----

Pre-requisites
--------------

.. warning::
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

* For more detailed information on diagnosing and resolving common GYRE-related issues (e.g., "command not found" errors, or unexpected workflow skips), please refer to the :ref:`GYRE Workflow Skipped or Failed <trouble_shooting_gyre>` entry in the Troubleshooting section, or consult the `official GYRE documentation <https://gyre.readthedocs.io/en/v7.0/index.html>`_.

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
        input_dir: "/path/to/your/MESA_grid/"

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
        gyre_output_subdir: "./gyre_outputs"            # Subdirectory within 'output_dir' to store GYRE runs (default: gyre_outputs)         
        num_gyre_threads: 1                             # Number of OpenMP threads for each individual GYRE run
        enable_gyre_parallel: true                      # Enable/disable parallel execution of multiple GYRE runs
        max_concurrent_gyre_runs: 4                     # Maximum number of concurrent GYRE runs if enable_gyre_parallel is true
        mesa_profile_pattern: "profile{:05d}.data.GYRE" # Example: "profile00042.data.GYRE"
        mesa_profile_base_dir_relative: "LOGS"          # Relative path from a MESA run directory to its LOGS folder (e.g., "LOGS")


Then, execute `mesalab` as usual:

.. code-block:: console

    $ mesalab --config path/to/your_config_settings.yaml

`mesalab` will look for the necessary MESA profile input (e.g., ``analysis_results/sorted_blue_loop_profiles.csv``) in the analysis_results directory relative to your specified ``output_dir`` from the previous analysis run.