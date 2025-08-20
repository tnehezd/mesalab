Tutorial 3: Running GYRE and RSP with `mesalab`: ``example/MESA_grid_gyre``, ``example/MESA_grid_gyre``
=================================================================================================================

The base of the ``MESA_grid`` dataset is discussed in :ref:`Tutorial 2. <tutorial_2>`that demonstrates how the `mesalab` pipeline filters, processes, and prepares stellar evolution data for pulsation analysis.

Since  pulsation analysis with GYRE or RSP requires the installation of MESA, MESASDK and GYRE it is challenging to set up and run interactively on a platform like Google Colab. Therefore, this tutorial walks through what the output of a successful run looks like utilizing GYRE and/or RSP workflow without an interacitve Google Colab notebook.


MESA Grid dataset
-----------------

This set of runs are **actual MESA stellar evolution outputs**, providing standard ``profile``, ``history``, and ``inlist`` files for demonstrating the analysis.

* **Grid Structure & Parameters:** It contains a **2x2 grid** of models with initial masses of **4 and 5 solar masses** and metallicities (Z) of **0.0090 and 0.0100**.

* **Evolutionary Coverage:** Simulations cover stellar evolution from the **pre-main sequence (pre-MS)** through to **after the blue loop phase**.

* **Blue Loop Behavior:** A key feature is the differing blue loop behavior: **5 Msun models exhibit blue loop crossings**, while **4 Msun models do not**. This highlights `mesalab`'s capability to identify and filter profiles based on evolutionary characteristics.

* **Location:** These models are found in the ``example/MESA_grid`` directory.

* **YAML Configuration Files:** The corresponding ``example_MESA_gyre.yaml`` and ``example_MESA_rsp.yaml`` configuration files set up is responsible to identify blue loops, generate plots, and run GYRE or RSP on the relevant stellar profiles.

----

Run MESA grid example with GYRE
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can easily run your first example by executing `mesalab` with the provided configuration file:

.. code-block:: console

    $ mesalab --config example/example_MESA_gyre.yaml


Upon execution, you'll see terminal output similar to this:

.. code-block:: console

    ================================================================================
                       mesalab CLI - Starting Analysis Workflow
                                  Version: 1.0.0.
    ================================================================================

    ======================================================================
        Starting MESA Analysis Workflow...
    ======================================================================

    Performing MESA Run Analysis: 100%|███████████████████████████████████| 4/4 [00:03<00:00,  1.15it/s]

    ======================================================================
        MESA Analysis Workflow Completed Successfully.
    ======================================================================

    ======================================================================
        Starting Plotting Workflow...
    ======================================================================

    ======================================================================
        Full Instability Strip Crossings Matrix (for Heatmap):
    ======================================================================
                4.0  5.0
    initial_Z
    0.009       0.0  2.0
    0.010       0.0  2.0
    ======================================================================

    Calculating BCs serially: 100%|█████████████████████████████████████████████| 373/373 [00:04<00:00, 88.77it/s]

    ======================================================================
        Plotting Workflow Completed Successfully.
    ======================================================================

    ======================================================================
        Starting GYRE Workflow...
    ======================================================================

    [2025-07-17 01:41:30] GYRE Pipeline: Initializing GYRE workflow from mesalab configuration...
    [2025-07-17 01:41:30] GYRE Progress: Processing M=5.0, Z=0.009 from run directory: run_5.0MSUN_z0.0090
    [2025-07-17 01:41:30] GYRE Progress: Searching profiles in: MESA_grid/run_5.0MSUN_z0.0090/LOGS within model range [2073-2246]
    # ... (GYRE processing details for individual profiles will appear here, showing progress) ...
    [2025-07-17 01:41:37] GYRE Progress: **profile00012 - SUCCESS**
    [2025-07-17 01:41:37] GYRE Progress: **profile00014 - SUCCESS**
    # ... (more profile successes and other GYRE messages) ...
    [2025-07-17 01:42:05] GYRE Pipeline: All GYRE runs completed.
    [2025-07-17 01:42:05] GYRE Pipeline: **GYRE pipeline execution complete.**

    ======================================================================
        GYRE Workflow Completed Successfully.
    ======================================================================

    ================================================================================
    ║                  mesalab Workflow Finished Successfully!                     ║
    ================================================================================




After the workflow completes, you will find the generated plots in the `example/MESA_grid_output/plots` directory. Here are some examples of the plots generated for this grid:

.. figure:: figs/example_2_CMD.png
   :alt: Example Gaia Color-Magnitude Diagram for the blue loop crossers
   :align: center
   :width: 600px

   Gaia Color-Magnitude Diagram (CMD) for the 5 Msun models that undergo blue loop evolution. This plot specifically focuses on models that are currently within the blue loop phase and have crossed the red (cool) boundary of the Instability Strip (IS), indicating evolutionary stages relevant for pulsating stars.

.. figure:: figs/example_2_heatmap.png
   :alt: Example Heatmap of Instability Strip Crossings
   :align: center
   :width: 600px

   Heatmap visualizing the number of instability strip crossings for different initial masses and metallicities.


----


Understanding GYRE Output
~~~~~~~~~~~~~~~~~~~~~~~~~

After the GYRE workflow is complete, a structured output directory is created to store the run data. The main output folder is ``gyre_outputs``, which contains subdirectories for each MESA stellar model run. The name of each subdirectory is identical to its corresponding input folder (e.g., ``run_5.0MSUN_z0.0090``). Within these directories, the final output GYRE files are placed into separate folders, named after the specific ``profileXX`` files that were previously filtered for analysis.

The typical structure within each MESA run directory will look like this:

.. code-block::

    example/MESA_grid_output/
    └── gyre_output/
        └── run_5.0MSUN_z0.0090/
             └── profile00018
                 ├── summary.h5
                 └── detail.l<l>.n<n>.TXT  # Multiple detail files, one per mode


This ``profile00018`` directory contains:

* ``summary.h5``: This is a binary HDF5 file containing an overview of all calculated pulsation modes for a *specific stellar profile*. It's data should look like this:

    .. code-block:: text

                E_norm         eta           freq           l  n_g n_p n_pg          omega
        ---------------------- --- ----------------------- --- --- --- ---- -----------------------
        7.716313969427929e-06  0.0 (0.3288290815023021+0j)   0   0   2    2 (4.1285738058857735+0j)
        4.977767116023725e-06  0.0 (0.4253575871382588+0j)   0   0   3    3  (5.340525796473679+0j)
        3.5467134221115035e-06 0.0 (0.5281812006857637+0j)   0   0   4    4  (6.631515253912462+0j)
        2.8337113271118767e-06 0.0 (0.635019445275698+0j)    0   0   5    5  (7.972909926383766+0j)
        2.4590763873617003e-06 0.0 (0.7390031628616821+0j)   0   0   6    6  (9.278464929921713+0j)
        2.6585877070418085e-06 0.0 (0.8449905263159454+0j)   0   0   7    7 (10.609176467091832+0j)
        3.5417203213359843e-06 0.0 (0.9541649016993462+0j)   0   0   8    8 (11.979902147504905+0j)
        4.947878175758324e-06  0.0 (1.0683611742967478+0j)   0   0   9    9 (13.413679651676546+0j)
        7.228378461640029e-06  0.0 (1.1830569761877034+0j)   0   0  10   10 (14.853728935543636+0j)
* ``detail.l<l>.n<n>.TXT``: These are plain text files, each containing detailed information about the eigenfunction (e.g., displacement, velocity, luminosity perturbations) of a specific pulsation mode in the star's interior. The filename indicates the spherical harmonic degree (`l`) and the radial order (`n`). For example, inspecting a ``detail.l0.n+10.TXT`` file (for a 5 Msun, Z=0.0100 model at a specific evolutionary stage) you should see:

    .. code-block:: text

             Gamma_1                P                   T         dW_dx ...          rho                    x           xi_h             xi_r
        ----------------- --------------------- ----------------- ----- ... --------------------- --------------------- ---- ---------------------------
        1.606969163191305 4.521157801068377e+19 147308873.3222435   0.0 ...      5315.50896647388                   0.0   0j                          0j
        1.606971014744742 4.520751437574627e+19 147304132.5888782   0.0 ...     5315.215595668408 1.208472088924861e-05   0j (-2.981450615162198e-08+0j)
        1.606972222965719 4.520489787509158e+19 147301039.0453129   0.0 ...     5315.024158263152 1.522589262621941e-05   0j (-3.756430947851294e-08+0j)
        1.606974103631016 4.520082534000535e+19 147296223.7726631   0.0 ...     5314.726179017575 1.918363197076153e-05   0j  (-4.73289618981165e-08+0j)
        # ... (approximately 1240 more rows) ...
        1.476818050225248      1131.83485879053  6028.82736586096  -0.0 ... 2.834199661473333e-09    0.9999996951714716   0j      (371.9168322490516+0j)
        1.476826592953945     1131.761019649602 6028.778110862875  -0.0 ... 2.834038079040092e-09    0.9999998013548269   0j      (371.9271310542792+0j)
        1.476830863819082     1131.724100088799 6028.753484881346  -0.0 ... 2.833957286079444e-09     0.999999854448763   0j      (371.9322807595553+0j)
        1.476835134309931     1131.687180535459 6028.728860071846  -0.0 ... 2.833876491880485e-09     0.999999907544212   0j      (371.9374306673092+0j)
        1.476837724407599     1131.664786712097 6028.713924351446  -0.0 ... 2.833827484927481e-09     0.999999939750376   0j      (371.9405544796087+0j)
        1.476839019409059     1131.653589801338 6028.706456636452  -0.0 ... 2.833802981287909e-09    0.9999999558536716   0j      (371.9421164141642+0j)
        1.476840314376306     1131.642392891255 6028.698989028436  -0.0 ... 2.833778477534831e-09    0.9999999719571027   0j      (371.9436783669861+0j)
        1.476841158114899     1131.635097354881 6028.694123442688  -0.0 ... 2.833762511636029e-09    0.9999999824496407   0j      (371.9446960938195+0j)
        1.476842001837285     1131.627801818827 6028.689257907953  -0.0 ... 2.833746545684138e-09    0.9999999929422394   0j      (371.9457138287099+0j)


.. tip::

    You can access the data of GYRE output files using various tools. For Python users, the ``pygyre`` `library <https://pygyre.readthedocs.io/en/stable/index.html>`_ is one of the most convenient options.


For instance, to load the ``summary.h5`` file shown above into a Python object, you would use:

.. code-block:: python

    >>> import pygyre
    >>> import numpy
    >>> s = pygyre.read_output('example/MESA_grid_output/run_5.0MSUN_z0.0100/profile00030/summary.h5')
    >>> print(s)



----

Run MESA grid example with RSP
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Similar to the case of GYRE, you can easily run your first example by executing `mesalab` with the provided configuration file:


.. code-block:: console

    $ mesalab --config example/example_MESA_rsp.yaml


Upon successful execution, you'll see terminal output similar to this:

.. code-block:: console


  ================================================================================
                      mesalab CLI - Starting Analysis Workflow
                                Version: 1.0.0
  ================================================================================


  ======================================================================
          Starting MESA Analysis Workflow...
  ======================================================================

  Performing MESA Run Analysis: 100%|███████████████████████████████████████████████████████████████████████████████████████████████| 4/4 [00:03<00:00,  1.25it/s]

  ======================================================================
          MESA Analysis Workflow Completed Successfully.
  ======================================================================


  ======================================================================
          Starting MESA RSP Workflow...
  ======================================================================

  MESA RSP Workflow: 100%|████████████████████████████████████████████████████████████████████████████████████████████████████| 373/373 [1:45:53<00:00, 17.03s/it]
  --- MESA RSP Workflow Summary ---
  Total runs: 373
  Successful runs: 373
  Failed runs: 0
  Timed out runs: 0
  Runs with unexpected errors: 0
  ---------------------------------

  ======================================================================
          MESA RSP Workflow Completed.
  ======================================================================


  ======================================================================
          Plotting workflow is disabled in configuration.
  ======================================================================


  ======================================================================
          GYRE workflow is disabled in configuration.
  ======================================================================


  ================================================================================
  ║                   mesalab Workflow Finished Successfully!                    ║
  ================================================================================


----


Understanding RSP Output
~~~~~~~~~~~~~~~~~~~~~~~~~

After the RSP workflow completes, each run's output is located in the ``rsp_outputs`` directory will be created within its respective run folder.

The typical structure within each MESA run directory will look like this:

.. code-block::

    example/MESA_grid_output/
    └── run_5.0MSUN_z0.0100/ # Example MESA run directory for profile00030
        └── gyre_output/
            ├── summary.h5
            └── detail.l<l>.n<n>.TXT  # Multiple detail files, one per mode


The ``gyre_output`` directory contains:

* ``summary.h5``: This is a binary HDF5 file containing an overview of all calculated pulsation modes for a *specific stellar profile*. It's data should look like this:

    .. code-block:: text

                E_norm         eta           freq           l  n_g n_p n_pg          omega
        ---------------------- --- ----------------------- --- --- --- ---- -----------------------
        7.716313969427929e-06  0.0 (0.3288290815023021+0j)   0   0   2    2 (4.1285738058857735+0j)
        4.977767116023725e-06  0.0 (0.4253575871382588+0j)   0   0   3    3  (5.340525796473679+0j)
        3.5467134221115035e-06 0.0 (0.5281812006857637+0j)   0   0   4    4  (6.631515253912462+0j)
        2.8337113271118767e-06 0.0 (0.635019445275698+0j)    0   0   5    5  (7.972909926383766+0j)
        2.4590763873617003e-06 0.0 (0.7390031628616821+0j)   0   0   6    6  (9.278464929921713+0j)
        2.6585877070418085e-06 0.0 (0.8449905263159454+0j)   0   0   7    7 (10.609176467091832+0j)
        3.5417203213359843e-06 0.0 (0.9541649016993462+0j)   0   0   8    8 (11.979902147504905+0j)
        4.947878175758324e-06  0.0 (1.0683611742967478+0j)   0   0   9    9 (13.413679651676546+0j)
        7.228378461640029e-06  0.0 (1.1830569761877034+0j)   0   0  10   10 (14.853728935543636+0j)
* ``detail.l<l>.n<n>.TXT``: These are plain text files, each containing detailed information about the eigenfunction (e.g., displacement, velocity, luminosity perturbations) of a specific pulsation mode in the star's interior. The filename indicates the spherical harmonic degree (`l`) and the radial order (`n`). For example, inspecting a ``detail.l0.n+10.TXT`` file (for a 5 Msun, Z=0.0100 model at a specific evolutionary stage) you should see:

    .. code-block:: text

             Gamma_1                P                   T         dW_dx ...          rho                    x           xi_h             xi_r
        ----------------- --------------------- ----------------- ----- ... --------------------- --------------------- ---- ---------------------------
        1.606969163191305 4.521157801068377e+19 147308873.3222435   0.0 ...      5315.50896647388                   0.0   0j                          0j
        1.606971014744742 4.520751437574627e+19 147304132.5888782   0.0 ...     5315.215595668408 1.208472088924861e-05   0j (-2.981450615162198e-08+0j)
        1.606972222965719 4.520489787509158e+19 147301039.0453129   0.0 ...     5315.024158263152 1.522589262621941e-05   0j (-3.756430947851294e-08+0j)
        1.606974103631016 4.520082534000535e+19 147296223.7726631   0.0 ...     5314.726179017575 1.918363197076153e-05   0j  (-4.73289618981165e-08+0j)
        # ... (approximately 1240 more rows) ...
        1.476818050225248      1131.83485879053  6028.82736586096  -0.0 ... 2.834199661473333e-09    0.9999996951714716   0j      (371.9168322490516+0j)
        1.476826592953945     1131.761019649602 6028.778110862875  -0.0 ... 2.834038079040092e-09    0.9999998013548269   0j      (371.9271310542792+0j)
        1.476830863819082     1131.724100088799 6028.753484881346  -0.0 ... 2.833957286079444e-09     0.999999854448763   0j      (371.9322807595553+0j)
        1.476835134309931     1131.687180535459 6028.728860071846  -0.0 ... 2.833876491880485e-09     0.999999907544212   0j      (371.9374306673092+0j)
        1.476837724407599     1131.664786712097 6028.713924351446  -0.0 ... 2.833827484927481e-09     0.999999939750376   0j      (371.9405544796087+0j)
        1.476839019409059     1131.653589801338 6028.706456636452  -0.0 ... 2.833802981287909e-09    0.9999999558536716   0j      (371.9421164141642+0j)
        1.476840314376306     1131.642392891255 6028.698989028436  -0.0 ... 2.833778477534831e-09    0.9999999719571027   0j      (371.9436783669861+0j)
        1.476841158114899     1131.635097354881 6028.694123442688  -0.0 ... 2.833762511636029e-09    0.9999999824496407   0j      (371.9446960938195+0j)
        1.476842001837285     1131.627801818827 6028.689257907953  -0.0 ... 2.833746545684138e-09    0.9999999929422394   0j      (371.9457138287099+0j)


.. tip::

    You can access the data of GYRE output files using various tools. For Python users, the ``pygyre`` `library <https://pygyre.readthedocs.io/en/stable/index.html>`_ is one of the most convenient options.


For instance, to load the ``summary.h5`` file shown above into a Python object, you would use:

.. code-block:: python

    >>> import pygyre
    >>> import numpy
    >>> s = pygyre.read_output('example/MESA_grid_output/run_5.0MSUN_z0.0100/profile00030/summary.h5')
    >>> print(s)
