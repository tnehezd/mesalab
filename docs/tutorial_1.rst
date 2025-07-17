Tutorial 1: Analyzing ``example`` grids
=======================================

`mesalab` contains two grids in the `example` directory: The first on is a MESA-generated grid of models, and the second is a synthetic, MIST-generated set.

----

1) MESA Grid: ``example/MESA_grid``
-----------------------------------

This set of runs are **actual MESA stellar evolution outputs**, providing standard ``profile``, ``history``, and ``inlist`` files for demonstrating the analysis.

* **Grid Structure & Parameters:** It contains a **2x2 grid** of models with initial masses of **4 and 5 solar masses** and metallicities (Z) of **0.0090 and 0.0100**.

* **Evolutionary Coverage:** Simulations cover stellar evolution from the **pre-main sequence (pre-MS)** through to **after the blue loop phase**.

* **Blue Loop Behavior:** A key feature is the differing blue loop behavior: **5 Msun models exhibit blue loop crossings**, while **4 Msun models do not**. This highlights `mesalab`'s capability to identify and filter profiles based on evolutionary characteristics.

* **Location:** These models are found in the ``example/MESA_grid`` directory.

* **YAML Configuration File:** The corresponding ``example_MESA.yaml`` configuration file set up is responsible to identify blue loops, generate plots, and run GYRE on the relevant stellar profiles.


Run MESA grid example
~~~~~~~~~~~~~~~~~~~~~

You can easily run your first example by executing `mesalab` with the provided configuration file:

.. code-block:: console

    $ mesalab --config example/example_MESA.yaml


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

.. figure:: figs/example_1_CMD.png
   :alt: Example Gaia Color-Magnitude Diagram for the blue loop crossers
   :align: center
   :width: 600px

   Gaia Color-Magnitude Diagram (CMD) for the 5 Msun models that undergo blue loop evolution. This plot specifically focuses on models that are currently within the blue loop phase and have crossed the red (cool) boundary of the Instability Strip (IS), indicating evolutionary stages relevant for pulsating stars.

.. figure:: figs/example_1_heatmap.png
   :alt: Example Heatmap of Instability Strip Crossings
   :align: center
   :width: 600px

   Heatmap visualizing the number of instability strip crossings for different initial masses and metallicities.


----


Understanding GYRE Output
~~~~~~~~~~~~~~~~~~~~~~~~~

After the GYRE workflow completes, you can explore its output. For each MESA run that underwent GYRE pulsation analysis, a dedicated ``gyre_output`` directory will be created within its respective run folder.

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

                 E_norm                 eta                               freq                    ...  n_pg                 omega
        ---------------------- -------------------- --------------------------------------------  ...  ---- -------------------------------------------
        9.791209204437041e-05  -0.15313422174528532  (0.2328182672224275-0.0001753764088693985j)  ...    1  (2.9221449596750224-0.0022011816140441224j)
        1.2329124369393266e-05 -0.02967224669642672  (0.3330096289637554-0.00017655234704729673j) ...    2  (4.179665197276014-0.0022159410307360772j)
        8.157609186464333e-06  -0.3416741223292514   (0.428953092230821-0.0015145708703314352j)   ...    3  (5.383869278615445-0.019009657994668198j)
        1.770810483940445e-06  -0.879360741944938    (0.9437558006384473-0.021370761792752234j)   ...    8  (11.845252904339308-0.2682283679976242j)

* ``detail.l<l>.n<n>.TXT``: These are plain text files, each containing detailed information about the eigenfunction (e.g., displacement, velocity, luminosity perturbations) of a specific pulsation mode in the star's interior. The filename indicates the spherical harmonic degree (`l`) and the radial order (`n`). For example, inspecting a ``detail.l0.n+1.TXT`` file (for a 5 Msun, Z=0.0100 model at a specific evolutionary stage) you should see:

    .. code-block:: text

            Gamma_1                  P                   T        ...          x            xi_h                              xi_r
        ----------------- --------------------- ----------------- ... -------------------- ---- -----------------------------------------------
        1.62657312464833  1.076658080300887e+19 92245315.34180437 ...               0.0015   0j    (-1.013349549174743e-12-4.719280249341766e-12j)
        1.62676123699543  1.064775473825093e+19 91863903.67944168 ...  0.001505562754591702  0j    (6.149949834599669e-10+2.74429075722165e-10j)
        1.626870671421351 1.057910788735952e+19 91642246.53956042 ...  0.001508794228216629  0j    (9.74455458317007e-10+4.398033536261451e-10j)
        # ... (approximately 1240 more rows) ...
        1.435497240162898   965.5686406393887   6143.551053133849 ...     0.999999762205718  0j    (101.060652446609+5.656352790941287e-05j)
        1.435502558493021   965.5316881589298   6143.52159999898  ...    0.9999998257630135  0j    (101.0606355072479+4.0887166044867e-05j)
        1.435507876302607   965.4947356874168   6143.492148521598 ...    0.9999998893224236  0j    (101.0606185645352+2.521121252529249e-05j)
        1.435511101579336   965.4723218981109   6143.474285415081 ...    0.9999999278758862  0j    (101.0606082861739+1.570304540546494e-05j)
        1.435512714152217   965.4611150045585   6143.465354067162 ...    0.9999999471529095  0j    (101.0606031465306+1.094901811835352e-05j)
        1.435514326677539   965.4499081118174   6143.456422870514 ...    0.9999999664301285  0j    (101.0605980065788+6.195028115518074e-06j)
        1.435515377318868   965.4426060711415   6143.450603684033 ...    0.9999999789906336  0j    (101.0605946573896+3.097505108646601e-06j)
        1.435516427937624    965.435304030864   6143.444784569691 ...    0.9999999915512134  0j    (101.0605913080717+0j)

.. tip::

    You can access the data of GYRE output files using various tools. For Python users, the ``pygyre`` `library <https://pygyre.readthedocs.io/en/stable/index.html>`_ is one of the most convenient options. 


For instance, to load the ``summary.h5`` file shown above into a Python object, you would use:

.. code-block:: python

    >>> import pygyre
    >>> import numpy
    >>> s = pygyre.read_output('example/MESA_grid_output/run_5.0MSUN_z0.0100/profile00030/summary.h5')
    >>> print(s) 


----

2) MIST synthetic: ``example/MIST_synthetic``
---------------------------------------------