MESA Grid Directory Scanner
===========================

.. automodule:: mesalab.analyzis.grid_analyzer
    :members:
    :undoc-members:
    :show-inheritance:


Data Reader Module
==================

.. automodule:: mesalab.analyzis.data_reader
    :members:
    :undoc-members:
    :show-inheritance:

Example usage
-------------

.. code-block:: python

    from mesalab.analyzis import data_reader

    # Example directory structure:
    # /path/to/my_mesa_sims/
    # ├── run_01/
    # │   ├── inlist_project
    # │   └── LOGS/
    # │       └── history.data
    # └── run_02/
    #     ├── inlist_project
    #     └── LOGS/
    #         └── history.data


    runs = data_reader.scan_mesa_runs('/path/to/multiple/runs', 'inlist_filename')
    for run in runs:
        print(f"Run directory: {run['run_dir_path']}, mass={run['mass']}, Z={run['z']}")



Data Analysis Performer
=======================

.. automodule:: mesalab.analyzis.mesa_analyzer
    :members:
    :undoc-members:
    :show-inheritance:

