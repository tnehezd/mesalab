Installation
===============

Before you begin, it's highly recommended to set up a **Python virtual environment** for `mesalab`. Virtual environments create an isolated space for your project's dependencies, preventing conflicts with other Python projects or your system's global Python installation. This ensures a clean and reproducible setup. You can choose your favorite virtual environment manger. 

If you're not familiar with this process, here are two commonly used and recommended options: **venv** (Python's built-in virtual environment module) or **Conda** (a popular package, dependency, and environment manager).

----

Using venv (Recommended for most Python-only projects)
------------------------------------------------------

To create and activate a virtual environment named ``mesalab-env`` using `venv`, navigate to your project directory and run:

.. code-block:: bash

    python3 -m venv mesalab-env
    source mesalab-env/bin/activate

You'll know the virtual environment is active when you see ``(mesalab-env)`` preceding your prompt in the terminal.

----

Using Mamba or Conda (Recommended for complex environments)
-----------------------------------------------------------

Standard Conda can be slow when resolving large scientific stacks. It is highly recommend using **Mamba** (a fast, C++ based drop-in replacement for Conda) to drastically speed up the installation.

If you have Mamba or Conda installed, you can create a dedicated environment for `mesalab`:

1.  **Create the environment:**

    **Option A: Using Mamba (Fastest)**
    
    .. code-block:: bash

        mamba create -n mesalab-env python=3.9
        mamba install -c conda-forge numpy pandas matplotlib scipy pyyaml tqdm numba swifter dask pyarrow h5py astropy addict

    **Option B: Using Conda (Legacy)**
    
    .. code-block:: bash

        conda create -n mesalab-env python=3.9
        conda install -c conda-forge numpy pandas matplotlib scipy pyyaml tqdm numba swifter dask pyarrow h5py astropy addict

2.  **Activate the environment:**

    .. code-block:: bash

        conda activate mesalab-env

    You'll know the environment is active when you see ``(mesalab-env)`` preceding your prompt in the terminal.



Once your virtual environment is active, you can proceed with installing the necessary dependencies.

----

Install from pip
----------------

To install `mesalab` with pip:

.. code-block:: console

   $ pip install mesalab

----

Building from source
--------------------

You can also install the current development version of `mesalab` with cloning the GitHub repository and install the code with pip:

.. code-block:: console

  $ git clone https://github.com/konkolyseismolab/mesalab
  $ cd mesalab
  $ pip install .

----

Unit test
---------
To run the unit tests you may need to install ``pytest``:

.. code-block:: console

  $ pip install pytest
  $ python3 -m pytest