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

Using Conda (Recommended if you already use Conda or manage complex environments)
---------------------------------------------------------------------------------

If you have Conda (Miniconda or Anaconda) installed, you can create a dedicated environment for `mesalab`:

1.  **Create the Conda environment:**

    .. code-block:: bash

        conda create -n mesalab-env python=3.9 # Or your preferred Python version, above 3.7

    This command creates an environment named ``mesalab-env`` with the specified Python version.

2.  **Activate the Conda environment:**

    .. code-block:: bash

        conda activate mesalab-env

    You'll know the Conda environment is active when you see ``(mesalab-env)`` preceding your prompt in the terminal.



Once your virtual environment is active, you can proceed with installing the necessary dependencies.

----

Install from pip
----------------

To install `mesalab` with pip:

.. code-block:: console

   $ pip install seismolab

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