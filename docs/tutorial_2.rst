Tutorial 2: Running an Example Analysis in Google Colab
=======================================================

This tutorial will guide you through running a full ``mesalab`` analysis within a `Google Colab <https://colab.research.google.com/>`_ notebook environment. By following these steps, you will learn how to:

* Set up ``mesalab`` in Colab.
* Access and utilize the provided example MESA data.
* Configure ``mesalab`` using a YAML file within the Colab environment.
* Execute the analysis pipeline.
* Inspect and visualize the generated output directly in Colab.

---

1.  Open a New Google Colab Notebook
------------------------------------

First, open your web browser and navigate to `Google Colab <https://colab.research.google.com/>`_.

* Sign in with your Google account.
* Click on **"File" -> "New notebook"** to create a fresh notebook.

---

2.  Set up ``mesalab`` and Get Example Data
-------------------------------------------

In your Colab notebook, you will use shell commands (prefixed with ``!``) to prepare the environment.

**2.1 Clone the ``mesalab`` repository**

First, clone the ``mesalab`` repository from GitHub. This will give you access to the source code and the ``example/`` directory containing the sample MESA data and configuration files.

.. code-block:: ipython

    # Clone the mesalab repository from GitHub
    !git clone https://github.com/YOUR_USERNAME/mesalab.git # <--- REPLACE WITH YOUR REPO URL

    # Navigate into the cloned directory
    %cd mesalab

You should now see the ``mesalab`` directory as your current working directory in Colab's file explorer (left sidebar).

**2.2 Install ``mesalab`` and its dependencies**

Now, install ``mesalab`` and all its required Python packages.

.. code-block:: ipython

    # Install mesalab in "editable" mode along with its dependencies
    !pip install -e .

    # If you also need specific development or visualization dependencies:
    # !pip install -r requirements.txt # Or specific packages like holoviews, pymultinest if needed
    # (Optional: If holoviews/PyMultiNest warnings persist and you need them, install them here)
    # !pip install holoviews pymultinest # Adjust based on actual needs

You might see messages about Holoviews or PyMultiNest not being imported if they are not specifically listed in your `requirements.txt` or installed separately. Install them if you intend to use the features that rely on them.

---

3.  Examine the Example Data and Configuration
----------------------------------------------

The ``example/`` directory, now located at ``mesalab/example/`` within your Colab environment, contains the necessary files for this walkthrough.

**3.1 Understand the Example Data Structure**

The ``example/`` directory contains:

* ``LOGS/``: A simplified directory structure of MESA output, including ``history.data`` files and potentially ``profileX.data`` files.
* ``mesa_inlists/``: (Optional) May contain MESA ``inlist_project`` files.
* ``gyre_config.in``: An example GYRE configuration file.
* ``example_config.yaml``: A pre-configured ``mesalab`` YAML file specifically for this example data.

You can explore these files using Colab's file browser on the left sidebar.

**3.2 Inspect ``example_config.yaml``**

Open ``example_config.yaml`` (located at ``mesalab/example/example_config.yaml``) in a text editor (e.g., by clicking on it in the Colab file browser) and review its contents. This file dictates how ``mesalab`` will process the example data.

Pay attention to these key settings:

* ``general_settings:``:
    * ``input_dir: ./LOGS``: This tells ``mesalab`` where the MESA output is, relative to the ``example_config.yaml`` file.
    * ``output_dir: ./mesalab_example_output``: Where ``mesalab`` will save all results.
* ``blue_loop_analysis:``, ``gyre_workflow:``, ``plotting_settings:``: These sections control which analyses and plots are enabled for the example run.

---

4.  Running the Analysis
------------------------

Now, let's execute ``mesalab`` using the provided example configuration. We need to run the command from the directory where ``example_config.yaml`` is located.

.. code-block:: ipython

    # Change directory to the example folder where the config file and LOGS are
    %cd example

    # Run mesalab with the example configuration file
    !mesalab --config example_config.yaml

You will see ``mesalab``'s log messages and progress bars directly in the Colab output cell. Depending on the example data size, this may take a few moments.

---

5.  Inspecting and Visualizing the Output
-----------------------------------------

Once ``mesalab`` completes its run, all generated results will be saved into the output directory specified in ``example_config.yaml`` (e.g., ``./mesalab_example_output``, relative to the ``example/`` directory).

**5.1 Navigate to the Output Directory**

You can navigate to the output directory to explore the generated files:

.. code-block:: ipython

    # Navigate to the output directory
    %cd mesalab_example_output

**5.2 View Generated Files**

You can list the contents of the directory:

.. code-block:: ipython

    !ls -lh

You will typically find:

* ``summary_metrics.csv``: A comprehensive CSV summary for each MESA run.
* ``detail_files/``: Detailed CSVs for identified blue loops (if enabled).
* ``plots/``: Generated image files (HR diagrams, heatmaps, etc.).
* ``gyre_inputs/``: GYRE input files (if the workflow was enabled).

**5.3 Display Plots Directly in the Notebook (Optional)**

You can display generated image files directly within your Colab notebook. For example, to view an HR diagram:

.. code-block:: ipython

    # Assuming an HR diagram named 'HRD_all_blue_loop_data.png' was generated in the 'plots' subfolder
    from IPython.display import Image, display
    display(Image(filename='plots/HRD_all_blue_loop_data.png')) # Adjust filename as per your generated plots

    # You can also view CSV files directly
    import pandas as pd
    summary_df = pd.read_csv('summary_metrics.csv')
    print(summary_df.head())

---

This concludes the walkthrough of using ``mesalab`` with example data in Google Colab. You are now equipped to adapt these steps for your own MESA simulation grids and integrate ``mesalab`` into your Colab-based workflows.