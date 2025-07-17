.. _overview_workflows:

Overview of mesalab Workflows
=============================

`mesalab` is designed to automate the post-processing and analysis of stellar evolution simulations performed with `MESA <https://docs.mesastar.org/>`_.

The main steps in `mesalab` are:

1.  :doc:`mesa_analysis_workflow`
2.  :doc:`plotting_workflow` (Optional)
3.  :doc:`gyre_workflow` (Optional)

Each of these steps can be enabled or disabled independently via your :ref:`YAML configuration <understanding_yaml_config>` file. This allows you to utilize `mesalab`'s execution to your specific needs, whether you want to run a full analysis pipeline or just a subset of operations.

The Analysis Flow 
-----------------

The steps in `mesalab` generally follow a sequential dependency:


1.  **MESA Run Analysis Workflow:**
    First, `mesalab` loads your raw MESA output files (``history.data`` and the ``inlist`` file) and the individual stellar profile files (``profileX.data`` and ``profiles.index``) and extracts key stellar parameters from them. Then, in the next step, it identifies significant evolutionary phases: hydrogen depletion, helium core ignition and the *blue loop* phase, if present. In this step, `mesalab` creates structured data summaries (e.g., in CSV format). This processed data then serves as the primary input for subsequent steps.


2.  **Plotting Workflow (Optional):**
    This phase **depends on the output of the MESA Run Analysis**. It takes the summarized and processed data to generate various types of plots, such as Hertzsprung-Russell (HR) diagrams, instability strip heatmaps, and blue loop visualizations. It helps you visually check the evolutionary tracks and pulsation properties of your MESA models.


3.  **GYRE Workflow (Optional):**
    This phase **depends on the results of the MESA Run Analysis**, specifically the models that the MESA Run Analysis identifies **blue loop crossers**. For these filtered models, it then requires their corresponding ``profileX.data`` and ``profileX.data.GYRE`` files.
    If enabled, `mesalab` prepares these selected MESA stellar profiles for the `GYRE <https://gyre.readthedocs.io/>`_ pulsation code. It then executes GYRE based on a user-provided ``gyre.in`` template, and provides data on pulsation frequencies and stability. This workflow **requires a separate installation of the GYRE code on your system**.

To configure and effectively utilize `mesalab` for automating your stellar modeling analysis tasks, proceed to the dedicated sections for a detailed description of each workflow.