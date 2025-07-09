# Welcome to mesalab's documentation!

This documentation provides an overview of the `mesalab` package, its functionalities, and how to use it for analyzing MESA and GYRE stellar evolution data.



```{toctree}
:maxdepth: 2
:caption: Contents:

installation
first_run
#more lists...
```

## About `mesalab`

`mesalab` is a Python package designed for the **efficient processing and analysis of stellar evolution simulations performed with MESA** (Modules for Experiments in Stellar Astrophysics). It is particularly well-suited for handling **large grids of simulations**, such as those varying stellar mass (M) and metallicity (Z).

The package streamlines the workflow of analyzing MESA outputs, providing tools for:

* **Data Collection:** Systematically gathering relevant data from MESA `LOGS` and GYRE `summary.h5` files across entire simulation grids.
* **Parameter Linking:** Seamlessly associating GYRE pulsation modes with their corresponding stellar parameters from MESA evolutionary tracks.
* **Data Aggregation:** Consolidating diverse simulation outputs into easily manageable formats (e.g., CSV files).
* **Visualization:** Generating insightful plots such as Hertzsprung-Russell Diagrams (HRDs) and Color-Magnitude Diagrams (CMDs), often enhanced with pulsation properties.

`mesalab` aims to facilitate the research of stellar evolution and asteroseismology by providing robust and flexible tools for handling complex simulation data.


## About this Documentation

This documentation was written by [Your Name] and generated using Sphinx and MyST Markdown.
Content generation, including initial text drafts and formatting suggestions, was partially assisted by a large language model (Google's Gemini).