# Welcome to `mesalab`'s documentation!

This documentation provides an overview of the `mesalab` package, its functionalities, and how to use it for analyzing MESA and GYRE stellar evolution data.



```{toctree}
:maxdepth: 3
:caption: Contents
about
installation
first_run
```
```{toctree}
:caption: API
preprocessing
blueloopanalyze
```

## About `mesalab`

The Python package `mesalab` is designed for **processing and analyze of stellar evolution simulations performed with MESA** (Modules for Experiments in Stellar Astrophysics). It is developed for handling **large grids of simulations**, such as grids with varying stellar mass (M) and metallicity (Z). The main purpose of the pipeline is to analyze each simulation, checks if the star crosses the *blue loop* on the instability strip, **then generates inlist files and runs corresponding GYRE simulations**.

The current version of `mesalab` is optimized for MESA version **23.05.01** and GYRE version **7.0**. The essential MESA output parameters required for this analysis are summarized in the following list:

The package runs through the workflow of analyzing MESA outputs as follows:

* **Data Collection:** The pipeline  gathers relevant data across entire simulation grids, such as:
    * **From MESA `LOGS/history.data` files:** Stellar evolution parameters like `log_g`, `log_Teff`, `log_L`, `mass`, `star_age`, `log_R`, `center_h1` and `center_he4`.
    * **From MESA inlist files:** Initial simulation parameters such as `initial_mass` and `initial_Z`.
* **Parameter Linking:** Seamlessly associating GYRE pulsation modes with their corresponding stellar parameters from MESA evolutionary tracks.
* **Data Aggregation:** Consolidating diverse simulation outputs into easily manageable formats (e.g., CSV files).
* **Visualization:** Generating insightful plots such as Hertzsprung-Russell Diagrams (HRDs) and Color-Magnitude Diagrams (CMDs), often enhanced with pulsation properties.

`mesalab` aims to facilitate the research of stellar evolution and asteroseismology by providing robust and flexible tools for handling complex simulation data.


## About this Documentation

This documentation was written by D. Tarczay-Nehéz and generated using Sphinx and MyST Markdown.
Content generation, including initial text drafts and formatting suggestions, was partially assisted by a large language model (Google's Gemini).
