---

## Test Cases

This directory contains various test cases for the `mesalab` program, each designed to demonstrate different aspects of its functionality.

---

### MIST Synthetic Data (for Basic Analysis & Quick Start)

This folder hosts synthetic stellar evolution tracks obtained from the **MIST (MESA Isochrones & Stellar Tracks) website**: [http://waps.cfa.harvard.edu/MIST/](http://waps.cfa.harvard.edu/MIST/). This dataset is intentionally small and included directly in the repository to provide a **quick and efficient way to demonstrate `mesalab`'s data pre-processing and basic analysis capabilities**, such as parsing input files and extracting fundamental stellar parameters.

The data covers models with a rotational velocity ratio of **v/vcrit = 0**, stellar masses ranging from **2.1 to 12.6 solar masses ($M_{\odot}$)**, and metallicities ([Fe/H]) of **-1 and +0.5**. The metallicity [Fe/H] values can be converted to the MESA abundance $Z$ using the following formula:

[Provide your blabla formula here, e.g., $Z = Z_{\odot} \times 10^{[Fe/H]}$ where $Z_{\odot}$ is the solar metallicity of your choice]

Since `mesalab` is primarily optimized for MESA's `history.data` file format, the EEP (Equal-Evolutionary-Phase) files directly downloadable from MIST have been converted into **quasi-history files**. These converted files mimic the structure and header of standard MESA `history.data` outputs. A `model_number` column has been arbitrarily added and populated with sequential numbers starting from 1 to ensure compatibility.

Additionally, as the MIST website only provides EEP files, dummy `inlist` files have been created for each track. These dummy files contain solely the stellar mass and initial metallicity, aligning with typical MESA input requirements.

The included models offer a diverse set of evolutionary behaviors, including:
* Tracks that exhibit a **blue loop** phase.
* Tracks that **do not** undergo a blue loop.
* Tracks that **have not yet reached the Red Giant Branch (RGB) tip**.

---

### MESA Grid Data (for Full Workflow & Advanced Analysis)

For a comprehensive demonstration of `mesalab`'s full capabilities, including **asteroseismology analyses with GYRE integration**, a separate, larger dataset of **real MESA stellar evolution runs** is available. This dataset contains:

* **Complete `history.data` files** for each simulation.
* **Corresponding MESA `profile` files**, which are essential for GYRE analyses.
* **Pre-generated GYRE input files** (`.gyre` or similar), enabling a full asteroseismic workflow.

Due to its size, this dataset is hosted externally. You can download it from:

**[YOUR ZENODO LINK FOR THE FULL MESA GRID DATASET HERE]**

This dataset is recommended for users who want to explore `mesalab`'s advanced features, especially those involving detailed internal stellar structure analysis.

---
