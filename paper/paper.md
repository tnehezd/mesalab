---
title: 'MESAlab: a Pipeline for Mapping the Blue Loop with MESA runs'
tags:
  - Python
  - astronomy
  - variable stars
authors:
  - name: Dóra Tarczay-Nehéz
    orcid: 0000-0003-3759-7616
    affiliation: "1, 2"
affiliations:
 - name: HUN-REN CSFK Konkoly Observatory, Konkoly Thege M. út 15-17, Budapest, 1121, Hungary
   index: 1
 - name: CSFK, MTA Centre of Excellence, Konkoly Thege M. út 15-17, Budapest, 1121, Hungary
   index: 2
date: 22 August 2025
bibliography: paper.bib
---

# Summary
Modules for Experiments in Stellar Astrophysics (MESA; @Paxton2011, @Paxton2013, @Paxton2015, @Paxton2018, @Paxton2019; @Jermyn2023) is a widely used open-source software for modelling stellar evolution. In many studies, the computational grids cover thousands of models (e.g., @Joyceetal2024; @TarczayNehezetal2026), which require a tremendous amount of time and computational effort to process.


To streamline data analysis, the `mesalab` package was developed. This Python-based pipeline is designed to simplify the post-processing of MESA outputs by automatically identifying various stellar evolutionary phases, with a specific focus on the "blue loop"—a blue-ward excursion in the Hertzsprung-Russell Diagram (HRD) for intermediate-mass stars often associated with peculiar pulsational phenomena like "strange modes".

# The `mesalab` pipeline

The pipeline offers several key functionalities

- It collects data on the mass and metallicity ranges of the grid.
- It calculates the necessary bolometric corrections (BCs) by utilizing the MESA Isochrones & Stellar Tracks (MIST) bolometric correction tables [@MIST].
- It automatically generates HRDs and Color-Magnitude Diagrams (CMDs) with Gaia passbands.
- Furthermore, `mesalab` provides crucial tools for asteroseismological investigations.


# Software design

The `mesalab` pipeline is designed as a modular and extensible Python package that reflects the typical analysis workflow of stellar evolution studies while remaining flexible for user-defined extensions. Rather than functioning as a collection of standalone scripts, the pipeline implements a structured, stage-based design in which each module performs a clearly defined role in the analysis chain.

A central design decision is the explicit separation between data parsing, evolutionary phase identification, visualization, and asteroseismological preparation. Raw MESA outputs are first processed using established community tools, after which `mesalab` applies domain-specific logic to identify blue loop crossings based on evolutionary tracks in the Hertzsprung–Russell diagram. This targeted filtering step is a core feature of the pipeline and enables efficient downstream analysis.

The asteroseismology modules are tightly integrated into the pipeline. Instead of only generating configuration files, `mesalab` optionally executes GYRE and MESA RSP directly, with support for sequential or parallel execution. This design choice minimizes manual intervention and ensures consistency between the selected evolutionary models and the subsequent pulsation analyses.

Python was chosen as the implementation language to facilitate interoperability with existing scientific libraries, ease of installation, and long-term maintainability. The modular architecture allows individual components—such as evolutionary phase detection or pulsation execution—to be adapted or replaced without disrupting the overall workflow.


#### The GYRE module

For blue loop crosser models, the pipeline generates GYRE [@Townsend2013] inlist files with user-specified parameters. In the next step, the pipeline automatically executes GYRE for these pre-filtered models, with the option to run the computations sequentially or in parallel, significantly enhance computational efficiency.

#### The RSP module

Similar to the GYRE module, the pipeline generates MESA RSP ([@Smolec2008], [@Paxton2019]) inlist files with user-specified parameters, then executes the MESA RSP module automatically. This process can also be configured to run the computations sequentially or in parallel, with the number of parallel threads also being a user-defined parameter.


# Statement of Need
Large grids of stellar evolution models produced with MESA [@Paxton2011, @Paxton2013, @Paxton2015, @Paxton2018, @Paxton2019; @Jermyn2023] are widely used in studies of intermediate-mass stars, especially for understanding phenomena like blue loops and associated pulsational modes. However, processing these grids is time-consuming and often requires repetitive, manual post-processing steps, including identifying evolutionary phases, generating diagrams, computing bolometric corrections, and preparing models for asteroseismic analysis.

The `mesalab` pipeline was developed to address this gap. It is a fully automated, Python-based, open-source package that:
* Identifies blue loop crossings and other key evolutionary phases in MESA grids.
* Computes bolometric corrections using MIST tables [@MIST].
* Generates Hertzsprung-Russell diagrams (HRDs) and Color-Magnitude Diagrams (CMDs) with Gaia passbands.
* Automates the generation and execution of GYRE [@Townsend2013] and MESA RSP [@Smolec2008; @Paxton2019] inlist files, with optional parallel execution for computational efficiency.

By automating these tasks, `mesalab` efficiently reduces the time and effort required to process large MESA grids, enabling reproducible, systematic analysis of blue loop evolution and associated pulsational behavior.

# State of the Field
Existing tools partially address components of this workflow. For example `py_mesa_reader` provides low-level access to MESA output files, facilitating data parsing and extraction. `FOAM` [@Foam] focuses on comparing theoretical pulsation predictions with observations. Individual modules like GYRE and MESA RSP enable linear and nonlinear pulsation analysis, respectively, but require manual preparation of input files and selection of models.

While these tools are powerful, there is currently no widely adopted, end-to-end pipeline that integrates all steps needed for systematic analysis of blue loop evolution in large MESA grids. Each tool alone addresses only part of the workflow, and significant manual effort is required to connect them.
`mesalab` fills this gap by providing a unified interface that automates the entire post-processing and asteroseismic preparation workflow, from parsing MESA outputs and identifying blue loop models to generating diagrams and executing pulsation calculations. Unlike single-purpose utilities, it enables researchers to efficiently and reproducibly analyze large model grids, reducing manual effort and improving consistency across studies.

# Research Impact Statement
The pipeline has already been employed within our research group, as demonstrated in a prior study by the author [@TarczayNehezetal2026], and it is actively being used in further studies currently in preparation. This highlights that `mesalab` is both practically effective and a valuable tool for ongoing research in the field of asteroseismology.


# Acknowledgements
This project has been supported by the KKP-137523 'SeismoLab' Élvonal grant of the Hungarian Research, Development and Innovation Office (NKFIH), and by the LP2025-14/2025 Lendület grant of the Hungarian Academy of Sciences.

# AI Usage Disclosure

Generative AI tools were utilized in both the development of the software and the preparation of this manuscript:

* **Software Development:** Microsoft Copilot and ChatGPT (GPT-4o) were used to assist in modularizing the pipeline. These tools were also used to generate unit tests for the core analysis functions, as well as the preparation of docstrings for the API documentation.

* **Manuscript Preparation:** Gemini 2.5 Flash was used to refine the paper sections to align with JOSS's updated scope and formatting requirements.

**Confirmation of Review:** All AI-assisted code, tests, and text outputs were manually reviewed, edited, and validated by the author. All core architectural decisions, scientific interpretations, and the overall testing strategy remain the original work of the human author, who assumes full responsibility for the accuracy and integrity of the submitted materials.

# References

