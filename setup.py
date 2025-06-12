from setuptools import setup, find_packages

setup(
    name="mesa_tools",
    version="0.1.0",
    author="Your Name", # Replace with your name
    author_email="email@example.com", # Replace with your email
    description="Tools for analyzing MESA stellar evolution simulation data, specifically for mass and metallicity grids.",
    long_description="A collection of tools designed for the analysis of MESA stellar evolution simulation data, with a specific focus on outputs from grids run across varying mass and metallicity parameters. Key functionalities include the searching for and characterization of blue loops, and the generation of Hertzsprung-Russell Diagrams (HRDs) and Color-Magnitude Diagrams (CMDs).",
    long_description_content_type="text/markdown",
    url="https://github.com/tnehezd/mesa_blue_loop",
    packages=find_packages(),
    install_requires=[
        'pandas>=1.0.0',      # Required for DataFrame operations
        'numpy>=1.18.0',      # Required for numerical operations
        'matplotlib>=3.3.0',  # Required for plotting
        'PyYAML>=5.3.0',      # Required for YAML config file parsing
        'tqdm>=4.0.0',        # Optional, for progress bars (if used in your code)
    ],
    entry_points={
        'console_scripts': [
            'mesa_grid_analyzer=mesa_tools.cli:main',
        ],
    },
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Topic :: Scientific/Engineering :: Astronomy",
        "Topic :: Scientific/Engineering :: Physics",
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Science/Research",
    ],
    python_requires='>=3.7',
)
