from setuptools import setup, find_packages
from os import path
import sys

sys.path.insert(0, "mesalab")
from version import __version__

# Load requirements
#requirements = None
#with open('requirements.txt') as file:
#    requirements = file.read().splitlines()

# If Python3: Add "README.md" to setup.
# Useful for PyPI. Irrelevant for users using Python2.
try:
    this_directory = path.abspath(path.dirname(__file__))
    with open(path.join(this_directory, 'README.md'), encoding='utf-8') as f:
        long_description = f.read()
except:
    long_description = ' '


setup(
    name="mesalab",
    version=__version__,
    author="Dora Tarczay-Nehez", 
    author_email="tarczaynehez.dora@csfk.org", 
    description="Tools for analyzing MESA stellar evolution simulation data, specifically for mass and metallicity grids.",
    long_description="A collection of tools designed for the analysis of MESA stellar evolution simulation data, with a specific focus on outputs from grids run across varying mass and metallicity parameters. Key functionalities include the searching for and characterization of blue loops, and the generation of Hertzsprung-Russell Diagrams (HRDs) and Color-Magnitude Diagrams (CMDs).",
    long_description_content_type="text/markdown",
    url="https://github.com/tnehezd/mesa_blue_loop",
    packages=find_packages(),
    install_requires=[
        # Core data handling and plotting libraries (already in your setup.py)
        'pandas>=1.3.5',      # Installed: 1.3.5
        'numpy>=1.21.6',      # Installed: 1.21.6
        'matplotlib>=3.5.3',  # Installed: 3.5.3
        'PyYAML>=6.0.1',      # Installed: 6.0.1
        'tqdm>=4.67.1',       # Installed: 4.67.1 (using your exact version for robustness)

        # Scientific and Astronomical Libraries (likely direct dependencies based on pipdeptree & project description)
        'scipy>=1.7.3',       # Installed: 1.7.3
        'astropy>=4.3.1',     # Installed: 4.3.1
        'isochrones>=2.1',    # Installed: 2.1 (Highly probable direct dependency for MIST analysis)
        'pygyre>=1.3.2',      # Installed: 1.3.2 (If used for pulsation analysis)
        'seaborn>=0.12.2',    # Installed: 0.12.2 (If used for specific plot types like heatmaps/distributions)
        'numba>=0.56.4',      # Installed: 0.56.4 (If used for performance optimization)
        'swifter>=1.4.0',     # Installed: 1.4.0 (If used to accelerate pandas operations)
        'f90nml>=1.4.5',      # Installed: 1.4.5 (If used gyre calculations)
        'addict',
        # Other potential dependencies that might be direct (check your imports)
        # 'gdr3bcg==1.1',     # If you directly import and use this package
        # 'holoviews>=1.16.2',# If you directly import and use HoloViews for interactive plots
        # 'panel>=0.14.4',    # If you directly import and use Panel for dashboards (often with HoloViews)
    ],
    entry_points={
        'console_scripts': [
            'mesalab=mesalab.cli:main',
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
    python_requires='>=3.9',
)
