# A file to set up the Python package for distribution.
from setuptools import setup, find_packages
from os import path
import sys

# Define the absolute path of the current file's directory.
here = path.abspath(path.dirname(__file__))

# Insert 'mesalab' into the system path to allow local imports.
sys.path.insert(0, "mesalab")
from version import __version__

# Read the requirements from the requirements.txt file.
# The `path.join(here, 'requirements.txt')` ensures the correct path is used
# regardless of the current working directory.
requirements = []
with open(path.join(here, 'requirements.txt'), encoding='utf-8') as file:
    requirements = file.read().splitlines()

# Load the README.md file for the long description on PyPI.
try:
    with open(path.join(here, 'README.md'), encoding='utf-8') as f:
        long_description = f.read()
except:
    # If the README.md file is not found, use a single space.
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
    install_requires=requirements,
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
