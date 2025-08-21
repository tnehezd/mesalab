from setuptools import setup, find_packages
from os import path
import sys

sys.path.insert(0, "mesalab")
from version import __version__

# Load requirements
requirements = []
with open('requirements.txt') as file:
    requirements = file.read().splitlines()

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