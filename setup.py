from setuptools import setup, find_packages
from os import path
import sys

here = path.abspath(path.dirname(__file__))
sys.path.insert(0, "mesalab")
from version import __version__

setup(
    name="mesalab",
    version=__version__,
    author="Dora Tarczay-Nehez",
    author_email="tarczaynehez.dora@csfk.org",
    description="Tools for analyzing MESA stellar evolution simulation data.",
    long_description="See README.md",
    long_description_content_type="text/markdown",
    url="https://github.com/tnehezd/mesa_blue_loop",
    packages=find_packages(),
    entry_points={
        'console_scripts': [
            'mesalab=mesalab.cli:main',
        ],
    },
)
