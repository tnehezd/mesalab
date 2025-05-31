# setup.py
from setuptools import setup, find_packages

setup(
    name="mesa_tools",
    version="0.1.0",
    author="A Te Neved",
    author_email="email@example.com",
    description="A collection of tools for analyzing MESA stellar evolution simulation data.",
    long_description=open("README.md").read(), 
    long_description_content_type="text/markdown",
    url="https://github.com/tnehezd/mesa_blue_loop",
    packages=find_packages(),
    install_requires=[
        # No external dependencies currently for parsing, but you might add 'pandas' later
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
    ],
    python_requires='>=3.6', 
)