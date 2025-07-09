# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# --- Path setup ------------------------------------------------------------
# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath.

import os
import sys
# This line is CRUCIAL. It tells Sphinx to add the parent directory (your project root)
# to the Python path, so it can find and import your 'mesalab' package.
sys.path.insert(0, os.path.abspath('..'))


# --- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

# Get version from your package (optional, but good practice)
try:
    from mesalab.version import __version__ as project_version
except ImportError:
    project_version = 'unknown' # Fallback if mesalab isn't installed or path is wrong

project = 'mesalab' # Use the name defined in setup.py
copyright = '2025, Dora Tarczay-Nehez' # Based on your setup.py
author = 'Dora Tarczay-Nehez' # Based on your setup.py
release = project_version # Use the version imported from your package
version = project_version # The short X.Y version

# --- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    'sphinx.ext.autodoc',     # For automatic documentation from docstrings
    'sphinx.ext.napoleon',    # To support Google and NumPy style docstrings
    'sphinx.ext.todo',        # For todo list directives
    'sphinx.ext.viewcode',    # Adds links to source code from the docs
    'sphinx.ext.mathjax',     # For rendering LaTeX math equations
    'sphinx.ext.intersphinx', # For linking to other Sphinx docs (e.g., NumPy, Pandas)
    'myst_parser', # <--- ADD THIS LINE to enable Markdown parsing

]

myst_enable_extensions = [
    "colon_fence",  # Crucial for directives like ````{toctree}` to be parsed
    # You can add other MyST extensions here if you need them:
    # "deflist",      # For definition lists
    # "fieldlist",    # For Sphinx-style field lists (like :param:)
    # "html_image",   # For raw HTML <img> tags
    # "replacements", # For common text replacements
    # "smartquotes",  # For converting "quotes" to proper curly quotes
    # "strikethrough",# For ~~strikethrough~~ text
    # "substitution", # For |name| substitutions
    # "tasklist",     # For - [ ] checkboxes
]

# Set the default role for intersphinx links if you use them
# default_role = 'py:obj' 

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

master_doc = 'index' # Így kell lennie!


# --- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme' # The popular Read the Docs theme
html_static_path = ['_static']

# --- Autodoc settings --------------------------------------------------------
# This helps autodoc find the right things
autodoc_default_options = {
    'members': True,
    'undoc-members': True,
    'private-members': False, # Set to True if you want to document private members
    'special-members': '__init__',
    'inherited-members': True,
    'show-inheritance': True,
    'member-order': 'bysource', # or 'alphabetical'
}

# Napoleon settings (for Google/NumPy style docstrings)
napoleon_google_docstring = True
napoleon_numpy_docstring = True
napoleon_include_init_with_doc = False
napoleon_include_private_with_doc = False
napoleon_include_special_with_doc = True
napoleon_use_admonition_for_examples = False
napoleon_use_admonition_for_notes = False
napoleon_use_admonition_for_references = False
napoleon_use_ivar = False
napoleon_use_param = True
napoleon_use_rtype = True
napoleon_preprocess_types = False
napoleon_type_aliases = None
napoleon_attr_annotations = True

# Todo settings (if you use :todo: directives)
todo_include_todos = True
