# tests/test_inlist_parser.py
import os
import pytest
from mesa_tools.inlist_parser import get_mesa_params_from_inlist

# A pytest fixture to create a temporary inlist file for each test
# This ensures tests are isolated and don't interfere with each other or real files.
@pytest.fixture
def temp_inlist_file(tmp_path):
    """Creates a temporary inlist_project file for testing."""
    file_path = tmp_path / "inlist_project"
    yield file_path # Yield the path to the test function
    # No explicit cleanup needed, tmp_path handles it

def test_get_mesa_params_standard_format(temp_inlist_file):
    """Test parsing of standard initial_mass and Zbase."""
    content = "initial_mass = 1.0\nZbase = 0.02"
    temp_inlist_file.write_text(content)
    mass, z = get_mesa_params_from_inlist(str(temp_inlist_file))
    assert mass == 1.0
    assert z == 0.02

def test_get_mesa_params_exponential_notation(temp_inlist_file):
    """Test parsing with exponential notation and alternative names."""
    content = "new_mass = 2.5e-1\nnew_z = 8.0e-3"
    temp_inlist_file.write_text(content)
    mass, z = get_mesa_params_from_inlist(str(temp_inlist_file))
    assert mass == 0.25 # 2.5e-1
    assert z == 0.008 # 8.0e-3

def test_get_mesa_params_whitespace_and_case_insensitivity(temp_inlist_file):
    """Test parsing with extra whitespace and mixed case."""
    content = "  Initial_Mass   =   3.0   \nZBASE = 0.01\n"
    temp_inlist_file.write_text(content)
    mass, z = get_mesa_params_from_inlist(str(temp_inlist_file))
    assert mass == 3.0
    assert z == 0.01

def test_get_mesa_params_missing_parameters(temp_inlist_file):
    """Test case where one or both parameters are missing."""
    content_mass_only = "initial_mass = 4.0\n"
    temp_inlist_file.write_text(content_mass_only)
    mass, z = get_mesa_params_from_inlist(str(temp_inlist_file))
    assert mass == 4.0
    assert z is None

    content_z_only = "Zbase = 0.03\n"
    temp_inlist_file.write_text(content_z_only)
    mass, z = get_mesa_params_from_inlist(str(temp_inlist_file))
    assert mass is None
    assert z == 0.03

    content_empty = ""
    temp_inlist_file.write_text(content_empty)
    mass, z = get_mesa_params_from_inlist(str(temp_inlist_file))
    assert mass is None
    assert z is None

def test_get_mesa_params_non_existent_file():
    """Test calling with a non-existent file path."""
    mass, z = get_mesa_params_from_inlist("non_existent_path/fake_inlist.txt")
    assert mass is None
    assert z is None

def test_get_mesa_params_invalid_content(temp_inlist_file):
    """Test parsing with malformed content."""
    content = "initial_mass = abc\nZbase = def" # Invalid numbers
    temp_inlist_file.write_text(content)
    mass, z = get_mesa_params_from_inlist(str(temp_inlist_file))
    assert mass is None
    assert z is None