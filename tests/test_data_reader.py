import pytest
from unittest.mock import patch, mock_open
from mesalab.analyzis.data_reader import extract_params_from_inlist, scan_mesa_runs, get_data_from_history_file
import pandas as pd
import os
import io

# ---
# Tests for 'extract_params_from_inlist' function
# ---

def test_extract_params_from_inlist_success():
    """Tests the successful extraction of MESA parameters from an inlist file
    with different formats (e.g., D0, E0)."""
    inlist_content = """
    ! This is a comment
    initial_mass = 1.0d0
    initial_Z = 2.0e-2
    initial_Y = 0.28D0 ! another comment
    """
    
    with patch("builtins.open", new_callable=mock_open, read_data=inlist_content):
        mass, z, y = extract_params_from_inlist("dummy_path/inlist")
        
    assert mass == 1.0
    assert z == 0.02
    assert y == 0.28
    
def test_extract_params_from_inlist_missing_values():
    """Tests handling a case where one or more parameters are missing."""
    inlist_content = """
    initial_mass = 1.5
    """
    
    with patch("builtins.open", new_callable=mock_open, read_data=inlist_content):
        mass, z, y = extract_params_from_inlist("dummy_path/inlist")
        
    assert mass == 1.5
    assert z is None  # The test should check that the missing value is None
    assert y is None
    
def test_extract_params_from_inlist_empty_file():
    """Tests handling an empty inlist file."""
    with patch("builtins.open", new_callable=mock_open, read_data=""):
        mass, z, y = extract_params_from_inlist("dummy_path/inlist")
        
    assert mass is None
    assert z is None
    assert y is None
    
def test_extract_params_from_inlist_file_not_found():
    """Tests the scenario where the inlist file does not exist."""
    with patch("builtins.open", side_effect=FileNotFoundError):
        # Calling the function within pytest.raises ensures the FileNotFoundError
        # is handled by the function itself, not the test failing.
        mass, z, y = extract_params_from_inlist("non_existent_path")
        
    assert mass is None
    assert z is None
    assert y is None

# ---
# Tests for 'scan_mesa_runs' function
# ---

@pytest.fixture
def mock_os_walk():
    """Mocks os.walk to simulate a file system scan."""
    mock_files = {
        'run_A': {'inlist_project', 'LOGS/history.data'},
        'run_B': {'inlist_project', 'LOGS/history.data'},
        'run_C': {'inlist_project'},  # Missing history.data
        'run_D': {'LOGS/history.data'} # Missing inlist
    }
    def mock_walker(path):
        for root, dirs, files in os.walk(path):
            yield root, list(mock_files.keys()), []
    return mock_walker

@patch("os.path.isdir", side_effect=lambda x: "run" in x)
@patch("os.listdir", return_value=["run_A", "run_B", "run_C"])
def test_scan_mesa_runs_success(mock_listdir, mock_isdir):
    """
    Tests a successful scan with two valid folders and one incomplete.
    """
    # Configure the mock for os.path.exists to reflect the real scenario
    def mock_exists_side_effect(path):
        # Only return True for the files that actually exist in your test case
        return "run_A/inlist_project" in path or \
               "run_A/LOGS/history.data" in path or \
               "run_B/inlist_project" in path or \
               "run_B/LOGS/history.data" in path or \
               "run_C/inlist_project" in path  # 'run_C' is missing history.data

    with patch("os.path.exists", side_effect=mock_exists_side_effect):
        # Configure the mock for extract_params_from_inlist
        with patch("mesalab.analyzis.data_reader.extract_params_from_inlist", side_effect=[
            (1.0, 0.014, 0.28), # Expected for run_A
            (2.0, 0.02, 0.25),  # Expected for run_B
            # For run_C, the function call should not happen because os.path.exists will return False for history.data
        ]):
            result = scan_mesa_runs("/dummy_dir", "inlist_project")

    assert len(result) == 2
    assert result[0]['mass'] == 1.0
    assert result[1]['mass'] == 2.0