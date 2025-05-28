# tests/test_grid_analyzer.py
import os
import pytest
from mesa_tools.grid_analyzer import analyze_mesa_grid_directory

# Fixture to set up a temporary MESA grid structure
@pytest.fixture
def mock_mesa_grid(tmp_path):
    """
    Creates a temporary directory structure resembling a MESA grid
    with inlist_project files.
    """
    # Define a base directory within the temporary path
    base_grid_path = tmp_path / "mock_mesa_runs"
    base_grid_path.mkdir()

    # Create run directories and inlist_project files
    # Run 1: Standard
    run1_path = base_grid_path / "run_M1_Z002"
    run1_path.mkdir()
    (run1_path / "inlist_project").write_text("initial_mass = 1.0\nZbase = 0.02")

    # Run 2: Exponential notation
    run2_path = base_grid_path / "run_M05_Z001"
    run2_path.mkdir()
    (run2_path / "inlist_project").write_text("new_mass = 0.5e0\nnew_z = 1.0e-2")

    # Run 3: Nested directory
    nested_path = base_grid_path / "nested" / "run_M3_Z0008"
    nested_path.mkdir(parents=True) # Create parent directories as well
    (nested_path / "inlist_project").write_text("initial_mass = 3.0\nZbase = 0.008")

    # Run 4: Directory without inlist_project (should be ignored)
    (base_grid_path / "empty_run").mkdir()

    # Run 5: Directory with malformed inlist (should be ignored or return None)
    run5_path = base_grid_path / "run_malformed"
    run5_path.mkdir()
    (run5_path / "inlist_project").write_text("initial_mass = invalid\nZbase = invalid")

    yield str(base_grid_path) # Yield the string path for the tests

def test_analyze_mesa_grid_directory_valid_runs(mock_mesa_grid):
    """Test that analyze_mesa_grid_directory correctly finds and parses valid runs."""
    found_runs = analyze_mesa_grid_directory(mock_mesa_grid)

    # We expect 3 valid runs (run1, run2, run3)
    assert len(found_runs) == 3

    # Check that the data is correctly extracted and paths are correct
    # Sort by mass for consistent ordering in tests
    found_runs.sort(key=lambda x: x['mass'])

    # Test run 1
    assert found_runs[0]['mass'] == 0.5
    assert found_runs[0]['z'] == 0.01
    assert "run_M05_Z001" in found_runs[0]['path']

    # Test run 2
    assert found_runs[1]['mass'] == 1.0
    assert found_runs[1]['z'] == 0.02
    assert "run_M1_Z002" in found_runs[1]['path']

    # Test run 3 (nested)
    assert found_runs[2]['mass'] == 3.0
    assert found_runs[2]['z'] == 0.008
    assert os.path.join("nested", "run_M3_Z0008") in found_runs[2]['path']


def test_analyze_mesa_grid_directory_non_existent_path():
    """Test calling with a non-existent root path."""
    found_runs = analyze_mesa_grid_directory("non_existent_grid_path")
    assert len(found_runs) == 0

def test_analyze_mesa_grid_directory_empty_root(tmp_path):
    """Test calling with an empty root directory."""
    empty_root_path = tmp_path / "empty_root"
    empty_root_path.mkdir()
    found_runs = analyze_mesa_grid_directory(str(empty_root_path))
    assert len(found_runs) == 0

def test_analyze_mesa_grid_directory_no_inlist_files(tmp_path):
    """Test a directory structure with no inlist_project files."""
    no_inlist_path = tmp_path / "no_inlist_folder"
    no_inlist_path.mkdir()
    (no_inlist_path / "sub1").mkdir()
    (no_inlist_path / "sub1" / "sub2").mkdir()
    found_runs = analyze_mesa_grid_directory(str(no_inlist_path))
    assert len(found_runs) == 0