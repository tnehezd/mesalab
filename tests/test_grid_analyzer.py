# tests/test_grid_analyzer.py
import pytest
import os
import logging
from unittest.mock import patch, MagicMock, call

# Import the function to be tested
# This assumes your provided code lives in 'mesalab/analyzis/grid_analyzer.py'
from mesalab.analyzis.grid_analyzer import analyze_mesa_grid_directory

# Also, we need to mock the imported dependency:
# from ..io.inlist_parser import get_mesa_params_from_inlist
# Since it's a relative import, we need to patch it correctly as it would be seen
# by the grid_analyzer module. The patch target depends on where grid_analyzer.py is.
# If mesalab/analyzis/grid_analyzer.py imports it as `from ..io.inlist_parser import get_mesa_params_from_inlist`,
# then the patch target from the test file would be `mesalab.analyzis.grid_analyzer.get_mesa_params_from_inlist`.

# Configure a logger for tests to capture log messages if needed for assertions
logging.basicConfig(level=logging.INFO)
test_logger = logging.getLogger(__name__)


# --- Tests for analyze_mesa_grid_directory ---

@patch('os.path.isdir')
@patch('os.path.exists')
@patch('os.listdir')
@patch('mesalab.analyzis.grid_analyzer.get_mesa_params_from_inlist') # Patch the dependency
@patch('mesalab.analyzis.grid_analyzer.logging.error')
@patch('mesalab.analyzis.grid_analyzer.logging.warning')
@patch('mesalab.analyzis.grid_analyzer.logging.debug')
def test_analyze_mesa_grid_directory_success(
    mock_debug, mock_warning, mock_error, mock_get_params, mock_listdir, mock_exists, mock_isdir
):
    """
    Tests successful scanning of a grid directory with multiple valid MESA runs.
    """
    dummy_grid_root = "/test/grid_data"
    mesa_output_subdir = "LOGS"
    inlist_filename = "inlist"
    inlist_alternatives = ["inlist_project"]

    # Mock directory structure:
    # /test/grid_data/
    #   run_A/
    #     LOGS/
    #     inlist (parsed)
    #   run_B/
    #     LOGS/
    #     inlist_project (parsed)
    #   non_run_dir/ (no LOGS)
    #   .hidden_dir/
    mock_listdir.return_value = ["run_A", "run_B", "non_run_dir", ".hidden_dir"]

    # Configure os.path.isdir to return True for all immediate subdirs
    mock_isdir.side_effect = lambda path: path in [
        dummy_grid_root,
        os.path.join(dummy_grid_root, "run_A"),
        os.path.join(dummy_grid_root, "run_B"),
        os.path.join(dummy_grid_root, "non_run_dir"),
        os.path.join(dummy_grid_root, ".hidden_dir")
    ]

    # Configure os.path.exists for LOGS directories and inlist files
    def exists_side_effect(path):
        # Base grid root always exists
        if path == dummy_grid_root:
            return True
        # LOGS subdirectories
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_A", "LOGS")):
            return True
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_B", "LOGS")):
            return True
        # Inlist files for runs A and B (one primary, one alternative)
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_A", inlist_filename)):
            return True
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_B", "inlist_project")):
            return True
        return False # Other paths (e.g., non_run_dir/LOGS) do not exist

    mock_exists.side_effect = exists_side_effect

    # Configure the mocked get_mesa_params_from_inlist (the external dependency)
    mock_get_params.side_effect = [
        {'initial_mass': 1.0, 'initial_z': 0.02},  # For run_A
        {'initial_mass': 2.5, 'initial_z': 0.01}   # For run_B (even if inlist_project is the one found)
    ]

    # Call the function under test
    results = analyze_mesa_grid_directory(
        dummy_grid_root, mesa_output_subdir, inlist_filename, inlist_alternatives
    )

    # Assertions
    assert len(results) == 2
    assert results[0]['path'] == os.path.join(dummy_grid_root, "run_A")
    assert results[0]['mass'] == 1.0
    assert results[0]['z'] == 0.02

    assert results[1]['path'] == os.path.join(dummy_grid_root, "run_B")
    assert results[1]['mass'] == 2.5
    assert results[1]['z'] == 0.01

    # Verify that get_mesa_params_from_inlist was called for the correct paths
    mock_get_params.assert_has_calls([
        call(os.path.join(dummy_grid_root, "run_A"), inlist_filename, inlist_alternatives),
        call(os.path.join(dummy_grid_root, "run_B"), inlist_filename, inlist_alternatives)
    ], any_order=True) # Order might vary depending on os.listdir

    mock_error.assert_not_called()
    mock_warning.assert_not_called()
    mock_debug.assert_any_call(f"Checking MESA run directory: '{os.path.join(dummy_grid_root, 'run_A')}'")


@patch('os.path.isdir')
@patch('os.path.exists')
@patch('os.listdir')
@patch('mesalab.analyzis.grid_analyzer.get_mesa_params_from_inlist')
@patch('mesalab.analyzis.grid_analyzer.logging.error')
@patch('mesalab.analyzis.grid_analyzer.logging.warning')
def test_analyze_mesa_grid_directory_invalid_root_path(
    mock_warning, mock_error, mock_get_params, mock_listdir, mock_exists, mock_isdir
):
    """
    Tests behavior when the grid_root_path is not a valid directory.
    """
    dummy_grid_root = "/non/existent/path"
    mock_isdir.return_value = False # Simulate root path not being a directory
    mock_exists.return_value = False
    mock_listdir.return_value = []

    results = analyze_mesa_grid_directory(dummy_grid_root)

    assert results == []
    mock_error.assert_called_once_with(f"Error: Grid root path '{dummy_grid_root}' is not a valid directory.")
    mock_get_params.assert_not_called()
    mock_warning.assert_not_called()


@patch('os.path.isdir')
@patch('os.path.exists')
@patch('os.listdir')
@patch('mesalab.analyzis.grid_analyzer.get_mesa_params_from_inlist')
@patch('mesalab.analyzis.grid_analyzer.logging.warning')
def test_analyze_mesa_grid_directory_missing_logs_or_params(
    mock_warning, mock_get_params, mock_listdir, mock_exists, mock_isdir
):
    """
    Tests scenarios where runs are skipped due to missing LOGS or missing params in inlist.
    """
    dummy_grid_root = "/test/partial_grid"
    mesa_output_subdir = "LOGS"
    inlist_filename = "inlist"

    mock_listdir.return_value = ["run_no_logs", "run_incomplete_inlist", "run_valid"]
    mock_isdir.side_effect = lambda path: path.startswith(dummy_grid_root) and \
                                         not "file" in os.path.basename(path)

    def exists_side_effect(path):
        if path == dummy_grid_root: return True
        # run_no_logs: has inlist, but no LOGS
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_no_logs", inlist_filename)): return True
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_no_logs", mesa_output_subdir)): return False # NO LOGS
        
        # run_incomplete_inlist: has LOGS, but inlist returns incomplete params
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_incomplete_inlist", inlist_filename)): return True
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_incomplete_inlist", mesa_output_subdir)): return True

        # run_valid: has everything
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_valid", inlist_filename)): return True
        if os.path.normpath(path) == os.path.normpath(os.path.join(dummy_grid_root, "run_valid", mesa_output_subdir)): return True
        return False

    mock_exists.side_effect = exists_side_effect

    # Configure mock_get_params: only 'run_valid' returns complete data
    def get_params_side_effect(run_path, *args):
        if "run_incomplete_inlist" in run_path:
            return {'initial_mass': 1.0} # Missing 'initial_z'
        if "run_valid" in run_path:
            return {'initial_mass': 1.2, 'initial_z': 0.008}
        return {} # Default for other cases

    mock_get_params.side_effect = get_params_side_effect

    # Call the function under test
    results = analyze_mesa_grid_directory(dummy_grid_root, mesa_output_subdir, inlist_filename)

    # Assertions
    assert len(results) == 1
    assert results[0]['path'] == os.path.join(dummy_grid_root, "run_valid")
    assert results[0]['mass'] == 1.2
    assert results[0]['z'] == 0.008

    # Verify warnings for skipped runs
    mock_warning.assert_called_once_with(
        f"Could not extract 'initial_mass' or 'initial_z' from inlist in '{os.path.join(dummy_grid_root, 'run_incomplete_inlist')}'. Skipping."
    )
    # Note: No warning/error for 'run_no_logs' because the `if os.path.exists(mesa_output_subdir)` check
    # handles that before `get_mesa_params_from_inlist` is called.

    # get_mesa_params_from_inlist should only be called for directories that pass the LOGS check
    mock_get_params.assert_has_calls([
        call(os.path.join(dummy_grid_root, "run_incomplete_inlist"), inlist_filename, []), # Default for alternatives is []
        call(os.path.join(dummy_grid_root, "run_valid"), inlist_filename, [])
    ], any_order=True)