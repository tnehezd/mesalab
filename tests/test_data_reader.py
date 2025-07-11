# tests/test_data_reader.py
import pytest
import pandas as pd
import numpy as np
import os
import logging
from unittest.mock import patch, mock_open, call

# Import the functions directly from their module
# This assumes your provided code lives in 'mesalab/analyzis/data_reader.py'
from mesalab.analyzis.data_reader import extract_params_from_inlist, scan_mesa_runs, get_data_from_history_file

# Configure a logger for tests to capture log messages if needed for assertions
logging.basicConfig(level=logging.INFO)
test_logger = logging.getLogger(__name__)


# --- Tests for extract_params_from_inlist ---

@patch("builtins.open", new_callable=mock_open)
@patch("mesalab.analyzis.data_reader.logger.warning")
def test_extract_params_from_inlist_success(mock_warn, mock_file):
    """
    Tests successful extraction of mass and Z from an inlist with various number formats.
    """
    inlist_content = """
    ! some comments
    initial_mass = 1.0d0
    initial_Z = 2.0e-2 ! another comment
    some_other_param = 123
    """
    mock_file.return_value.read.return_value = inlist_content

    mass, z = extract_params_from_inlist("test_path/inlist_full")

    assert mass == 1.0
    assert z == 0.02
    mock_warn.assert_not_called()


@patch("builtins.open", new_callable=mock_open)
@patch("mesalab.analyzis.data_reader.logger.warning")
def test_extract_params_from_inlist_missing_mass(mock_warn, mock_file):
    """
    Tests when initial_mass is missing from the inlist.
    """
    inlist_content = """
    initial_Z = 0.014
    """
    mock_file.return_value.read.return_value = inlist_content

    mass, z = extract_params_from_inlist("test_path/inlist_no_mass")

    assert mass is None
    assert z == 0.014
    mock_warn.assert_called_once_with("Could not find 'initial_mass' in test_path/inlist_no_mass.")


@patch("builtins.open", new_callable=mock_open)
@patch("mesalab.analyzis.data_reader.logger.warning")
def test_extract_params_from_inlist_missing_z(mock_warn, mock_file):
    """
    Tests when initial_Z is missing from the inlist.
    """
    inlist_content = """
    initial_mass = 2.5
    """
    mock_file.return_value.read.return_value = inlist_content

    mass, z = extract_params_from_inlist("test_path/inlist_no_z")

    assert mass == 2.5
    assert z is None
    mock_warn.assert_called_once_with("Could not find 'initial_Z' in test_path/inlist_no_z.")


@patch("builtins.open", new_callable=mock_open)
@patch("mesalab.analyzis.data_reader.logger.error")
def test_extract_params_from_inlist_file_not_found(mock_error, mock_file):
    """
    Tests handling of FileNotFoundError.
    """
    mock_file.side_effect = FileNotFoundError

    mass, z = extract_params_from_inlist("non_existent_path/inlist")

    assert mass is None
    assert z is None
    mock_error.assert_called_once_with("Inlist file not found: non_existent_path/inlist")




# --- Tests for scan_mesa_runs ---

@patch('os.listdir')
@patch('os.path.isdir')
@patch('os.path.exists')
@patch('mesalab.analyzis.data_reader.extract_params_from_inlist') # Patch the internal function called by scan_mesa_runs
@patch('mesalab.analyzis.data_reader.logger.warning')
@patch('mesalab.analyzis.data_reader.logger.info')
def test_scan_mesa_runs_success(mock_info, mock_warning, mock_extract_params, mock_exists, mock_isdir, mock_listdir):
    """
    Tests scan_mesa_runs with valid MESA run directories.
    """
    input_dir = "/dummy/mesa_runs"
    inlist_name = "inlist_project"

    mock_listdir.return_value = ["run_01", "run_02", ".hidden_dir"] # Simulate subdirectories
    mock_isdir.side_effect = lambda x: True # All listed items are directories

    # Simulate specific file existence and content
    mock_exists.side_effect = lambda path: any(
        target_path in path for target_path in [
            os.path.join(input_dir, "run_01", inlist_name),
            os.path.join(input_dir, "run_01", "LOGS", "history.data"),
            os.path.join(input_dir, "run_02", inlist_name),
            os.path.join(input_dir, "run_02", "LOGS", "history.data")
        ]
    )

    # Configure mock_extract_params_from_inlist to return values
    mock_extract_params.side_effect = [
        (1.0, 0.02), # For run_01
        (1.5, 0.01)  # For run_02
    ]

    result = scan_mesa_runs(input_dir, inlist_name)

    assert len(result) == 2
    assert result[0]['mass'] == 1.0
    assert result[0]['z'] == 0.02
    assert result[0]['run_dir_path'] == os.path.join(input_dir, "run_01")
    assert result[0]['history_file_path'] == os.path.join(input_dir, "run_01", "LOGS", "history.data")

    assert result[1]['mass'] == 1.5
    assert result[1]['z'] == 0.01
    assert result[1]['run_dir_path'] == os.path.join(input_dir, "run_02")
    assert result[1]['history_file_path'] == os.path.join(input_dir, "run_02", "LOGS", "history.data")

    # Verify that extract_params_from_inlist was called for each valid inlist
    mock_extract_params.assert_has_calls([
        call(os.path.join(input_dir, "run_01", inlist_name)),
        call(os.path.join(input_dir, "run_02", inlist_name))
    ])
    mock_warning.assert_not_called()
    mock_info.assert_any_call(f"Finished scanning. Found 2 valid MESA runs.")


@patch('os.listdir')
@patch('os.path.isdir')
@patch('os.path.exists')
@patch('mesalab.analyzis.data_reader.extract_params_from_inlist')
@patch('mesalab.analyzis.data_reader.logger.warning')
@patch('mesalab.analyzis.data_reader.logger.info')
def test_scan_mesa_runs_no_subdirectories(mock_info, mock_warning, mock_extract_params, mock_exists, mock_isdir, mock_listdir):
    """
    Tests scan_mesa_runs when the input directory contains no subdirectories.
    """
    input_dir = "/dummy/empty_runs"
    inlist_name = "inlist_project"

    mock_listdir.return_value = [] # Simulate no subdirectories
    mock_isdir.return_value = False
    mock_exists.return_value = False

    result = scan_mesa_runs(input_dir, inlist_name)

    assert result == []
    mock_warning.assert_called_once_with(f"No non-hidden subdirectories found directly in '{input_dir}'. Ensure your MESA runs are in individual folders within this input directory.")
    mock_extract_params.assert_not_called()


@patch('os.listdir')
@patch('os.path.isdir')
@patch('os.path.exists')
@patch('mesalab.analyzis.data_reader.extract_params_from_inlist')
@patch('mesalab.analyzis.data_reader.logger.warning')
@patch('mesalab.analyzis.data_reader.logger.info')
def test_scan_mesa_runs_missing_files(mock_info, mock_warning, mock_extract_params, mock_exists, mock_isdir, mock_listdir):
    """
    Tests scan_mesa_runs when subdirectories exist but are missing inlist or history.data.
    """
    input_dir = "/dummy/incomplete_runs"
    inlist_name = "inlist_project"

    mock_listdir.return_value = ["run_missing_inlist", "run_missing_history", "run_invalid_params"]
    mock_isdir.side_effect = lambda x: True

    def custom_exists(path):
        if "run_missing_inlist" in path and inlist_name in path:
            return False # inlist missing
        if "run_missing_inlist" in path and "history.data" in path:
            return True # history exists
        if "run_missing_history" in path and inlist_name in path:
            return True # inlist exists
        if "run_missing_history" in path and "history.data" in path:
            return False # history missing
        if "run_invalid_params" in path and inlist_name in path:
            return True
        if "run_invalid_params" in path and "history.data" in path:
            return True
        return False

    mock_exists.side_effect = custom_exists

    mock_extract_params.side_effect = [
        (None, 0.02), # For 'run_invalid_params', simulating missing mass
    ]

    result = scan_mesa_runs(input_dir, inlist_name)

    assert result == []

    mock_info.assert_any_call(f"Inlist file '{inlist_name}' not found in '{os.path.join(input_dir, 'run_missing_inlist')}'. Skipping this directory.")
    mock_info.assert_any_call(f"history.data not found at '{os.path.join(input_dir, 'run_missing_history', 'LOGS', 'history.data')}'. Skipping this directory.")
    mock_warning.assert_called_once_with(f"Could not extract mass/Z from inlist '{os.path.join(input_dir, 'run_invalid_params', inlist_name)}'. Skipping this run.")
    mock_extract_params.assert_called_once()


# --- Tests for get_data_from_history_file ---

@patch('os.path.exists')
@patch('numpy.genfromtxt')
@patch('mesalab.analyzis.data_reader.logger.error')
def test_get_data_from_history_file_success(mock_error, mock_genfromtxt, mock_exists):
    """
    Tests successful loading of a history.data file.
    """
    history_path = "/dummy/run/LOGS/history.data"
    mock_exists.return_value = True

    mock_data_dtype = [('model_number', '<i4'), ('star_age', '<f8'), ('log_Teff', '<f8')]
    mock_data = np.array([
        (1, 0.1, 3.8),
        (2, 0.2, 3.75),
        (3, 0.3, 3.7)
    ], dtype=mock_data_dtype)
    mock_genfromtxt.return_value = mock_data

    df = get_data_from_history_file(history_path)

    mock_exists.assert_called_once_with(history_path)
    mock_genfromtxt.assert_called_once_with(
        history_path, names=True, comments="#", skip_header=5, dtype=None, encoding='utf-8'
    )
    mock_error.assert_not_called()

    assert not df.empty
    pd.testing.assert_frame_equal(df, pd.DataFrame(mock_data).astype({'model_number': int}))


@patch('os.path.exists')
@patch('numpy.genfromtxt')
@patch('mesalab.analyzis.data_reader.logger.error')
def test_get_data_from_history_file_file_not_found(mock_error, mock_genfromtxt, mock_exists):
    """
    Tests handling of FileNotFoundError in get_data_from_history_file.
    """
    history_path = "/dummy/nonexistent/history.data"
    mock_exists.return_value = False

    df = get_data_from_history_file(history_path)

    mock_exists.assert_called_once_with(history_path)
    mock_genfromtxt.assert_not_called()
    mock_error.assert_called_once_with(f"History file not found: {history_path}")
    assert df.empty


@patch('os.path.exists')
@patch('numpy.genfromtxt')
@patch('mesalab.analyzis.data_reader.logger.error')
def test_get_data_from_history_file_genfromtxt_error(mock_error, mock_genfromtxt, mock_exists):
    """
    Tests handling of a parsing error from numpy.genfromtxt.
    """
    history_path = "/dummy/corrupted/history.data"
    mock_exists.return_value = True
    mock_genfromtxt.side_effect = ValueError("Corrupted file format")

    df = get_data_from_history_file(history_path)

    mock_exists.assert_called_once_with(history_path)
    mock_genfromtxt.assert_called_once()
    mock_error.assert_called_once_with(f"Error loading or processing {history_path} using np.genfromtxt: Corrupted file format")
    assert df.empty


@patch('os.path.exists')
@patch('numpy.genfromtxt')
@patch('mesalab.analyzis.data_reader.logger.error')
def test_get_data_from_history_file_single_row(mock_error, mock_genfromtxt, mock_exists):
    """
    Tests handling of a history.data file containing only a single data row,
    which sometimes causes genfromtxt to return a 0-D array.
    """
    history_path = "/dummy/single_row/history.data"
    mock_exists.return_value = True

    mock_data_dtype = [('model_number', '<i4'), ('star_age', '<f8')]
    mock_data_single_row = np.array((10, 0.5), dtype=mock_data_dtype)
    mock_genfromtxt.return_value = mock_data_single_row

    df = get_data_from_history_file(history_path)

    assert not df.empty
    assert len(df) == 1
    assert df.iloc[0]['model_number'] == 10
    assert df.iloc[0]['star_age'] == 0.5
    # Change np.intc to np.int64 or just check if it's an integer type
    assert pd.api.types.is_integer_dtype(df['model_number']) # More robust check
    mock_error.assert_not_called()


@patch('os.path.exists')
@patch('numpy.genfromtxt')
@patch('mesalab.analyzis.data_reader.logger.error')
def test_get_data_from_history_file_non_numeric_conversion(mock_error, mock_genfromtxt, mock_exists):
    """
    Tests that non-numeric values in columns are correctly coerced to NaN.
    """
    history_path = "/dummy/mixed_data/history.data"
    mock_exists.return_value = True

    mock_data_dtype = [('model_number', '<i4'), ('log_Teff', 'U10')]
    mock_data = np.array([
        (1, '3.8'),
        (2, 'INVALID'), # This string should become NaN
        (3, '3.7')
    ], dtype=mock_data_dtype)
    mock_genfromtxt.return_value = mock_data

    df = get_data_from_history_file(history_path)

    assert not df.empty
    assert 'log_Teff' in df.columns
    assert pd.isna(df.loc[1, 'log_Teff'])
    assert df.loc[0, 'log_Teff'] == 3.8
    assert df.loc[2, 'log_Teff'] == 3.7
    assert df['log_Teff'].dtype == np.float64
    mock_error.assert_not_called()