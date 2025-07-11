# tests/test_mesa_analyzer.py
import pytest
import os
import pandas as pd
import numpy as np
import yaml
import logging
from unittest.mock import patch, MagicMock, mock_open
from types import SimpleNamespace # To simulate the 'args' object

from mesalab.analyzis.mesa_analyzer import perform_mesa_analysis


# Configure the logger for tests to see log messages
logging.basicConfig(level=logging.INFO)
test_logger = logging.getLogger(__name__)

# --- Fixtures for setting up test data and environment ---

@pytest.fixture
def mock_args_base():
    """Base mock args object for tests."""
    args = SimpleNamespace(
        general_settings=SimpleNamespace(
            input_dir="dummy_input_dir",
            inlist_name="inlist",
            force_reanalysis=False
        ),
        blue_loop_analysis=SimpleNamespace(
            analyze_blue_loop=False,
            blue_loop_output_type="summary" # Or "all"
        ),
        gyre_workflow=SimpleNamespace(
            run_gyre_workflow=False
        )
    )
    return args

@pytest.fixture
def mock_mesa_run_infos():
    """Mock data returned by the scan_mesa_runs function."""
    return [
        {'mass': 1.0, 'z': 0.02, 'run_dir_path': '/dummy/path/M1.0Z0.02', 'history_file_path': '/dummy/path/M1.0Z0.02/history.data'},
        {'mass': 1.2, 'z': 0.02, 'run_dir_path': '/dummy/path/M1.2Z0.02', 'history_file_path': '/dummy/path/M1.2Z0.02/history.data'},
        {'mass': 1.0, 'z': 0.01, 'run_dir_path': '/dummy/path/M1.0Z0.01', 'history_file_path': '/dummy/path/M1.0Z0.01/history.data'},
    ]

@pytest.fixture
def mock_history_df():
    """Dummy DataFrame to simulate the content of a MESA history file."""
    return pd.DataFrame({
        'model_number': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        'star_age': np.linspace(0.1, 1.0, 10),
        'log_Teff': np.linspace(3.7, 4.0, 10),
        'log_L': np.linspace(1.0, 2.0, 10),
        'log_g': np.linspace(4.0, 3.5, 10),
        'log_R': np.linspace(0.0, 1.0, 10),
        'profile_number': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        'log_dt': np.linspace(0.1, 0.2, 10), # May be needed for analyze_blue_loop_and_instability
    })

@pytest.fixture
def mock_blue_loop_analysis_output():
    """Mock data returned by the analyze_blue_loop_and_instability function."""
    # Simulate a successful blue loop detection
    blue_loop_df = pd.DataFrame({
        'initial_mass': [1.0, 1.0, 1.0],
        'initial_Z': [0.02, 0.02, 0.02],
        'star_age': [0.3, 0.4, 0.5],
        'model_number': [3, 4, 5],
        'log_Teff': [3.8, 3.9, 3.85],
        'log_L': [1.3, 1.5, 1.4],
        'log_g': [3.8, 3.7, 3.75],
        'profile_number': [3, 4, 5]
    })
    return {
        'blue_loop_detail_df': blue_loop_df,
        'crossing_count': 1,
        'state_times': {
            'first_is_entry_age': 0.25,
            'last_is_exit_age': 0.55,
            'instability_start_age': 0.35,
            'instability_end_age': 0.45,
        }
    }

@pytest.fixture
def mock_empty_blue_loop_analysis_output():
    """Mock data returned by analyze_blue_loop_and_instability for an unsuccessful blue loop."""
    return {
        'blue_loop_detail_df': pd.DataFrame(), # Empty DataFrame if no blue loop
        'crossing_count': 0,
        'state_times': {}
    }


# --- Tests for the perform_mesa_analysis function ---

# The @patch decorators replace real functions/objects with mock objects
# during the test. This allows us to control their behavior.
@patch('mesalab.analyzis.mesa_analyzer.scan_mesa_runs')
@patch('mesalab.analyzis.mesa_analyzer.get_data_from_history_file')
@patch('mesalab.analyzis.mesa_analyzer.analyze_blue_loop_and_instability')
@patch('mesalab.analyzis.mesa_analyzer.os.path.exists')
@patch('mesalab.analyzis.mesa_analyzer.os.listdir')
@patch('mesalab.analyzis.mesa_analyzer.os.makedirs')
@patch('mesalab.analyzis.mesa_analyzer.os.remove')
@patch('mesalab.analyzis.mesa_analyzer.pd.read_csv')
@patch('mesalab.analyzis.mesa_analyzer.pd.DataFrame.to_csv')
@patch('mesalab.analyzis.mesa_analyzer.yaml.dump')
def test_perform_mesa_analysis_reanalysis_needed_all_blue_loop(
    mock_yaml_dump, mock_to_csv, mock_read_csv, mock_os_remove, mock_os_makedirs, mock_os_listdir, mock_os_path_exists,
    mock_analyze_blue_loop_and_instability, mock_get_data_from_history_file, mock_scan_mesa_runs,
    mock_args_base, mock_mesa_run_infos, mock_history_df, mock_blue_loop_analysis_output
):
    """
    Tests perform_mesa_analysis when a full reanalysis is needed (force_reanalysis=True)
    and blue loop analysis is enabled with 'all' output type.
    """
    # Configure the args object for this specific test
    args = mock_args_base
    args.general_settings.force_reanalysis = True
    args.blue_loop_analysis.analyze_blue_loop = True
    args.blue_loop_analysis.blue_loop_output_type = "all" # Changed output type

    # Configure the behavior of the mocked dependencies
    mock_os_path_exists.return_value = False # Simulate that CSVs do not exist
    mock_scan_mesa_runs.return_value = mock_mesa_run_infos
    mock_get_data_from_history_file.return_value = mock_history_df
    mock_analyze_blue_loop_and_instability.return_value = mock_blue_loop_analysis_output

    analysis_results_sub_dir = "/tmp/analysis_results"
    detail_files_output_dir = "/tmp/detail_files"
    gyre_input_csv_name = "gyre_input.csv"

    # Call the function under test
    summary_df, detail_df, history_dict, gyre_csv_path = perform_mesa_analysis(
        args, analysis_results_sub_dir, detail_files_output_dir, gyre_input_csv_name
    )

    # --- Assertions (Checks) ---

    # Verify that the main mocked functions were called
    mock_scan_mesa_runs.assert_called_once_with(args.general_settings.input_dir, args.general_settings.inlist_name)
    assert mock_get_data_from_history_file.call_count == len(mock_mesa_run_infos)
    assert mock_analyze_blue_loop_and_instability.call_count == len(mock_mesa_run_infos) # Should be called for each run

    # Verify file system operations
    mock_os_makedirs.assert_any_call(analysis_results_sub_dir, exist_ok=True)
    mock_os_makedirs.assert_any_call(detail_files_output_dir, exist_ok=True)
    mock_os_remove.assert_called_with(os.path.join(analysis_results_sub_dir, "skipped_runs_log.txt")) # Log file cleared

    # Verify that summary, cross-grid, and GYRE input CSVs were saved
    assert mock_to_csv.call_count >= 3 # Summary, Cross-grid, GYRE input, and detail CSVs per Z
    # Check for specific calls (order might vary)
    call_args_list = [call.args[0] for call in mock_to_csv.call_args_list]
    
    summary_path_in_calls = [path for path in call_args_list if "summary_results.csv" in path]
    assert len(summary_path_in_calls) == 1
    assert summary_path_in_calls[0] == os.path.join(analysis_results_sub_dir, "summary_results.csv")

    cross_path_in_calls = [path for path in call_args_list if "crossing_count_grid.csv" in path]
    assert len(cross_path_in_calls) == 1
    assert cross_path_in_calls[0] == os.path.join(analysis_results_sub_dir, "crossing_count_grid.csv")

    gyre_path_in_calls = [path for path in call_args_list if gyre_input_csv_name in path]
    assert len(gyre_path_in_calls) == 1
    assert gyre_path_in_calls[0] == os.path.join(analysis_results_sub_dir, gyre_input_csv_name)

    # Verify the returned DataFrames
    assert not summary_df.empty
    assert 'blue_loop_crossing_count' in summary_df.columns
    assert 'blue_loop_duration_yr' in summary_df.columns

    assert not detail_df.empty
    assert 'log_Teff' in detail_df.columns # Because 'all' output type was chosen

    assert len(history_dict) > 0 # Full history data should be collected

    assert gyre_csv_path == os.path.join(analysis_results_sub_dir, gyre_input_csv_name)
    assert gyre_csv_path is not None and gyre_csv_path != ""


@patch('mesalab.analyzis.mesa_analyzer.scan_mesa_runs')
@patch('mesalab.analyzis.mesa_analyzer.get_data_from_history_file')
@patch('mesalab.analyzis.mesa_analyzer.analyze_blue_loop_and_instability')
@patch('mesalab.analyzis.mesa_analyzer.os.path.exists')
@patch('mesalab.analyzis.mesa_analyzer.os.listdir')
@patch('mesalab.analyzis.mesa_analyzer.os.makedirs')
@patch('mesalab.analyzis.mesa_analyzer.os.remove')
@patch('mesalab.analyzis.mesa_analyzer.pd.read_csv')
@patch('mesalab.analyzis.mesa_analyzer.pd.DataFrame.to_csv')
@patch('mesalab.analyzis.mesa_analyzer.yaml.dump')
def test_perform_mesa_analysis_no_reanalysis_needed(
    mock_yaml_dump, mock_to_csv, mock_read_csv, mock_os_remove, mock_os_makedirs, mock_os_listdir, mock_os_path_exists,
    mock_analyze_blue_loop_and_instability, mock_get_data_from_history_file, mock_scan_mesa_runs,
    mock_args_base
):
    """
    Tests perform_mesa_analysis when no reanalysis is needed (all CSVs already exist).
    """
    args = mock_args_base
    args.general_settings.force_reanalysis = False
    args.blue_loop_analysis.analyze_blue_loop = True # Blue loop analysis enabled
    args.gyre_workflow.run_gyre_workflow = True # GYRE workflow enabled

    # Simulate that all CSVs already exist
    mock_os_path_exists.side_effect = lambda x: x in [
        os.path.join("/tmp/analysis_results", "summary_results.csv"),
        os.path.join("/tmp/analysis_results", "crossing_count_grid.csv"),
        os.path.join("/tmp/analysis_results", "gyre_input.csv"),
        "/tmp/detail_files" # Directory exists
    ]
    mock_os_listdir.return_value = ["detail_z0.0200.csv"] # At least one detail CSV exists

    # Mock `pd.read_csv` to return appropriate DataFrames when the function tries to load them.
    mock_summary_df = pd.DataFrame({
        'initial_mass': [1.0, 1.2],
        'initial_Z': [0.02, 0.02],
        'blue_loop_crossing_count': [1, 0], # One has a blue loop, one doesn't
        'blue_loop_duration_yr': [1000.0, np.nan],
        'run_dir_path': ['/dummy/path/M1.0Z0.02', '/dummy/path/M1.2Z0.02']
    }).set_index(['initial_Z', 'initial_mass'])

    mock_detail_df = pd.DataFrame({
        'initial_mass': [1.0],
        'initial_Z': [0.02],
        'star_age': [0.1, 0.2, 0.3],
        'model_number': [1, 2, 3],
        'log_Teff': [3.7, 3.8, 3.9],
        'log_L': [1.0, 1.1, 1.2]
    })

    def mock_read_csv_side_effect(path, **kwargs):
        if "summary_results.csv" in path:
            return mock_summary_df
        elif "detail_z0.0200.csv" in path:
            return mock_detail_df
        return pd.DataFrame() # Return empty DF otherwise

    mock_read_csv.side_effect = mock_read_csv_side_effect


    analysis_results_sub_dir = "/tmp/analysis_results"
    detail_files_output_dir = "/tmp/detail_files"
    gyre_input_csv_name = "gyre_input.csv"

    summary_df, detail_df, history_dict, gyre_csv_path = perform_mesa_analysis(
        args, analysis_results_sub_dir, detail_files_output_dir, gyre_input_csv_name
    )

    # Verify that the full analysis functions were NOT called
    mock_scan_mesa_runs.assert_not_called()
    mock_get_data_from_history_file.assert_not_called()
    mock_analyze_blue_loop_and_instability.assert_not_called()
    mock_yaml_dump.assert_not_called()
    mock_os_remove.assert_not_called()

    # Verify that CSVs were read
    mock_read_csv.assert_any_call(os.path.join(analysis_results_sub_dir, "summary_results.csv"), index_col=['initial_Z', 'initial_mass'])
    mock_read_csv.assert_any_call(os.path.join(detail_files_output_dir, "detail_z0.0200.csv")) # Check detail CSV read

    # Verify that no new saving occurred (unless internal logic dictates otherwise)
    # The GYRE input CSV should ideally not be re-saved if it exists and force_reanalysis is False.
    mock_to_csv.assert_not_called() # Assuming no new saves happen in this branch

    # Verify the returned DataFrames
    assert not summary_df.empty
    # The summary_df should be filtered (only blue_loop_crossing_count > 0)
    assert len(summary_df) == 1 # Only one entry had a blue loop in mock_summary_df
    assert summary_df.index[0] == (0.02, 1.0)

    assert not detail_df.empty
    assert 'log_Teff' in detail_df.columns # Columns from the loaded mock detail DF

    assert not history_dict # No history dict returned because analysis was skipped

    # If the GYRE input CSV exists, the return value should be its path
    assert gyre_csv_path == os.path.join(analysis_results_sub_dir, gyre_input_csv_name)


@patch('mesalab.analyzis.mesa_analyzer.scan_mesa_runs')
@patch('mesalab.analyzis.mesa_analyzer.get_data_from_history_file')
@patch('mesalab.analyzis.mesa_analyzer.analyze_blue_loop_and_instability')
@patch('mesalab.analyzis.mesa_analyzer.os.path.exists')
@patch('mesalab.analyzis.mesa_analyzer.os.listdir')
@patch('mesalab.analyzis.mesa_analyzer.os.makedirs')
@patch('mesalab.analyzis.mesa_analyzer.os.remove')
@patch('mesalab.analyzis.mesa_analyzer.pd.read_csv')
@patch('mesalab.analyzis.mesa_analyzer.pd.DataFrame.to_csv')
@patch('mesalab.analyzis.mesa_analyzer.yaml.dump')
def test_perform_mesa_analysis_no_blue_loop_analysis(
    mock_yaml_dump, mock_to_csv, mock_read_csv, mock_os_remove, mock_os_makedirs, mock_os_listdir, mock_os_path_exists,
    mock_analyze_blue_loop_and_instability, mock_get_data_from_history_file, mock_scan_mesa_runs,
    mock_args_base, mock_mesa_run_infos, mock_history_df
):
    """
    Tests the scenario when blue loop analysis is turned off.
    In this case, the summary CSV should contain all successful runs, and no detail CSVs should be generated.
    """
    args = mock_args_base
    args.general_settings.force_reanalysis = True
    args.blue_loop_analysis.analyze_blue_loop = False # Blue loop analysis disabled
    args.gyre_workflow.run_gyre_workflow = True # Gyre workflow enabled

    mock_os_path_exists.return_value = False # Force reanalysis
    mock_scan_mesa_runs.return_value = mock_mesa_run_infos
    mock_get_data_from_history_file.return_value = mock_history_df
    # analyze_blue_loop_and_instability should NOT be called!
    # No need to set return_value as it won't be invoked.

    analysis_results_sub_dir = "/tmp/analysis_results_nobl"
    detail_files_output_dir = "/tmp/detail_files_nobl"
    gyre_input_csv_name = "gyre_input_nobl.csv"

    summary_df, detail_df, history_dict, gyre_csv_path = perform_mesa_analysis(
        args, analysis_results_sub_dir, detail_files_output_dir, gyre_input_csv_name
    )

    # Verify that the blue loop analysis function was NOT called
    mock_analyze_blue_loop_and_instability.assert_not_called()

    # Verify that detail CSVs were NOT saved
    call_args_list = [call.args[0] for call in mock_to_csv.call_args_list]
    assert not any("detail_z" in path for path in call_args_list)

    # Verify the returned DataFrames
    assert not summary_df.empty
    assert len(summary_df) == len(mock_mesa_run_infos) # All runs should be included
    assert pd.isna(summary_df['blue_loop_crossing_count']).all() # No blue loop data recorded

    assert detail_df.empty # Detail DataFrame should be empty

    assert len(history_dict) == len(mock_mesa_run_infos) # History data should still be collected
    assert gyre_csv_path == os.path.join(analysis_results_sub_dir, gyre_input_csv_name)


@patch('mesalab.analyzis.mesa_analyzer.scan_mesa_runs')
@patch('mesalab.analyzis.mesa_analyzer.get_data_from_history_file')
@patch('mesalab.analyzis.mesa_analyzer.analyze_blue_loop_and_instability')
@patch('mesalab.analyzis.mesa_analyzer.os.path.exists')
@patch('mesalab.analyzis.mesa_analyzer.os.listdir')
@patch('mesalab.analyzis.mesa_analyzer.os.makedirs')
@patch('mesalab.analyzis.mesa_analyzer.os.remove')
@patch('mesalab.analyzis.mesa_analyzer.pd.read_csv')
@patch('mesalab.analyzis.mesa_analyzer.pd.DataFrame.to_csv')
@patch('mesalab.analyzis.mesa_analyzer.yaml.dump')
def test_perform_mesa_analysis_no_gyre_workflow(
    mock_yaml_dump, mock_to_csv, mock_read_csv, mock_os_remove, mock_os_makedirs, mock_os_listdir, mock_os_path_exists,
    mock_analyze_blue_loop_and_instability, mock_get_data_from_history_file, mock_scan_mesa_runs,
    mock_args_base, mock_mesa_run_infos, mock_history_df, mock_blue_loop_analysis_output
):
    """
    Tests the scenario when the GYRE workflow is turned off.
    In this case, the GYRE input CSV should not be generated.
    """
    args = mock_args_base
    args.general_settings.force_reanalysis = True
    args.blue_loop_analysis.analyze_blue_loop = True
    args.gyre_workflow.run_gyre_workflow = False # GYRE workflow disabled

    mock_os_path_exists.return_value = False # Force reanalysis
    mock_scan_mesa_runs.return_value = mock_mesa_run_infos
    mock_get_data_from_history_file.return_value = mock_history_df
    mock_analyze_blue_loop_and_instability.return_value = mock_blue_loop_analysis_output


    analysis_results_sub_dir = "/tmp/analysis_results_nogyre"
    detail_files_output_dir = "/tmp/detail_files_nogyre"
    gyre_input_csv_name = "gyre_input_nogyre.csv"

    summary_df, detail_df, history_dict, gyre_csv_path = perform_mesa_analysis(
        args, analysis_results_sub_dir, detail_files_output_dir, gyre_input_csv_name
    )

    # Verify that the GYRE input CSV was NOT saved
    call_args_list = [call.args[0] for call in mock_to_csv.call_args_list]
    assert not any(gyre_input_csv_name in path for path in call_args_list)

    assert gyre_csv_path == "" # Should be an empty string

    # The other return values should be correct, as in the first test
    assert not summary_df.empty