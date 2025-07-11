import pytest
import os
import pandas as pd
import numpy as np
import yaml
import logging
from unittest.mock import patch, MagicMock, mock_open, call # Import 'call' for assert_any_call
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
    """Mock data returned by the scan_mesa_runs function.
    ### FIX: Ensure unique (mass, Z) pairs for each entry
    """
    return [
        {'mass': 1.0, 'z': 0.02, 'run_dir_path': '/dummy/path/M1.0Z0.02', 'history_file_path': '/dummy/path/M1.0Z0.02/history.data'},
        {'mass': 1.2, 'z': 0.02, 'run_dir_path': '/dummy/path/M1.2Z0.02', 'history_file_path': '/dummy/path/M1.2Z0.02/history.data'},
        {'mass': 1.0, 'z': 0.01, 'run_dir_path': '/dummy/path/M1.0Z0.01', 'history_file_path': '/dummy/path/M1.0Z0.01/history.data'},
        ### FIX: Add a fourth unique run for a different Z if you intend to have 3 unique Zs or more
        # For this test, 3 unique (mass,Z) pairs is fine, leading to 3 history entries.
        # If you meant to have 3 different Zs, you'd add:
        # {'mass': 1.0, 'z': 0.03, 'run_dir_path': '/dummy/path/M1.0Z0.03', 'history_file_path': '/dummy/path/M1.0Z0.03/history.data'},
    ]

@pytest.fixture
def mock_history_df():
    """Dummy DataFrame to simulate the content of a MESA history file."""
    # This specific fixture is only used as a default return_value,
    # for specific tests, side_effect will be used to create distinct DFs.
    return pd.DataFrame({
        'model_number': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        'star_age': np.linspace(0.1, 1.0, 10),
        'log_Teff': np.linspace(3.7, 4.0, 10),
        'log_L': np.linspace(1.0, 2.0, 10),
        'log_g': np.linspace(4.0, 3.5, 10),
        'log_R': np.linspace(0.0, 1.0, 10),
        'profile_number': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
        'log_dt': np.linspace(0.1, 0.2, 10),
        'initial_mass': [1.0] * 10,  # Added for history_dict keying
        'initial_Z': [0.02] * 10    # Added for history_dict keying
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
    args = mock_args_base
    args.general_settings.force_reanalysis = True
    args.blue_loop_analysis.analyze_blue_loop = True
    args.blue_loop_analysis.blue_loop_output_type = "all" # Changed output type
    args.gyre_workflow.run_gyre_workflow = True # Ensure GYRE workflow is enabled for this test

    # Configure the behavior of the mocked dependencies
    mock_os_path_exists.return_value = False # Simulate that CSVs do not exist
    mock_scan_mesa_runs.return_value = mock_mesa_run_infos

    ### FIX 1: Generate history DFs based on the actual mock_mesa_run_infos structure
    # This ensures that each call to get_data_from_history_file gets a distinct DF
    # with the correct initial_mass and initial_Z for proper history_dict population.
    history_dfs_for_side_effect = []
    for run_info in mock_mesa_run_infos:
        df = mock_history_df.copy()
        df['initial_mass'] = run_info['mass']
        df['initial_Z'] = run_info['z']
        history_dfs_for_side_effect.append(df)
    mock_get_data_from_history_file.side_effect = history_dfs_for_side_effect

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
    # The `mesa_analyzer.py` code explicitly creates both.
    mock_os_makedirs.assert_any_call(analysis_results_sub_dir, exist_ok=True)
    mock_os_makedirs.assert_any_call(detail_files_output_dir, exist_ok=True)


    # Verify that summary, cross-grid, and GYRE input CSVs were saved
    # and potentially for each blue_loop_detail_df if blue_loop_output_type is 'all'.
    # There are 3 runs (all with unique Z, M pairs now), and for each, a detail CSV is saved.
    # So total: 1 (summary) + 1 (cross-grid) + 1 (gyre) + 3 (detail, one for each unique Z) = 6 calls.
    ### FIX 2: Correct expected call count for mock_to_csv
    expected_unique_zs = len(set(run_info['z'] for run_info in mock_mesa_run_infos))
    # Number of calls to to_csv should be:
    # 1 for summary_results.csv
    # 1 for crossing_count_grid.csv
    # 1 for gyre_input.csv (because args.gyre_workflow.run_gyre_workflow = True)
    # expected_unique_zs for detail_z*.csv files (because blue_loop_output_type is 'all')
    expected_to_csv_calls = 1 + 1 + 1 + expected_unique_zs
    assert mock_to_csv.call_count == expected_to_csv_calls

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

    # Given the code groups history data by Z, assert that the number of entries
    # in history_dict matches the number of unique Z values in the mock runs.
    # With mock_mesa_run_infos = [{'mass': 1.0, 'z': 0.02}, {'mass': 1.2, 'z': 0.02}, {'mass': 1.0, 'z': 0.01}]
    # Unique Zs are {0.02, 0.01}, so 2 unique Zs.
    expected_unique_z_count = len(set(run_info['z'] for run_info in mock_mesa_run_infos))
    assert len(history_dict) == expected_unique_z_count

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

    analysis_results_sub_dir = "/tmp/analysis_results"
    detail_files_output_dir = "/tmp/detail_files"
    gyre_input_csv_name = "gyre_input.csv"

    # Simulate that all CSVs already exist and return correct values
    # For blue loop analysis ON and gyre workflow ON
    mock_os_path_exists.side_effect = lambda x: x in [
        os.path.join(analysis_results_sub_dir, "summary_results.csv"),
        os.path.join(analysis_results_sub_dir, "crossing_count_grid.csv"),
        os.path.join(analysis_results_sub_dir, gyre_input_csv_name), # Explicitly check for gyre_input_csv_name
        detail_files_output_dir, # Directory exists
        os.path.join(detail_files_output_dir, "detail_z0.0200.csv"), # Example detail file
        os.path.join(detail_files_output_dir, "detail_z0.0100.csv")  # Example detail file
    ]
    # Provide list of detail files for os.listdir to simulate
    mock_os_listdir.return_value = ["detail_z0.0200.csv", "detail_z0.0100.csv"]

    # Mock `pd.read_csv` to return appropriate DataFrames when the function tries to load them.
    # Ensure mock_summary_df has entries that pass the blue loop filter if analyze_blue_loop is True
    mock_summary_df = pd.DataFrame({
        'initial_mass': [1.0, 1.2, 1.0],
        'initial_Z': [0.02, 0.02, 0.01],
        'blue_loop_crossing_count': [1, 0, 1], # 1.0Z0.02 and 1.0Z0.01 have blue loops, 1.2Z0.02 doesn't
        'blue_loop_duration_yr': [1000.0, np.nan, 800.0],
        'run_dir_path': ['/dummy/path/M1.0Z0.02', '/dummy/path/M1.2Z0.02', '/dummy/path/M1.0Z0.01'],
        'first_model_number': [10, 50, 20], # Added for GYRE input
        'last_model_number': [20, 60, 30] # Added for GYRE input
    }).set_index(['initial_Z', 'initial_mass'])

    # Mock detail DFs for each Z
    mock_detail_df_z002 = pd.DataFrame({
        'initial_mass': [1.0] * 3, 'initial_Z': [0.02] * 3,
        'star_age': [0.1, 0.2, 0.3], 'model_number': [1, 2, 3],
        'log_Teff': [3.7, 3.8, 3.9], 'log_L': [1.0, 1.1, 1.2]
    })
    mock_detail_df_z001 = pd.DataFrame({
        'initial_mass': [1.0] * 2, 'initial_Z': [0.01] * 2,
        'star_age': [0.5, 0.6], 'model_number': [5, 6],
        'log_Teff': [3.6, 3.7], 'log_L': [0.9, 1.0]
    })
    # Dummy GYRE input DF
    mock_gyre_input_df = pd.DataFrame({
        'initial_mass': [1.0], 'initial_Z': [0.02],
        'mesa_run_directory': ['/dummy/path/M1.0Z0.02'],
        'min_model_number': [10], 'max_model_number': [20]
    })

    def mock_read_csv_side_effect(path, **kwargs):
        if "summary_results.csv" in path:
            return mock_summary_df
        elif "detail_z0.0200.csv" in path:
            return mock_detail_df_z002
        elif "detail_z0.0100.csv" in path:
            return mock_detail_df_z001
        elif gyre_input_csv_name in path:
            return mock_gyre_input_df # Return the dummy GYRE DF
        return pd.DataFrame() # Return empty DF otherwise


    mock_read_csv.side_effect = mock_read_csv_side_effect


    summary_df, detail_df, history_dict, gyre_csv_path = perform_mesa_analysis(
        args, analysis_results_sub_dir, detail_files_output_dir, gyre_input_csv_name
    )

    # Verify that the full analysis functions were NOT called
    mock_scan_mesa_runs.assert_not_called()
    mock_get_data_from_history_file.assert_not_called()
    mock_analyze_blue_loop_and_instability.assert_not_called()
    mock_yaml_dump.assert_not_called()
    mock_os_remove.assert_not_called()
    
    # These directories *should* be ensured by os.makedirs(..., exist_ok=True)
    # when loading, so we expect them to be called if the paths exist.
    mock_os_makedirs.assert_any_call(analysis_results_sub_dir, exist_ok=True)
    mock_os_makedirs.assert_any_call(detail_files_output_dir, exist_ok=True)

    # Verify that CSVs were read
    mock_read_csv.assert_any_call(os.path.join(analysis_results_sub_dir, "summary_results.csv"), index_col=['initial_Z', 'initial_mass'])
    mock_read_csv.assert_any_call(os.path.join(detail_files_output_dir, "detail_z0.0200.csv")) # Check detail CSV read
    mock_read_csv.assert_any_call(os.path.join(detail_files_output_dir, "detail_z0.0100.csv")) # Check other detail CSV read
    mock_read_csv.assert_any_call(os.path.join(analysis_results_sub_dir, gyre_input_csv_name), index_col=['initial_Z', 'initial_mass']) # Verify GYRE CSV is read, it's checked by the exists condition now

    # Verify that no new saving occurred (unless internal logic dictates otherwise)
    mock_to_csv.assert_not_called() # Assuming no new saves happen in this branch

    # Verify the returned DataFrames
    assert not summary_df.empty
    # The summary_df should be filtered (only blue_loop_crossing_count > 0)
    # From mock_summary_df, (0.02, 1.0) and (0.01, 1.0) should remain.
    assert len(summary_df) == 2
    assert (0.02, 1.0) in summary_df.index
    assert (0.01, 1.0) in summary_df.index
    assert (0.02, 1.2) not in summary_df.index # This one has 0 crossings

    assert not detail_df.empty
    assert 'log_Teff' in detail_df.columns # Columns from the loaded mock detail DF
    assert len(detail_df) == len(mock_detail_df_z002) + len(mock_detail_df_z001) # Combined length of loaded detail DFs

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
    mock_args_base, mock_mesa_run_infos, mock_history_df # Keep mock_history_df fixture in signature
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

    ### FIX 3: Use side_effect for mock_get_data_from_history_file to provide unique DFs
    # This ensures history_dict gets all 3 entries based on different (mass, Z)
    history_dfs_for_side_effect = []
    for run_info in mock_mesa_run_infos:
        df = mock_history_df.copy()
        df['initial_mass'] = run_info['mass']
        df['initial_Z'] = run_info['z']
        history_dfs_for_side_effect.append(df)
    mock_get_data_from_history_file.side_effect = history_dfs_for_side_effect

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

    expected_unique_z_count = len(set(run_info['z'] for run_info in mock_mesa_run_infos))
    assert len(history_dict) == expected_unique_z_count # History data should be grouped by unique Z values

    assert gyre_csv_path == os.path.join(analysis_results_sub_dir, gyre_input_csv_name) # Should return the expected path

    # The code creates these directories regardless of blue loop analysis status
    mock_os_makedirs.assert_any_call(analysis_results_sub_dir, exist_ok=True)
    mock_os_makedirs.assert_any_call(detail_files_output_dir, exist_ok=True) # It seems the code always creates both directories


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

    ### FIX 4: Use side_effect for mock_get_data_from_history_file here too
    # This ensures history_dict gets all entries based on different (mass, Z)
    history_dfs_for_side_effect = []
    for run_info in mock_mesa_run_infos:
        df = mock_history_df.copy()
        df['initial_mass'] = run_info['mass']
        df['initial_Z'] = run_info['z']
        history_dfs_for_side_effect.append(df)
    mock_get_data_from_history_file.side_effect = history_dfs_for_side_effect

    mock_analyze_blue_loop_and_instability.return_value = mock_blue_loop_analysis_output


    analysis_results_sub_dir = "/tmp/analysis_results_nogyre"
    detail_files_output_dir = "/tmp/detail_files_nogyre"
    gyre_input_csv_name = "gyre_input_nogyre.csv"

    summary_df, detail_df, history_dict, gyre_csv_path = perform_mesa_analysis(
        args, analysis_results_sub_dir, detail_files_output_dir, gyre_input_csv_name
    )

    # Verify that the GYRE input CSV was NOT saved
    call_args_list = [call.args[0] for call in mock_to_csv.call_args_list]
    ### FIX 5: This assertion is now correct IF your mesa_analyzer.py code properly guards GYRE CSV saving.
    # It seems your mesa_analyzer.py code was indeed fixed, so this should pass now.
    assert not any(gyre_input_csv_name in path for path in call_args_list)

    assert gyre_csv_path == "" # Should be an empty string, as no GYRE workflow was run

    # The other return values should be correct, as in the first test
    assert not summary_df.empty
    
    expected_unique_z_count = len(set(run_info['z'] for run_info in mock_mesa_run_infos))
    assert len(history_dict) == expected_unique_z_count # Ensure history_dict is complete

    # The code creates these directories regardless of GYRE workflow status
    mock_os_makedirs.assert_any_call(analysis_results_sub_dir, exist_ok=True)
    mock_os_makedirs.assert_any_call(detail_files_output_dir, exist_ok=True)