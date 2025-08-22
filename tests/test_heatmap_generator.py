# tests/test_heatmap_generator.py

import pytest
import pandas as pd
import numpy as np
import os
import sys

# Add the parent directory to the path so we can import the module
# This is a common practice for running tests that are in a 'tests' directory
# and need to import from the project root.
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

# Import the function we want to test
from mesalab.plotting.heatmap_generator import generate_heatmaps_and_time_diff_csv

@pytest.fixture
def setup_dirs_and_data(tmp_path):
    """
    Sets up temporary directories and dummy data for the tests.
    
    Pytest's `tmp_path` fixture creates a temporary directory for each test
    and handles its cleanup, which is perfect for testing file I/O.
    
    Args:
        tmp_path: Pytest fixture for temporary directories.
        
    Returns:
        A tuple containing the paths to the dummy directories and data.
    """
    # Create output directories
    plots_dir = tmp_path / "plots"
    results_dir = tmp_path / "analysis_results"
    plots_dir.mkdir()
    results_dir.mkdir()

    # Create a dummy cross_data_df with some NaN values
    cross_data = {
        0.8: [0, 1, 2],
        0.9: [1, np.nan, 3],
        1.0: [2, 3, 5]
    }
    cross_df = pd.DataFrame(cross_data, index=[0.005, 0.008, 0.012])

    # Create a dummy summary CSV with time data
    summary_data = {
        'initial_mass': [0.8, 0.8, 0.8, 0.9, 0.9, 0.9, 1.0, 1.0, 1.0],
        'initial_Z': [0.005, 0.008, 0.012, 0.005, 0.008, 0.012, 0.005, 0.008, 0.012],
        'blue_loop_crossing_count': [0, 1, 2, 1, 0, 3, 2, 3, 5],
        'blue_loop_start_age': [np.nan, 10.0, 15.0, 12.0, np.nan, 18.0, 20.0, 25.0, 30.0],
        'blue_loop_end_age': [np.nan, 12.0, 17.0, 15.0, np.nan, 21.0, 22.0, 28.0, 33.0],
        'instability_start_age': [np.nan, 10.5, 15.5, 12.5, np.nan, 18.5, 20.5, 25.5, 30.5],
        'instability_end_age': [np.nan, 11.5, 16.5, 13.5, np.nan, 19.5, 21.5, 26.5, 31.5],
        'calculated_blue_loop_duration': [np.nan, 2.0, 2.0, 3.0, np.nan, 3.0, 2.0, 3.0, 3.0],
        'calculated_instability_duration': [np.nan, 1.0, 1.0, 1.0, np.nan, 1.0, 1.0, 1.0, 1.0]
    }
    summary_df = pd.DataFrame(summary_data)
    summary_csv_path = results_dir / "summary.csv"
    summary_df.to_csv(summary_csv_path, index=False)

    return (
        cross_df,
        str(summary_csv_path),
        [0.005, 0.008, 0.012],
        [0.8, 0.9, 1.0],
        str(plots_dir),
        str(results_dir)
    )

def test_heatmap_generation_success(setup_dirs_and_data):
    """
    Tests if the heatmap PNG file is successfully created.
    """
    cross_df, summary_path, unique_zs, unique_masses, plots_dir, results_dir = setup_dirs_and_data
    
    # Run the function with a mock of the original data and paths
    generate_heatmaps_and_time_diff_csv(
        cross_df, summary_path, unique_zs, unique_masses, plots_dir, results_dir
    )
    
    # Check if the heatmap file was created
    expected_heatmap_path = os.path.join(plots_dir, "mesa_grid_blue_loop_heatmap.png")
    assert os.path.exists(expected_heatmap_path)
    assert os.path.getsize(expected_heatmap_path) > 0 # Ensure the file is not empty

def test_time_diff_csv_generation_success(setup_dirs_and_data):
    """
    Tests if the time differences CSV file is created correctly
    when the blue loop analysis is enabled.
    """
    cross_df, summary_path, unique_zs, unique_masses, plots_dir, results_dir = setup_dirs_and_data

    # Run the function with analyze_blue_loop=True
    generate_heatmaps_and_time_diff_csv(
        cross_df, summary_path, unique_zs, unique_masses, plots_dir, results_dir, analyze_blue_loop=True
    )
    
    # Check if the time differences CSV was created
    expected_csv_path = os.path.join(results_dir, "mesa_grid_time_differences.csv")
    assert os.path.exists(expected_csv_path)
    
    # Read the generated CSV and check its content
    generated_df = pd.read_csv(expected_csv_path)
    assert 'initial_mass' in generated_df.columns
    assert 'calculated_blue_loop_duration' in generated_df.columns
    assert 'calculated_instability_duration' in generated_df.columns

def test_empty_dataframe_handling(setup_dirs_and_data, caplog):
    """
    Tests that the function handles an empty input DataFrame gracefully
    and logs a warning instead of crashing.
    """
    cross_df, summary_path, unique_zs, unique_masses, plots_dir, results_dir = setup_dirs_and_data
    
    # Create an empty DataFrame
    empty_df = pd.DataFrame()
    
    # Use caplog to capture log messages
    with caplog.at_level('WARNING'):
        generate_heatmaps_and_time_diff_csv(
            empty_df, summary_path, unique_zs, unique_masses, plots_dir, results_dir
        )
    
    # Check if a warning was logged and no file was created
    assert "cross_data_df is empty. Cannot generate heatmaps." in caplog.text
    assert not os.path.exists(os.path.join(plots_dir, "mesa_grid_blue_loop_heatmap.png"))

def test_no_time_diff_csv_when_disabled(setup_dirs_and_data):
    """
    Tests that the time differences CSV is NOT created when analyze_blue_loop is False.
    """
    cross_df, summary_path, unique_zs, unique_masses, plots_dir, results_dir = setup_dirs_and_data
    
    # Ensure a summary CSV exists
    assert os.path.exists(summary_path)

    # Run the function with analyze_blue_loop=False
    generate_heatmaps_and_time_diff_csv(
        cross_df, summary_path, unique_zs, unique_masses, plots_dir, results_dir, analyze_blue_loop=False
    )
    
    # Check that the time differences CSV was NOT created
    expected_csv_path = os.path.join(results_dir, "mesa_grid_time_differences.csv")
    assert not os.path.exists(expected_csv_path)
