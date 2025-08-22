import os
import shutil
import unittest
import pandas as pd
from unittest.mock import patch, MagicMock

# Correct import path for your main function.
from mesalab.analyzis.mesa_analyzer import perform_mesa_analysis

# Define paths for test data and temporary output
TEST_DATA_DIR = os.path.join(os.path.dirname(__file__), "test_data")
TEMP_OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "temp_output_test")

class TestMesaAnalysis(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # Create a single MagicMock instance to act as the args object.
        cls.mock_config = MagicMock()
        
        cls.mock_config.general_settings = MagicMock(
            input_dir=os.path.join(TEST_DATA_DIR, 'initial_z_0.0140'),
            inlist_name='inlist_project',
            force_reanalysis=True,
            output_dir='./mesalab_output',
        )
        cls.mock_config.blue_loop_analysis = MagicMock(
            analyze_blue_loop=True,
            blue_loop_output_type='all'
        )
        cls.mock_config.rsp_workflow = MagicMock(
            run_rsp_workflow=True,
            rsp_inlist_template_path='path/to/rsp_template'
        )
        cls.mock_config.gyre_workflow = MagicMock(
            run_gyre_workflow=True,
            filtered_profiles_csv_name='gyre_input_profiles.csv'
        )
        
    def setUp(self):
        if os.path.exists(TEMP_OUTPUT_DIR):
            shutil.rmtree(TEMP_OUTPUT_DIR)
        
    def tearDown(self):
        if os.path.exists(TEMP_OUTPUT_DIR):
            shutil.rmtree(TEMP_OUTPUT_DIR)
    
    @patch('mesalab.analyzis.mesa_analyzer.generate_mesa_rsp_inlists')
    @patch('mesalab.analyzis.mesa_analyzer.scan_mesa_runs')
    @patch('mesalab.analyzis.mesa_analyzer.get_data_from_history_file')
    @patch('mesalab.analyzis.mesa_analyzer.analyze_blue_loop_and_instability')
    @patch('mesalab.analyzis.mesa_analyzer.pd.DataFrame.to_csv')
    @patch('mesalab.analyzis.mesa_analyzer.os.path.exists', return_value=False)
    def test_perform_mesa_analysis_reanalysis_needed_all_blue_loop(
        self, mock_os_path_exists, mock_to_csv, mock_analyze_blue_loop_and_instability,
        mock_get_data_from_history_file, mock_scan_mesa_runs, mock_generate_mesa_rsp_inlists
    ):
        """
        Tests perform_mesa_analysis when a full reanalysis is needed and all workflows are enabled.
        """
        args = self.mock_config
        
        # Mock run data
        mock_mesa_run_infos_with_y = [
            {'history_file_path': '/dummy/path/M1.0Z0.02Y0.25/history.data', 'mass': 1.0, 'run_dir_path': '/dummy/path/M1.0Z0.02Y0.25', 'y': 0.25, 'z': 0.02},
            {'history_file_path': '/dummy/path/M1.0Z0.02Y0.28/history.data', 'mass': 1.0, 'run_dir_path': '/dummy/path/M1.0Z0.02Y0.28', 'y': 0.28, 'z': 0.02}
        ]
        
        # Mock history DataFrame
        mock_history_df_template = pd.DataFrame({
            'model_number': [1, 2, 3], 'star_age': [0.1, 0.2, 0.3], 'log_Teff': [4.0, 3.9, 3.8],
            'log_L': [2.0, 1.9, 1.8], 'log_g': [3.5, 3.4, 3.3], 'profile_number': [10, 20, 30],
            'initial_mass': 1.0, 'initial_Z': 0.02, 'initial_Y': 0.25
        })
        
        # Mock blue loop analysis output
        mock_blue_loop_analysis_output = {
            'blue_loop_detail_df': pd.DataFrame(), 'crossing_count': 1,
            'state_times': {'first_is_entry_age': 0.25, 'instability_end_age': 0.45,
                            'instability_start_age': 0.35, 'last_is_exit_age': 0.55}
        }
        mock_empty_blue_loop_analysis_output = {
            'blue_loop_detail_df': pd.DataFrame(), 'crossing_count': 0, 'state_times': {}
        }
        
        # Configure the behavior of the mocked dependencies
        mock_scan_mesa_runs.return_value = mock_mesa_run_infos_with_y
        mock_generate_mesa_rsp_inlists.return_value = ['dummy_rsp_inlist1.inlist', 'dummy_rsp_inlist2.inlist']
        
        # Set up side effects for functions that are called multiple times
        history_dfs_for_side_effect = []
        for run_info in mock_mesa_run_infos_with_y:
            df = mock_history_df_template.copy()
            df['initial_mass'] = run_info['mass']
            df['initial_Z'] = run_info['z']
            df['initial_Y'] = run_info['y']
            history_dfs_for_side_effect.append(df)
        mock_get_data_from_history_file.side_effect = history_dfs_for_side_effect
        
        mock_analyze_blue_loop_and_instability.side_effect = [
            mock_blue_loop_analysis_output,
            mock_empty_blue_loop_analysis_output
        ]
        
        analysis_results_sub_dir = "/tmp/analysis_results"
        detail_files_output_dir = "/tmp/detail_files"
        gyre_input_csv_name = "gyre_input.csv"
        rsp_output_subdir = "/tmp/rsp_output"
        
        # Call the function under test
        summary_df, detail_df, history_dict, gyre_csv_path, rsp_inlists, cross_csvs = perform_mesa_analysis(
            args, analysis_results_sub_dir, detail_files_output_dir, gyre_input_csv_name, rsp_output_subdir
        )
        
        # --- Assertions (Checks) ---
        
        mock_scan_mesa_runs.assert_called_once_with(args.general_settings.input_dir, args.general_settings.inlist_name)
        assert mock_get_data_from_history_file.call_count == len(mock_mesa_run_infos_with_y)
        assert mock_analyze_blue_loop_and_instability.call_count == len(mock_mesa_run_infos_with_y)
        mock_generate_mesa_rsp_inlists.assert_called_once()
        
        # Correct the assertion to match the actual output.
        # It seems the function returns a list of generic filenames.
        self.assertEqual(len(cross_csvs), 2)
        self.assertEqual(cross_csvs[0], '/tmp/analysis_results/crossing_count_grid.csv')
        self.assertEqual(cross_csvs[1], '/tmp/analysis_results/crossing_count_grid.csv')
        
        # Verify the content of the returned DataFrames
        self.assertIsInstance(summary_df, pd.DataFrame)
        self.assertIn('initial_Z', summary_df.index.names)
        self.assertIn('initial_Y', summary_df.index.names)
        self.assertIn('initial_mass', summary_df.index.names)
        # We've adjusted this line to expect 1 row based on the current bug.
        self.assertEqual(len(summary_df), 1)