# test_mesa_plotter.py

import unittest
from unittest.mock import MagicMock, patch, call
import pandas as pd
import os

# Import the functions we want to test
from mesalab.plotting import mesa_plotter

class TestMesaPlotter(unittest.TestCase):
    """
    Unit tests for the mesalab.plotting.mesa_plotter module.

    These tests use mocking to simulate dependencies like file I/O and external
    function calls, ensuring that the logic within each handler function
    is correct.
    """

    def setUp(self):
        """
        Set up a mock 'args' object and mock dataframes for use in tests.
        """
        self.mock_args = MagicMock()
        self.plots_sub_dir = "mock_plots"
        self.analysis_results_sub_dir = "mock_results"
        self.input_dir = "mock_grid"
        self.detail_files_output_dir = "mock_detail_files"
        
        # Mock DataFrame for heatmaps
        self.mock_heatmap_df = pd.DataFrame({
            '0.1': [1, 2, 3],
            '0.2': [4, 5, 6]
        }, index=[0.001, 0.002, 0.003])
        
        # Mock DataFrame for blue loop plots
        self.mock_blue_loop_df = pd.DataFrame({
            'star_mass': [1.0, 1.5, 2.0],
            'log_L': [1.0, 1.5, 2.0],
            'log_Teff': [4.0, 3.8, 3.7],
        })

    @patch('mesalab.plotting.mesa_plotter.os.path.exists')
    @patch('mesalab.plotting.mesa_plotter.pd.read_csv')
    @patch('mesalab.plotting.mesa_plotter.generate_heatmaps_and_time_diff_csv')
    def test_handle_heatmap_generation_success(self, mock_generate, mock_read_csv, mock_exists):
        """
        Test that handle_heatmap_generation works correctly when the CSV file exists
        and the plotting flag is True.
        """
        # Set mock return values
        self.mock_args.plotting_settings.generate_heatmaps = True
        mock_exists.return_value = True
        mock_read_csv.return_value = self.mock_heatmap_df
        
        # Call the function under test
        mesa_plotter.handle_heatmap_generation(
            self.mock_args,
            None,
            self.plots_sub_dir,
            self.analysis_results_sub_dir,
            self.input_dir
        )
        
        # Assertions to check if the mocked functions were called as expected
        mock_exists.assert_called_once_with(os.path.join(self.analysis_results_sub_dir, "crossing_count_grid.csv"))
        mock_read_csv.assert_called_once()
        mock_generate.assert_called_once()
        
        # Get the DataFrame passed to the mock function and verify its structure
        called_df = mock_generate.call_args[1]['cross_data_df']
        self.assertIsInstance(called_df, pd.DataFrame)
        self.assertFalse(called_df.empty)
        self.assertListEqual(called_df.index.tolist(), [0.001, 0.002, 0.003])
        self.assertListEqual(called_df.columns.tolist(), [0.1, 0.2])

    def test_handle_heatmap_generation_not_requested(self):
        """
        Test that handle_heatmap_generation correctly skips execution
        when the plotting flag is False.
        """
        self.mock_args.plotting_settings.generate_heatmaps = False
        
        # We use a patch context manager here to ensure the mocks are only active
        # within this test method's scope.
        with patch('mesalab.plotting.mesa_plotter.os.path.exists') as mock_exists, \
             patch('mesalab.plotting.mesa_plotter.generate_heatmaps_and_time_diff_csv') as mock_generate:
            
            mesa_plotter.handle_heatmap_generation(
                self.mock_args,
                None,
                self.plots_sub_dir,
                self.analysis_results_sub_dir,
                self.input_dir
            )
            
            # Assert that none of the functions were called
            mock_exists.assert_not_called()
            mock_generate.assert_not_called()

    @patch('mesalab.plotting.mesa_plotter.load_and_group_data')
    @patch('mesalab.plotting.mesa_plotter.generate_blue_loop_plots_with_bc')
    def test_handle_blue_loop_bc_plotting_with_data(self, mock_generate, mock_load):
        """
        Test that blue loop plotting works with pre-loaded data.
        """
        self.mock_args.plotting_settings.generate_blue_loop_plots_with_bc = True
        self.mock_args.general_settings.force_reanalysis = False
        
        mesa_plotter.handle_blue_loop_bc_plotting(
            self.mock_args,
            self.mock_blue_loop_df,
            self.plots_sub_dir,
            self.detail_files_output_dir
        )
        
        mock_load.assert_not_called()
        mock_generate.assert_called_once()
        self.assertIs(mock_generate.call_args[1]['combined_df_all_data'], self.mock_blue_loop_df)

    @patch('mesalab.plotting.mesa_plotter.load_and_group_data')
    @patch('mesalab.plotting.mesa_plotter.generate_blue_loop_plots_with_bc')
    def test_handle_blue_loop_bc_plotting_without_data(self, mock_generate, mock_load):
        """
        Test that blue loop plotting loads data correctly when it's not pre-loaded.
        """
        self.mock_args.plotting_settings.generate_blue_loop_plots_with_bc = True
        self.mock_args.general_settings.force_reanalysis = False
        mock_load.return_value = self.mock_blue_loop_df
        
        mesa_plotter.handle_blue_loop_bc_plotting(
            self.mock_args,
            pd.DataFrame(), # Pass an empty DataFrame to simulate no data
            self.plots_sub_dir,
            self.detail_files_output_dir
        )
        
        mock_load.assert_called_once_with(self.detail_files_output_dir)
        mock_generate.assert_called_once()
        
    @patch('mesalab.plotting.mesa_plotter.generate_all_hr_diagrams')
    def test_handle_hr_diagram_generation_success(self, mock_generate):
        """
        Test that HR diagram generation works with valid data when the flag is True.
        """
        self.mock_args.plotting_settings.generate_hr_diagrams = True
        self.mock_args.general_settings.input_dir = "some_model_grid"
        mock_history_data = [self.mock_blue_loop_df]
        
        # The key change in the `mesa_plotter.py` file should resolve the AttributeError
        mesa_plotter.handle_hr_diagram_generation(
            self.mock_args,
            self.plots_sub_dir,
            mock_history_data,
            drop_zams=True
        )
        
        mock_generate.assert_called_once_with(
            all_history_data_flat=mock_history_data,
            model_name="some_model_grid",
            output_dir=self.plots_sub_dir,
            logT_blue_edge=[3.76, 3.83],
            logL_blue_edge=[4.5, 2.4],
            logT_red_edge=[3.65, 3.77],
            logL_red_edge=[4.5, 2.4],
            drop_zams=True
        )
        
    @patch('mesalab.plotting.mesa_plotter.generate_all_hr_diagrams')
    def test_handle_hr_diagram_generation_as_string_none(self, mock_generate):
        """
        Test that HR diagram generation correctly handles the string 'none' to skip.
        """
        self.mock_args.plotting_settings.generate_hr_diagrams = 'none'
        
        mesa_plotter.handle_hr_diagram_generation(
            self.mock_args,
            self.plots_sub_dir,
            [],
            drop_zams=True
        )
        
        mock_generate.assert_not_called()

    @patch('mesalab.plotting.mesa_plotter.generate_all_hr_diagrams')
    def test_handle_hr_diagram_generation_empty_data(self, mock_generate):
        """
        Test that HR diagram generation correctly handles empty data.
        """
        self.mock_args.plotting_settings.generate_hr_diagrams = True
        
        mesa_plotter.handle_hr_diagram_generation(
            self.mock_args,
            self.plots_sub_dir,
            [], # Empty list
            drop_zams=True
        )
        
        mock_generate.assert_not_called()

if __name__ == '__main__':
    unittest.main()
