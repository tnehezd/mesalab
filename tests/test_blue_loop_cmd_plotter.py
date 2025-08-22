import os
import unittest
import pandas as pd
import numpy as np
from unittest.mock import patch, MagicMock, call
from mesalab.bluelooptools import blue_loop_cmd_plotter as bcmd

class TestBlueLoopCmdPlotter(unittest.TestCase):
    
    # Setup - This method runs before each test
    def setUp(self):
        self.dummy_df = pd.DataFrame({
            'log_Teff': [3.78, 3.82, 3.75, np.nan],
            'log_L': [2.8, 3.0, 3.2, 2.9],
            'log_g': [3.5, 3.2, 3.0, 3.1],
            'initial_Z': [0.008, 0.008, 0.008, 0.014],
            'Z': [0.008, 0.008, 0.008, 0.014]
        })

        self.dummy_df_valid = pd.DataFrame({
            'log_Teff': [3.78, 3.82, 3.75],
            'log_L': [2.8, 3.0, 3.2],
            'log_g': [3.5, 3.2, 3.0],
            'initial_Z': [0.008, 0.008, 0.008]
        })

    # --- Test z_to_feh() function ---
    def test_z_to_feh_valid_z(self):
        """Tests Z to Fe/H conversion for a standard Z value."""
        z = 0.0152
        expected_feh = 0.0
        self.assertAlmostEqual(bcmd.z_to_feh(z), expected_feh, places=5)
        
        z = 0.0076
        expected_feh = -0.30103
        self.assertAlmostEqual(bcmd.z_to_feh(z), expected_feh, places=5)

    def test_z_to_feh_non_positive_z(self):
        """Tests Z to Fe/H conversion with non-positive Z values."""
        self.assertTrue(np.isnan(bcmd.z_to_feh(0)))
        self.assertTrue(np.isnan(bcmd.z_to_feh(-0.01)))

    # --- Test _calculate_bc_for_single_point() function ---
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.bc_grid')
    def test_calculate_bc_for_single_point_valid(self, mock_bc_grid):
        """Tests the BC calculation for valid input."""
        mock_bc_grid.interp.side_effect = [[0.1], [0.2], [0.3]]
        
        args = (0, 10**3.78, 3.5, bcmd.z_to_feh(0.008), 0.0)
        result = bcmd._calculate_bc_for_single_point(args)
        
        self.assertEqual(result[0], 0)
        self.assertAlmostEqual(result[1], 0.1)
        self.assertAlmostEqual(result[2], 0.2)
        self.assertAlmostEqual(result[3], 0.3)
        
        calls = mock_bc_grid.interp.call_args_list
        self.assertEqual(calls[0].args[1], ['G'])
        self.assertEqual(calls[1].args[1], ['BP'])
        self.assertEqual(calls[2].args[1], ['RP'])

    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.bc_grid')
    def test_calculate_bc_for_single_point_nan_input(self, mock_bc_grid):
        """Tests the BC calculation with a NaN input."""
        args = (1, np.nan, 3.5, 0.0, 0.0)
        result = bcmd._calculate_bc_for_single_point(args)
        
        self.assertTrue(np.isnan(result[1]))
        self.assertTrue(np.isnan(result[2]))
        self.assertTrue(np.isnan(result[3]))
        mock_bc_grid.interp.assert_not_called()

    # --- Test load_and_group_data() function ---
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.pd.read_csv')
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.os.listdir')
    def test_load_and_group_data_valid(self, mock_listdir, mock_read_csv):
        """Tests loading valid CSV files from a directory."""
        mock_listdir.return_value = ['file1.csv', 'file2.csv']
        
        df1 = pd.DataFrame({'initial_mass': [1.0], 'initial_Z': [0.014], 'log_Teff': [3.7]})
        df2 = pd.DataFrame({'initial_mass': [1.5], 'initial_Z': [0.008], 'log_Teff': [3.8]})
        mock_read_csv.side_effect = [df1, df2]
        
        combined_df = bcmd.load_and_group_data('/dummy/path')
        
        self.assertEqual(len(combined_df), 2)
        self.assertIn('initial_Z', combined_df.columns)
        self.assertIn('initial_mass', combined_df.columns)
        self.assertEqual(mock_read_csv.call_count, 2)
    
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.pd.read_csv')
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.os.listdir')
    def test_load_and_group_data_with_z_column(self, mock_listdir, mock_read_csv):
        """Tests loading a file with a 'Z' column instead of 'initial_Z'."""
        mock_listdir.return_value = ['file1.csv']
        df = pd.DataFrame({'initial_mass': [1.0], 'Z': [0.014], 'log_Teff': [3.7]})
        mock_read_csv.return_value = df
        
        combined_df = bcmd.load_and_group_data('/dummy/path')
        
        self.assertIn('initial_Z', combined_df.columns)
        self.assertNotIn('Z', combined_df.columns)

    def test_load_and_group_data_empty_dir(self):
        """Tests behavior when the directory is empty."""
        with patch('mesalab.bluelooptools.blue_loop_cmd_plotter.os.listdir', return_value=[]):
            combined_df = bcmd.load_and_group_data('/dummy/path')
            self.assertTrue(combined_df.empty)

    # --- Test generate_blue_loop_plots_with_bc() function ---
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.plt')
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.os.makedirs')
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.pkg_resources')
    @patch('mesalab.bluelooptools.blue_loop_cmd_plotter.bc_grid')
    def test_generate_blue_loop_plots_with_bc(self, mock_bc_grid, mock_pkg_resources, mock_makedirs, mock_plt):
        """Tests the main plotting function without creating real plots."""
        # Create separate mock objects for each figure and axes, each with a mocked savefig method
        mock_fig1 = MagicMock()
        mock_ax1 = MagicMock()
        mock_fig2 = MagicMock()
        mock_ax2 = MagicMock()
        mock_fig3 = MagicMock()
        mock_ax3 = MagicMock()
        
        # Set the side_effect for plt.subplots to return different tuples for each call
        mock_plt.subplots.side_effect = [(mock_fig1, mock_ax1), (mock_fig2, mock_ax2), (mock_fig3, mock_ax3)]
        
        # Provide a long enough side_effect to cover all interp calls (3 rows * 3 calls/row = 9 total)
        mock_bc_grid.interp.side_effect = [[0.1], [0.2], [0.3], [0.1], [0.2], [0.3], [0.1], [0.2], [0.3]]
        
        # Use the 'valid' DataFrame that has no NaNs
        bcmd.generate_blue_loop_plots_with_bc(self.dummy_df_valid, 'dummy_output_dir')
        
        # Assertions
        mock_makedirs.assert_called_once_with('dummy_output_dir')
        
        # Assert that the `savefig` method was called on each mock figure object, including the 'dpi' argument.
        mock_fig1.savefig.assert_called_once_with(os.path.join('dummy_output_dir', 'HRD_all_blue_loop_data.png'), dpi=200)
        mock_fig2.savefig.assert_called_once_with(os.path.join('dummy_output_dir', 'CMD_Gaia_all_blue_loop_data.png'), dpi=200)
        mock_fig3.savefig.assert_called_once_with(os.path.join('dummy_output_dir', 'LogL_LogG_all_blue_loop_data.png'), dpi=200)
        
        # We can also assert that plt.savefig was not called, because the code uses fig.savefig
        mock_plt.savefig.assert_not_called()

        self.assertEqual(mock_plt.close.call_count, 3)

    def test_generate_plots_empty_df(self):
        """Tests that the function handles an empty DataFrame gracefully."""
        with patch('mesalab.bluelooptools.blue_loop_cmd_plotter.plt') as mock_plt:
            with patch('mesalab.bluelooptools.blue_loop_cmd_plotter.os.makedirs') as mock_makedirs:
                # Provide an empty DataFrame
                bcmd.generate_blue_loop_plots_with_bc(pd.DataFrame(), 'dummy_output_dir')
                
                # Assert that no plotting or saving happened
                mock_plt.savefig.assert_not_called()
                mock_plt.close.assert_not_called()
                mock_makedirs.assert_called_once_with('dummy_output_dir')
