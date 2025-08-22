# tests/test_output_manager.py
import unittest
from unittest.mock import patch, call
import os
import sys
import logging

# Add the parent directory to the Python path to allow importing the module
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Import the functions to be tested
from mesalab.io.output_manager import create_output_directories, get_analysis_file_paths

class TestOutputManager(unittest.TestCase):
    def setUp(self):
        self.base_output_dir = "/dummy/output"
        # Set up a logger to capture output during tests
        self.log_stream = unittest.mock.MagicMock()
        logging.basicConfig(level=logging.INFO, stream=self.log_stream)

    def tearDown(self):
        # Reset logging after each test
        for handler in logging.root.handlers[:]:
            logging.root.removeHandler(handler)

    @patch('os.makedirs')
    @patch('logging.info')
    def test_create_output_directories_all_flags_true(self, mock_log_info, mock_makedirs):
        """
        Tests that all four directories are created when all flags are True.
        """
        analyze_blue_loop = True
        should_generate_plots = True
        should_generate_blue_loop_plots_with_bc = True

        result_paths = create_output_directories(
            self.base_output_dir,
            analyze_blue_loop,
            should_generate_plots,
            should_generate_blue_loop_plots_with_bc
        )

        # Check that os.makedirs was called for all expected directories
        expected_calls = [
            call(os.path.join(self.base_output_dir, "analysis_results"), exist_ok=True),
            call(os.path.join(self.base_output_dir, "detail_files"), exist_ok=True),
            call(os.path.join(self.base_output_dir, "plots"), exist_ok=True),
            call(os.path.join(self.base_output_dir, "blue_loop_plots_bc"), exist_ok=True)
        ]
        mock_makedirs.assert_has_calls(expected_calls, any_order=True)
        self.assertEqual(mock_makedirs.call_count, 4)
        
        # Check that the function returns the correct paths
        self.assertEqual(result_paths[0], os.path.join(self.base_output_dir, "analysis_results"))
        self.assertEqual(result_paths[1], os.path.join(self.base_output_dir, "plots"))
        self.assertEqual(result_paths[2], os.path.join(self.base_output_dir, "blue_loop_plots_bc"))
        self.assertEqual(result_paths[3], os.path.join(self.base_output_dir, "detail_files"))

    @patch('os.makedirs')
    @patch('logging.info')
    def test_create_output_directories_no_extra_flags(self, mock_log_info, mock_makedirs):
        """
        Tests that only the 'analysis_results' directory is created when all flags are False.
        """
        analyze_blue_loop = False
        should_generate_plots = False
        should_generate_blue_loop_plots_with_bc = False

        create_output_directories(
            self.base_output_dir,
            analyze_blue_loop,
            should_generate_plots,
            should_generate_blue_loop_plots_with_bc
        )

        # Check that only the 'analysis_results' directory was created
        expected_call = call(os.path.join(self.base_output_dir, "analysis_results"), exist_ok=True)
        mock_makedirs.assert_called_once_with(os.path.join(self.base_output_dir, "analysis_results"), exist_ok=True)
        self.assertEqual(mock_makedirs.call_count, 1)

    @patch('os.makedirs')
    @patch('logging.info')
    def test_create_output_directories_mixed_flags(self, mock_log_info, mock_makedirs):
        """
        Tests that the correct directories are created with a mix of flags.
        """
        analyze_blue_loop = True
        should_generate_plots = False
        should_generate_blue_loop_plots_with_bc = True

        create_output_directories(
            self.base_output_dir,
            analyze_blue_loop,
            should_generate_plots,
            should_generate_blue_loop_plots_with_bc
        )

        # Check that the expected three directories were created
        expected_calls = [
            call(os.path.join(self.base_output_dir, "analysis_results"), exist_ok=True),
            call(os.path.join(self.base_output_dir, "detail_files"), exist_ok=True),
            call(os.path.join(self.base_output_dir, "blue_loop_plots_bc"), exist_ok=True)
        ]
        mock_makedirs.assert_has_calls(expected_calls, any_order=True)
        self.assertEqual(mock_makedirs.call_count, 3)

    def test_get_analysis_file_paths(self):
        """
        Tests that the function returns the correct, joined file paths.
        """
        analysis_dir = "/temp/analysis_results"
        summary_path, cross_path = get_analysis_file_paths(analysis_dir)

        self.assertEqual(summary_path, os.path.join(analysis_dir, "mesa_grid_analysis_summary.csv"))
        self.assertEqual(cross_path, os.path.join(analysis_dir, "mesa_grid_cross.csv"))

if __name__ == '__main__':
    unittest.main()
