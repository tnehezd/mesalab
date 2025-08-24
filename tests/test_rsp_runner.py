import unittest
from unittest.mock import patch, MagicMock
import os
import shutil
import subprocess
import tempfile
import time
from addict import Dict

# Use the correct import path for the mesalab.rsptools package.
from mesalab.rsptools.rsp_runner import run_mesa_rsp_single, run_mesa_rsp_workflow

class TestRSPModules(unittest.TestCase):
    """
    Test suite for the rsptools module, which handles MESA RSP runs.
    """

    def setUp(self):
        """
        Set up a temporary directory and mock objects for testing.
        """
        # Create a temporary directory that will be automatically cleaned up.
        self.test_dir = tempfile.mkdtemp()
        self.inlist_dir = os.path.join(self.test_dir, 'inlists')
        self.mesa_binary_dir = os.path.join(self.test_dir, 'mesa_binary')
        self.output_dir = os.path.join(self.test_dir, 'output')

        # Create necessary subdirectories.
        os.makedirs(self.inlist_dir)
        os.makedirs(self.mesa_binary_dir)
        os.makedirs(self.output_dir)

        # Create a mock MESA executable file.
        self.mesa_exe_path = os.path.join(self.mesa_binary_dir, 'star')
        with open(self.mesa_exe_path, 'w') as f:
            f.write("#!/bin/bash\necho 'Mock MESA executable'")
        os.chmod(self.mesa_exe_path, 0o755)  # Make it executable.

        # Create mock inlist files for testing.
        self.inlist_path1 = os.path.join(self.inlist_dir, 'inlist_rsp_run1')
        self.inlist_path2 = os.path.join(self.inlist_dir, 'inlist_rsp_run2')
        with open(self.inlist_path1, 'w') as f:
            f.write("&inlist_rsp /\n")
        with open(self.inlist_path2, 'w') as f:
            f.write("&inlist_rsp /\n")
        self.inlist_paths = [self.inlist_path1, self.inlist_path2]

        # Create a mock config object using Dict for easy attribute access.
        self.mock_config = Dict({
            'general_settings': {
                'mesa_binary_dir': self.mesa_binary_dir,
            },
            'rsp_workflow': {
                'enable_rsp_parallel': False,  # Default setting for most tests.
                'max_concurrent_rsp_runs': 1,
                'num_rsp_threads': 1
            }
        })

    def tearDown(self):
        """
        Remove the temporary directory and all its contents after each test.
        """
        shutil.rmtree(self.test_dir)

    # --- `run_mesa_rsp_single` tests ---
    @patch('mesalab.rsptools.rsp_runner.subprocess.run')
    def test_run_single_success(self, mock_run):
        """
        Test that `run_mesa_rsp_single` returns a successful status on a successful subprocess run.
        """
        # Mock the successful return of `subprocess.run`.
        mock_run.return_value = MagicMock(spec=subprocess.CompletedProcess, returncode=0, stdout='success', stderr='')

        result = run_mesa_rsp_single(
            self.inlist_path1, self.mesa_binary_dir, 1, self.output_dir
        )
        
        # Assertions to check the function's output.
        self.assertEqual(result['status'], 'successful')
        self.assertIn('duration', result)
        mock_run.assert_called_once()
        # Ensure the OMP_NUM_THREADS environment variable was set correctly.
        self.assertEqual(mock_run.call_args[1]['env']['OMP_NUM_THREADS'], '1')

    @patch('mesalab.rsptools.rsp_runner.subprocess.run')
    def test_run_single_subprocess_error(self, mock_run):
        """
        Test that `run_mesa_rsp_single` handles a `subprocess.CalledProcessError`.
        """
        # Mock `subprocess.run` to raise an error.
        mock_run.side_effect = subprocess.CalledProcessError(returncode=1, cmd='test_cmd', stderr='error_message')

        result = run_mesa_rsp_single(
            self.inlist_path1, self.mesa_binary_dir, 1, self.output_dir
        )

        # Assert that the status is 'failed' and the error message is correct.
        self.assertEqual(result['status'], 'failed')
        self.assertIn('error_message', result['error'])

    @patch('mesalab.rsptools.rsp_runner.subprocess.run')
    def test_run_single_timeout(self, mock_run):
        """
        Test that `run_mesa_rsp_single` handles a `subprocess.TimeoutExpired`.
        """
        # Mock `subprocess.run` to raise a timeout.
        mock_run.side_effect = subprocess.TimeoutExpired(cmd='test_cmd', timeout=900)

        result = run_mesa_rsp_single(
            self.inlist_path1, self.mesa_binary_dir, 1, self.output_dir
        )

        # Assert that the status is 'timeout' and the error message is correct.
        self.assertEqual(result['status'], 'timeout')
        self.assertIn('Timeout', result['error'])

    @patch('mesalab.rsptools.rsp_runner.os.path.exists')
    def test_run_single_no_mesa_exe(self, mock_exists):
        """
        Test that `run_mesa_rsp_single` handles a missing MESA executable.
        """
        # A mock side effect, amely azt eredményezi, hogy az `os.path.exists`
        # a MESA futtatható fájlra False értéket ad vissza, minden másra pedig True-t.
        mock_exists.side_effect = lambda path: path != self.mesa_exe_path
        
        result = run_mesa_rsp_single(
            self.inlist_path1, self.mesa_binary_dir, 1, self.output_dir
        )
        self.assertEqual(result['status'], 'failed')
        self.assertIn('star', result['error'])

    # --- `run_mesa_rsp_workflow` tests ---
    @patch('mesalab.rsptools.rsp_runner.run_mesa_rsp_single', side_effect=lambda *args, **kwargs: {'status': 'successful', 'duration': 1})
    def test_workflow_sequential_success(self, mock_run_single):
        """
        Test the sequential workflow with all successful runs.
        """
        self.mock_config.rsp_workflow.enable_rsp_parallel = False
        
        results = run_mesa_rsp_workflow(self.inlist_paths, self.mock_config, self.output_dir)

        self.assertEqual(len(results['successful']), 2)
        self.assertEqual(len(results['failed']), 0)
        self.assertEqual(len(results['timeout']), 0)
        self.assertEqual(mock_run_single.call_count, 2)

    @patch('mesalab.rsptools.rsp_runner.run_mesa_rsp_single', side_effect=lambda *args, **kwargs: {'status': 'successful', 'duration': 1})
    def test_workflow_parallel_success(self, mock_run_single):
        """
        Test the parallel workflow with all successful runs.
        """
        self.mock_config.rsp_workflow.enable_rsp_parallel = True
        self.mock_config.rsp_workflow.max_concurrent_rsp_runs = 2
        
        results = run_mesa_rsp_workflow(self.inlist_paths, self.mock_config, self.output_dir)

        self.assertEqual(len(results['successful']), 2)
        self.assertEqual(len(results['failed']), 0)
        self.assertEqual(len(results['timeout']), 0)
        self.assertEqual(mock_run_single.call_count, 2)

    @patch('mesalab.rsptools.rsp_runner.run_mesa_rsp_single', side_effect=[
        {'status': 'successful', 'duration': 1},
        {'status': 'failed', 'error': 'run_failed', 'duration': 1},
        {'status': 'timeout', 'error': 'timeout', 'duration': 1},
        {'status': 'successful', 'duration': 1}
    ])
    def test_workflow_mixed_results(self, mock_run_single):
        """
        Test the workflow with a mix of successful, failed, and timed-out runs.
        """
        inlists = ['inlist1', 'inlist2', 'inlist3', 'inlist4']
        self.mock_config.rsp_workflow.enable_rsp_parallel = False
        
        results = run_mesa_rsp_workflow(inlists, self.mock_config, self.output_dir)

        self.assertEqual(len(results['successful']), 2)
        self.assertEqual(len(results['failed']), 1)
        self.assertEqual(len(results['timeout']), 1)
        self.assertEqual(len(results['error']), 0)

    @patch('mesalab.rsptools.rsp_runner.os.path.exists', return_value=False)
    @patch('mesalab.rsptools.rsp_runner.os.path.isdir', return_value=True)
    def test_workflow_invalid_mesa_dir(self, mock_isdir, mock_exists):
        """
        Test that the workflow handles an invalid MESA binary directory.
        """
        results = run_mesa_rsp_workflow(self.inlist_paths, self.mock_config, self.output_dir)
        self.assertIn('initial_error', results)
        self.assertEqual(results['initial_error'], 'invalid_mesa_binary_dir')
        self.assertEqual(len(results['successful']), 0)

    def test_workflow_empty_inlist(self):
        """
        Test the workflow with an empty inlist list.
        """
        results = run_mesa_rsp_workflow([], self.mock_config, self.output_dir)
        self.assertEqual(results['successful'], [])
        self.assertEqual(results['failed'], [])
        self.assertEqual(results['timeout'], [])
        self.assertEqual(results['error'], [])

if __name__ == '__main__':
    unittest.main()