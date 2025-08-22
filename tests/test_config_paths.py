import os
import sys
import unittest
from unittest.mock import patch, MagicMock

# Import the functions to be tested from the correct path.
# Assuming the file is at the `mesalab/io/config_paths.py` path.
# If the path is different, please modify this line.
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from mesalab.io.config_paths import find_mesa_star_dir_in_release, find_mesa_binary_dir, set_environment_variables_for_executables

class TestConfigPaths(unittest.TestCase):
    """
    Test class for the functions of the mesalab.io.config_paths module.
    """
    
    @patch('os.path.isdir')
    def test_find_mesa_star_dir_in_release_success(self, mock_isdir):
        """
        Tests that mesa_star_dir returns the correct path if it exists.
        """
        # Set up the mock so that the /dummy/mesa-dir/star path appears to be an existing folder.
        mock_isdir.return_value = True
        
        mesa_dir = '/dummy/mesa-dir'
        expected_path = '/dummy/mesa-dir/star'
        result = find_mesa_star_dir_in_release(mesa_dir)
        
        self.assertEqual(result, expected_path)
        mock_isdir.assert_called_with(expected_path)

    @patch('os.path.isdir')
    def test_find_mesa_star_dir_in_release_failure(self, mock_isdir):
        """
        Tests that mesa_star_dir returns None if the path does not exist.
        """
        # Set up the mock so that the /dummy/mesa-dir/star path does not exist.
        mock_isdir.return_value = False
        
        mesa_dir = '/dummy/mesa-dir'
        result = find_mesa_star_dir_in_release(mesa_dir)
        
        self.assertIsNone(result)

    @patch('glob.glob', return_value=[])
    @patch('os.path.exists', return_value=False)
    @patch('os.path.isdir', return_value=False)
    def test_find_mesa_binary_dir_failure(self, mock_isdir, mock_exists, mock_glob):
        """
        Tests that find_mesa_binary_dir returns None if the 'rn' binary is not found.
        """
        mesa_star_dir = '/dummy/mesa-dir/star'
        result = find_mesa_binary_dir(mesa_star_dir)
        self.assertIsNone(result)

    @patch('glob.glob', return_value=[])
    @patch('os.path.exists', return_value=True) # `rn` exists in star/work
    @patch('os.path.isdir', return_value=True) # The work folder exists
    def test_find_mesa_binary_dir_work_path(self, mock_isdir, mock_exists, mock_glob):
        """
        Tests finding the `rn` binary at the star/work/ path.
        """
        mesa_star_dir = '/dummy/mesa-dir/star'
        expected_path = '/dummy/mesa-dir/star/work'
        result = find_mesa_binary_dir(mesa_star_dir)
        self.assertEqual(result, expected_path)
    
    def test_find_mesa_binary_dir_test_suite_path(self):
        """
        Tests finding the `rn` binary at the test_suite/*/work/ path.
        """
        mesa_star_dir = '/dummy/mesa-dir/star'
        expected_path = '/dummy/mesa-dir/star/test_suite/example/work'
        
        # We use patches for the test_suite path, but not for the others.
        with patch('glob.glob', return_value=['/dummy/mesa-dir/star/test_suite/example/work']), \
             patch('os.path.isdir', side_effect=lambda x: x == expected_path), \
             patch('os.path.exists', side_effect=lambda x: x == os.path.join(expected_path, 'rn')):

            result = find_mesa_binary_dir(mesa_star_dir)
            self.assertEqual(result, expected_path)

    def test_find_mesa_binary_dir_star_path(self):
        """
        Tests finding the `rn` binary directly in the star/ folder.
        """
        mesa_star_dir = '/dummy/mesa-dir/star'
        
        # Here we must ensure that the `work` and `test_suite` paths do NOT exist.
        with patch('os.path.isdir', side_effect=lambda x: x == mesa_star_dir), \
             patch('os.path.exists', side_effect=lambda x: x == os.path.join(mesa_star_dir, 'rn')), \
             patch('glob.glob', return_value=[]):
            
            result = find_mesa_binary_dir(mesa_star_dir)
            self.assertEqual(result, mesa_star_dir)

    # --- Tests for set_environment_variables_for_executables ---

    def test_set_env_vars_mesa_dependent_workflow_disabled(self):
        """
        Tests that the function exits immediately if no MESA-dependent workflow is enabled.
        """
        mock_config_data = MagicMock()
        mock_config_data.gyre_workflow.get.return_value = False
        mock_config_data.rsp_workflow.get.return_value = False
        
        with patch.dict('os.environ', {}, clear=True):
            set_environment_variables_for_executables(mock_config_data)
            self.assertEqual(os.environ, {})

    @patch('os.path.isdir', return_value=True)
    @patch('os.path.exists', return_value=True)
    def test_set_env_vars_success_with_auto_detection(self, mock_exists, mock_isdir):
        """
        Tests the correct setting of environment variables and the automatic detection of paths.
        """
        # Mocks the auto-detection functions
        with patch('mesalab.io.config_paths.find_mesa_star_dir_in_release', return_value='/detected/star_dir'), \
             patch('mesalab.io.config_paths.find_mesa_binary_dir', return_value='/detected/binary_dir'):
            
            mock_config_data = MagicMock()
            mock_config_data.gyre_workflow.get.return_value = True
            mock_config_data.rsp_workflow.get.return_value = False
            mock_config_data.general_settings.get.side_effect = lambda k: {
                'mesasdk_root': '/dummy/sdk',
                'mesa_dir': '/dummy/mesa-dir'
            }.get(k)
            mock_config_data.general_settings.get.return_value = None # The mesa_star_dir and mesa_binary_dir are not explicitly set
            
            # run the function and ensure correct operation
            set_environment_variables_for_executables(mock_config_data)
            
            self.assertEqual(os.environ['MESA_DIR'], '/dummy/mesa-dir')
            self.assertEqual(os.environ['MESASDK_ROOT'], '/dummy/sdk')
            self.assertEqual(mock_config_data.general_settings.mesa_star_dir, '/detected/star_dir')
            self.assertEqual(mock_config_data.general_settings.mesa_binary_dir, '/detected/binary_dir')

    @patch('sys.exit')
    @patch('os.path.isdir', return_value=True)
    @patch('os.path.exists', return_value=True)
    def test_set_env_vars_exit_on_auto_detect_failure(self, mock_exists, mock_isdir, mock_exit):
        """
        Tests that the function exits if the automatic detection of mesa_star_dir fails.
        """
        with patch('mesalab.io.config_paths.find_mesa_star_dir_in_release', return_value=None):
            mock_config_data = MagicMock()
            mock_config_data.gyre_workflow.get.return_value = True
            mock_config_data.general_settings.get.side_effect = lambda k: {
                'mesa_dir': '/dummy/mesa-dir'
            }.get(k)
            
            set_environment_variables_for_executables(mock_config_data)
            
            mock_exit.assert_called_with(1)
