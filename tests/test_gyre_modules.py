import unittest
from unittest.mock import patch
from easydict import EasyDict as Dict
import os
import shutil
import tempfile
import sys
# Importing gyre_modules
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import mesalab.gyretools.gyre_modules as gyre_mod

class TestGyreModules(unittest.TestCase):
    def setUp(self):
        # Create a temporary directory for tests
        self.test_dir = tempfile.mkdtemp()
        self.input_dir = os.path.join(self.test_dir, 'test_input')
        self.gyre_inlist_template_path = os.path.join(self.test_dir, 'gyre_inlist_template.inlist')
        self.filtered_csv_path = os.path.join(self.test_dir, 'filtered_profiles.csv')
        self.mock_gyre_executable = os.path.join(self.test_dir, 'mock_gyre_bin', 'gyre')

        # Create the necessary directory structure and dummy files
        os.makedirs(os.path.join(self.input_dir, 'M_2.0_Z_0.014', 'LOGS'), exist_ok=True)
        os.makedirs(os.path.dirname(self.mock_gyre_executable), exist_ok=True)
        
        # Create a dummy GYRE executable file
        with open(self.mock_gyre_executable, 'w') as f:
            f.write("#!/bin/bash\necho 'mock gyre run'")
        os.chmod(self.mock_gyre_executable, 0o755)

        # Create the `profiles.index` file using the user-provided format.
        with open(os.path.join(self.input_dir, 'M_2.0_Z_0.014', 'LOGS', 'profiles.index'), 'w') as f:
            f.write("    100 models.    lines hold model number, priority, and profile number.\n")
            f.write("      1       2       1\n")
            f.write("    110       2       2\n") # The test looks for model 110, which has profile number 2.
            f.write("    120       1       3\n")

        # Create a dummy profile110.data file
        with open(os.path.join(self.input_dir, 'M_2.0_Z_0.014', 'LOGS', 'profile110.data'), 'w') as f:
            f.write("dummy data")
            
        # Create the dummy profile2.data.GYRE file, which the code expects based on the profile number.
        with open(os.path.join(self.input_dir, 'M_2.0_Z_0.014', 'LOGS', 'profile2.data.GYRE'), 'w') as f:
            f.write("dummy GYRE data")

        # Create a `filtered_profiles.csv` file with all the required columns
        with open(self.filtered_csv_path, 'w') as f:
            f.write("initial_mass,initial_Z,min_model_number,max_model_number,mesa_run_directory\n")
            f.write("2.0,0.014,110,110,M_2.0_Z_0.014\n")

        # Create the dummy gyre_inlist template file
        with open(self.gyre_inlist_template_path, 'w') as f:
            f.write("dummy gyre inlist content")

    def tearDown(self):
        # Remove the temporary directory
        shutil.rmtree(self.test_dir)

    @patch('mesalab.gyretools.gyre_modules.shutil.which')
    def test_run_gyre_workflow_filtered_profiles_success(self, mock_which):
        """Tests the workflow in 'FILTERED_PROFILES' mode with success."""
        
        # Make shutil.which return the path to our mock executable
        mock_which.return_value = self.mock_gyre_executable
        
        config_data = Dict({
            'general_settings': {
                'gyre_dir': '/path/to/gyre',
                'output_dir': self.test_dir,
                'input_dir': self.input_dir
            },
            'gyre_workflow': {
                'run_gyre_workflow': True,
                'run_mode': 'FILTERED_PROFILES',
                'gyre_inlist_template_path': self.gyre_inlist_template_path,
                'filtered_profiles_csv_name': 'filtered_profiles.csv',
                'enable_gyre_parallel': False,
                'max_concurrent_gyre_runs': 1,
                'num_gyre_threads': 1
            }
        })
        
        original_cwd = os.getcwd()
        try:
            os.chdir(self.input_dir)
            with patch('mesalab.gyretools.gyre_modules.run_single_gyre_model', return_value=0) as mock_single_run:
                result = gyre_mod.run_gyre_workflow(config_data, filtered_profiles_csv_path=self.filtered_csv_path)

                self.assertEqual(result, 0)
                self.assertEqual(mock_single_run.call_count, 1)
        finally:
            os.chdir(original_cwd)

# You would run this from your project's root directory: `python -m unittest tests.test_gyre_modules`