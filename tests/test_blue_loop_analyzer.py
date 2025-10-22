import unittest
import pandas as pd
import numpy as np
from unittest.mock import patch

from mesalab.bluelooptools import blue_loop_analyzer as bl_analyzer


class TestBlueLoopAnalyzer(unittest.TestCase):

    # --- Simple tests for helper functions ---

    def test_is_in_instability_strip_inside(self):
        """Check if a point is correctly identified as inside the Instability Strip."""
        self.assertTrue(bl_analyzer.is_in_instability_strip(3.75, 3.5))

    def test_is_in_instability_strip_outside(self):
        """Check if points outside the strip are correctly rejected."""
        self.assertFalse(bl_analyzer.is_in_instability_strip(4.0, 3.0))  # Too hot
        self.assertFalse(bl_analyzer.is_in_instability_strip(3.6, 3.0))  # Too cool
        self.assertFalse(bl_analyzer.is_in_instability_strip(3.75, 2.0))  # Too dim
        self.assertFalse(bl_analyzer.is_in_instability_strip(3.75, 5.0))  # Too bright

    # --- Tests for analyze_blue_loop_and_instability ---

    @patch('mesalab.bluelooptools.blue_loop_analyzer.logging')
    def test_analyze_blue_loop_and_instability_low_mass(self, mock_logging):
        """Ensure low-mass stars are skipped from analysis."""
        result = bl_analyzer.analyze_blue_loop_and_instability(
            pd.DataFrame(), initial_mass=1.5, initial_Z=0.02, initial_Y=0.28
        )
        self.assertEqual(result['crossing_count'], 0)
        mock_logging.info.assert_called_once()
        self.assertIn("Skipping blue loop analysis for M=1.5 Msun", mock_logging.info.call_args[0][0])

    @patch('mesalab.bluelooptools.blue_loop_analyzer.logging')
    def test_analyze_blue_loop_and_instability_missing_columns(self, mock_logging):
        """Ensure missing required columns are handled gracefully."""
        dummy_df = pd.DataFrame({
            'log_Teff': [3.8, 3.7],
            'log_L': [2.8, 3.0],
            'star_age': [1e8, 1.1e8],
            'model_number': [1, 2],
            'log_g': [3.5, 3.2],
            'center_he4': [0.5, 0.4],
            'log_R': [1.1, 1.2]  # Required for fallback in analyzer
        })
        result = bl_analyzer.analyze_blue_loop_and_instability(
            dummy_df, initial_mass=5.0, initial_Z=0.02, initial_Y=0.28
        )
        self.assertTrue(np.isnan(result['crossing_count']))
        mock_logging.error.assert_called_once()
        self.assertIn("Missing required columns", mock_logging.error.call_args[0][0])

    @patch('mesalab.bluelooptools.blue_loop_analyzer.logging')
    def test_analyze_blue_loop_and_instability_no_hydrogen_exhaustion(self, mock_logging):
        """Ensure stars that never leave the main sequence are handled correctly."""
        dummy_df = pd.DataFrame({
            'log_Teff': [3.8, 3.7, 3.6],
            'log_L': [2.8, 2.9, 3.0],
            'center_h1': [0.9, 0.8, 0.7],  # No H exhaustion
            'star_age': [1e8, 1.1e8, 1.2e8],
            'model_number': [1, 2, 3],
            'log_g': [3.5, 3.2, 3.0],
            'center_he4': [1e-2, 1e-2, 1e-2],
            'log_R': [1.1, 1.2, 1.3]
        })
        result = bl_analyzer.analyze_blue_loop_and_instability(
            dummy_df, initial_mass=5.0, initial_Z=0.02, initial_Y=0.28
        )
        self.assertEqual(result['crossing_count'], 0)
        mock_logging.warning.assert_called_once()
        self.assertIn("No hydrogen exhaustion found", mock_logging.warning.call_args[0][0])

    @patch('mesalab.bluelooptools.blue_loop_analyzer.logging')
    @patch('mesalab.bluelooptools.blue_loop_analyzer.is_in_instability_strip')
    def test_analyze_blue_loop_and_instability_valid_loop(self, mock_is_in_is, mock_logging):
        """Test full blue loop analysis with a valid evolutionary track."""
        # Mock IS membership: only specific Teff-L pairs are considered inside the strip
        mock_is_in_is.side_effect = lambda teff, lum: (teff, lum) in [(3.7, 3.0), (3.75, 3.2)]

        dummy_df = pd.DataFrame({
            'log_Teff': [
                4.0, 3.9, 3.8,  # MS
                3.75, 3.7, 3.65,  # RGB
                3.7, 3.75, 3.8, 3.85,  # Blue loop (to blue)
                3.75, 3.7, 3.65, 3.6,  # Blue loop (to red)
                3.55, 3.5, 3.45, 3.4  # AGB
            ],
            'log_L': [
                2.0, 2.1, 2.2,
                2.3, 2.5, 2.7,
                3.0, 3.2, 3.4, 3.5,
                3.6, 3.7, 3.8, 4.0,
                4.1, 4.2, 4.3, 4.5
            ],
            'center_h1': [
                0.9, 0.8, 0.7, 0.6, 0.5, 1e-5,
                1e-5, 1e-5, 1e-5, 1e-5,
                1e-5, 1e-5, 1e-5, 1e-5,
                1e-5, 1e-5, 1e-5, 1e-5
            ],
            'star_age': np.linspace(1e8, 1.5e8, 18),
            'model_number': np.arange(1, 19),
            'log_g': np.linspace(4.0, 3.0, 18),
            'center_he4': [
                1.0, 1.0, 1.0, 1.0, 1.0, 0.99,
                0.95, 0.9, 0.85, 0.8,
                0.7, 0.6, 0.5, 0.4,
                0.3, 0.2, 0.1, 1e-5
            ],
            'log_R': np.linspace(1.0, 2.0, 18)
        })

        result = bl_analyzer.analyze_blue_loop_and_instability(
            dummy_df, initial_mass=5.0, initial_Z=0.02, initial_Y=0.28
        )

        # Basic checks
        self.assertEqual(result['crossing_count'], 1)
        self.assertTrue(pd.notna(result['state_times']['ms_end_age']))
        self.assertTrue(pd.notna(result['state_times']['min_teff_post_ms_age']))
        self.assertTrue(pd.notna(result['state_times']['first_is_entry_age']))
        self.assertTrue(pd.notna(result['state_times']['last_is_exit_age']))
        self.assertFalse(result['blue_loop_detail_df'].empty)

        # Check that only the expected models are in the IS
        expected_models = [7, 8, 9, 10]
        actual_models = result['blue_loop_detail_df']['model_number'].tolist()
        self.assertEqual(actual_models, expected_models)

        # Check max values from filtered data
        self.assertAlmostEqual(result['max_log_L'], result['blue_loop_detail_df']['log_L'].max())
        self.assertAlmostEqual(result['max_log_Teff'], result['blue_loop_detail_df']['log_Teff'].max())

        # Confirm number of IS points
        self.assertEqual(len(result['blue_loop_detail_df']), 4)

        
class TestDurationFunctions(unittest.TestCase):

    def test_safe_duration_valid(self):
        self.assertEqual(bl_analyzer.safe_duration(100.0, 150.0), 50.0)

    def test_safe_duration_nan_start(self):
        self.assertTrue(np.isnan(bl_analyzer.safe_duration(np.nan, 150.0)))

    def test_safe_duration_nan_end(self):
        self.assertTrue(np.isnan(bl_analyzer.safe_duration(100.0, np.nan)))

    def test_safe_duration_end_before_start(self):
        self.assertTrue(np.isnan(bl_analyzer.safe_duration(150.0, 100.0)))

    def test_safe_duration_equal_start_end(self):
        self.assertTrue(np.isnan(bl_analyzer.safe_duration(100.0, 100.0)))

    def test_instability_duration_single_segment(self):
        df = pd.DataFrame({'star_age': [1.0, 1.1, 1.2, 1.3]})
        is_series = pd.Series([False, True, True, False])
        self.assertEqual(bl_analyzer.compute_true_instability_duration(df, is_series), 1.3 - 1.1)

    def test_instability_duration_multiple_segments(self):
        df = pd.DataFrame({'star_age': [1.0, 1.1, 1.2, 1.3, 1.4, 1.5]})
        is_series = pd.Series([False, True, False, True, True, False])
        expected = (1.2 - 1.1) + (1.5 - 1.3)
        self.assertEqual(bl_analyzer.compute_true_instability_duration(df, is_series), expected)

    def test_instability_duration_open_ended(self):
        df = pd.DataFrame({'star_age': [1.0, 1.1, 1.2]})
        is_series = pd.Series([False, True, True])
        self.assertEqual(bl_analyzer.compute_true_instability_duration(df, is_series), 1.2 - 1.1)

    def test_instability_duration_none_inside(self):
        df = pd.DataFrame({'star_age': [1.0, 1.1, 1.2]})
        is_series = pd.Series([False, False, False])
        self.assertEqual(bl_analyzer.compute_true_instability_duration(df, is_series), 0.0)

    def test_instability_duration_enters_never_exits(self):
        df = pd.DataFrame({'star_age': [1.0, 1.1, 1.2, 1.3]})
        is_series = pd.Series([False, False, True, True])
        self.assertEqual(bl_analyzer.compute_true_instability_duration(df, is_series), 1.3 - 1.2)
        