# tests/test_inlist_parser.py
import unittest
from unittest.mock import patch, mock_open, call
import os
import sys
import logging

# A szülőkönyvtár hozzáadása a python útvonalhoz, hogy importálni tudjuk az inlist_parser-t
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from mesalab.io.inlist_parser import get_mesa_params_from_inlist

class TestInlistParser(unittest.TestCase):
    def setUp(self):
        self.run_path = "/dummy/run/path"
        self.inlist_content_base = """
# MESA inlist
initial_mass = 1.0
initial_z = 0.02
initial_y = 0.28
"""
        self.inlist_content_d_notation = """
# MESA inlist
initial_mass = 5.0
initial_z = 1.25d-2
initial_y = 0.28
"""
        # A naplózás elkapása a tesztekhez, hogy ne a konzolra írjon
        self.log_stream = unittest.mock.MagicMock()
        logging.basicConfig(level=logging.DEBUG, stream=self.log_stream)
        # Az eredeti, rossz debug fájl törlése, ha esetleg létezik
        # Ez már nem releváns az inlist_parser.py javított változatával,
        # de a jó gyakorlat kedvéért érdemes benne hagyni.
        temp_debug_file = "inlist_parser_direct_debug.log"
        if os.path.exists(temp_debug_file):
            os.remove(temp_debug_file)


    def tearDown(self):
        # A naplózás alaphelyzetbe állítása minden teszt után
        for handler in logging.root.handlers[:]:
            logging.root.removeHandler(handler)

    @patch('os.path.exists')
    @patch('builtins.open', new_callable=mock_open)
    def test_successful_parsing(self, mock_file, mock_exists):
        """
        Tests that the function successfully parses all parameters from a standard inlist.
        """
        mock_exists.return_value = True
        mock_file.return_value.read.return_value = self.inlist_content_base

        result = get_mesa_params_from_inlist(self.run_path)

        self.assertIsNotNone(result)
        self.assertEqual(result.get('initial_mass'), 1.0)
        self.assertEqual(result.get('initial_z'), 0.02)
        self.assertEqual(result.get('initial_y'), 0.28)

        # A hívások ellenőrzése rugalmasabban
        mock_file.assert_called()

    @patch('os.path.exists')
    @patch('builtins.open', new_callable=mock_open)
    def test_parsing_with_d_notation(self, mock_file, mock_exists):
        """
        Tests that the function correctly handles MESA's 'd' notation for exponents.
        """
        mock_exists.return_value = True
        mock_file.return_value.read.return_value = self.inlist_content_d_notation

        result = get_mesa_params_from_inlist(self.run_path)

        self.assertIsNotNone(result)
        self.assertEqual(result.get('initial_mass'), 5.0)
        # Az assertAlmostEqual használata float összehasonlításhoz
        self.assertAlmostEqual(result.get('initial_z'), 0.0125)
        self.assertAlmostEqual(result.get('initial_y'), 0.28)
    
    @patch('os.path.exists')
    @patch('builtins.open', new_callable=mock_open)
    def test_missing_mass(self, mock_file, mock_exists):
        """
        Tests that the function returns None if 'initial_mass' is not found.
        """
        mock_exists.return_value = True
        mock_file.return_value.read.return_value = self.inlist_content_base.replace('initial_mass = 1.0', '')

        result = get_mesa_params_from_inlist(self.run_path)

        self.assertIsNone(result)
        mock_file.assert_called()

    @patch('os.path.exists')
    @patch('builtins.open', new_callable=mock_open)
    def test_missing_z(self, mock_file, mock_exists):
        """
        Tests that the function returns None if 'initial_z' is not found.
        """
        mock_exists.return_value = True
        mock_file.return_value.read.return_value = self.inlist_content_base.replace('initial_z = 0.02', '')

        result = get_mesa_params_from_inlist(self.run_path)

        self.assertIsNone(result)
        mock_file.assert_called()

    @patch('os.path.exists')
    @patch('builtins.open', new_callable=mock_open)
    def test_missing_y(self, mock_file, mock_exists):
        """
        Tests that the function returns a valid dict even if 'initial_y' is not found.
        """
        mock_exists.return_value = True
        mock_file.return_value.read.return_value = self.inlist_content_base.replace('initial_y = 0.28', '')

        result = get_mesa_params_from_inlist(self.run_path)

        self.assertIsNotNone(result)
        self.assertEqual(result.get('initial_mass'), 1.0)
        self.assertEqual(result.get('initial_z'), 0.02)
        self.assertIsNone(result.get('initial_y'))
        mock_file.assert_called()

    @patch('os.path.exists', side_effect=[True, False])
    @patch('builtins.open', new_callable=mock_open)
    def test_inlist_alternatives_order(self, mock_file, mock_exists):
        """
        Tests that the function correctly checks the primary filename first.
        """
        mock_file.return_value.read.return_value = self.inlist_content_base
    
        get_mesa_params_from_inlist(self.run_path,
                                    inlist_filename="inlist",
                                    inlist_alternatives=["inlist_alt"])
    
        # Check that it tried the primary 'inlist' first, and since it was found,
        # it did not check for 'inlist_alt'.
        expected_exists_calls = [
            call(os.path.join(self.run_path, "inlist")),
        ]
        mock_exists.assert_has_calls(expected_exists_calls)
        self.assertEqual(mock_exists.call_count, 1)

    @patch('os.path.exists', side_effect=[False, True])
    @patch('builtins.open', new_callable=mock_open)
    def test_inlist_alternatives_fallback(self, mock_file, mock_exists):
        """
        Tests that the function correctly finds an alternative inlist filename if the primary is not found.
        """
        mock_file.return_value.read.return_value = self.inlist_content_base
    
        result = get_mesa_params_from_inlist(
            self.run_path,
            inlist_filename="inlist",
            inlist_alternatives=["inlist_alt"]
        )
    
        self.assertIsNotNone(result)
        self.assertEqual(result.get('initial_mass'), 1.0)
    
        # Check that it tried the primary 'inlist' first, and since it was not found,
        # it then checked for 'inlist_alt'.
        expected_exists_calls = [
            call(os.path.join(self.run_path, "inlist")),
            call(os.path.join(self.run_path, "inlist_alt"))
        ]
        mock_exists.assert_has_calls(expected_exists_calls)
        self.assertEqual(mock_exists.call_count, 2)


if __name__ == '__main__':
    unittest.main()
