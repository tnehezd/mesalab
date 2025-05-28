# mesa_tools/mesa_reader.py

import pandas as pd
import numpy as np
import os

def read_history_data(run_path, mesa_output_subdir="LOGS"):
    """
    Reads the history.data file from a MESA run directory into a pandas DataFrame.
    Handles two common MESA history.data formats for column headers:
    1. The 5th line starts with '#' and contains space-separated column names.
    2. The 5th line contains numerical indices, and the 6th line contains the actual names (without '#').

    Args:
        run_path (str): The full path to the root directory of a single MESA run.
        mesa_output_subdir (str): The name of the subdirectory within the run_path
                                  where output files (like history.data) are located.

    Returns:
        pandas.DataFrame or None: A DataFrame containing the history data, or None if
                                  the file cannot be found or read correctly.
    """
    history_file_path = os.path.join(run_path, mesa_output_subdir, "history.data")

    if not os.path.exists(history_file_path):
        print(f"Error: History file not found at {history_file_path}")
        return None

    try:
        header_line_num = -1 
        column_names = []

        with open(history_file_path, 'r') as f:
            lines = [f.readline() for _ in range(6)] 

            if len(lines) >= 5:
                line_5 = lines[4]
                if line_5.strip().startswith('#'):
                    column_names = [name for name in line_5.strip().lstrip('#').strip().split() if name]
                    header_line_num = 4
                elif len(lines) >= 6:
                    line_6 = lines[5]
                    column_names = [name for name in line_6.strip().split() if name]
                    header_line_num = 5
            
        if not column_names:
            print(f"Error: Failed to extract column names from {history_file_path}. File might be malformed or in an unknown format.")
            return None

        skiprows_count = header_line_num + 1

        # Use on_bad_lines='skip' (for Pandas >= 1.3.0) or error_bad_lines=False (older Pandas)
        # We need to be careful with engine='python' and on_bad_lines/error_bad_lines.
        # error_bad_lines and warn_bad_lines are deprecated in newer Pandas,
        # replaced by on_bad_lines.

        # Let's try with on_bad_lines first, as it's the modern approach.
        # If your Pandas is older than 1.3, this will raise a TypeError
        # and you'll need to revert to the deprecated arguments.
        
        try:
            history_df = pd.read_csv(
                history_file_path,
                sep=r'\s+', 
                header=None, 
                skiprows=skiprows_count, 
                engine='python',
                on_bad_lines='skip' # <--- ADDED THIS LINE
            )
        except TypeError:
            # Fallback for older Pandas versions if on_bad_lines is not supported
            print(f"Warning: on_bad_lines not supported in your Pandas version for {history_file_path}. Falling back to deprecated arguments.")
            history_df = pd.read_csv(
                history_file_path,
                sep=r'\s+', 
                header=None, 
                skiprows=skiprows_count, 
                engine='python',
                error_bad_lines=False, # <--- DEPRECATED BUT MIGHT WORK
                warn_bad_lines=True    # <--- DEPRECATED BUT MIGHT WORK
            )
            # This fallback might not completely prevent the error if the underlying
            # C parser (default for engine='c') is not used.
            # With engine='python', 'error_bad_lines' might not have the intended effect
            # for inconsistent number of fields per row in all cases.

        # Assign the extracted column names
        # Make sure the number of columns in the DataFrame matches the expected column_names
        # This can be an issue if 'on_bad_lines' skipped lines and then the total columns don't match.
        if len(history_df.columns) != len(column_names):
            print(f"Warning: Number of columns in DataFrame ({len(history_df.columns)}) "
                  f"does not match expected columns ({len(column_names)}) for {history_file_path}. "
                  f"This might happen if some lines were malformed and skipped. "
                  f"Attempting to reindex columns based on the expected number of columns found.")
            
            # This is a bit of a hack: if the dataframe has too many columns (e.g. from a bad row)
            # and the parser didn't completely handle it, we can truncate.
            # Or if it has too few, we can pad with NaN, but that's less likely for a 'too many fields' error.
            
            # The most robust way is to re-read the file line by line if read_csv fails completely
            # or if it results in mismatched columns.
            # For now, let's assume `on_bad_lines='skip'` will ensure that the remaining rows
            # conform to the expected number of columns, and if not, we'll slice.
            
            # If the DataFrame has more columns than `column_names`, slice it.
            if len(history_df.columns) > len(column_names):
                history_df = history_df.iloc[:, :len(column_names)]
            # If it has fewer, it's a more complex problem, usually handled by fillna for missing columns.
            # But the error was "saw 245" fields when expecting 177, so it's about *too many*.

        history_df.columns = column_names

        return history_df

    except FileNotFoundError:
        print(f"Error: History file not found at {history_file_path}")
        return None
    except pd.errors.EmptyDataError:
        print(f"Error: History file is empty or malformed at {history_file_path}")
        return None
    except Exception as e:
        print(f"An unexpected error occurred while reading {history_file_path}: {e}")
        return None

# --- Example usage (for local testing of this module) ---
if __name__ == "__main__":
    # IMPORTANT: Update this path to a real MESA run directory on your system for testing
    test_run_path = "/home/tnehezd/workdir/mesa-r23.05.1/STRANGE/nad_convos_mid/run_nad_convos_mid_15.0MSUN_z0.0015"
    # test_run_path = "/home/tnehezd/workdir/mesa-r23.05.1/STRANGE/nad_convos_mid/run_nad_convos_mid_7.5MSUN_z0.0115" # Uncomment to test the problematic file

    print(f"Testing history.data reading for: {test_run_path}")
    history_data = read_history_data(test_run_path)

    if history_data is not None:
        print("\nHistory data loaded successfully. First 5 rows:")
        print(history_data.head())
        print("\nAvailable columns in DataFrame:")
        print(history_data.columns.tolist())
    else:
        print("Failed to load history data for test run.")