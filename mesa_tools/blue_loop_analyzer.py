import pandas as pd
import numpy as np
import os
import collections # Import collections for deque

from mesa_tools.mesa_reader import read_history_data

# Vertices defining the instability strip in the log(Teff) - log(L) plane.
# These values are approximate and might need adjustment based on specific stellar models or literature.
# Order: (log_Teff, log_L)
INSTABILITY_STRIP_VERTICES = [
    (3.85, 1.0),
    (3.72, 1.0),
    (3.72, 4.0),
    (3.85, 4.0)
]

def inside_instability_strip(log_Teff, log_L):
    """
    Checks if a given point (log_Teff, log_L) is inside the defined instability strip.
    This is a simplified check based on a rectangular region. For more precise analysis,
    a proper polygon check or a more complex definition might be needed.

    Args:
        log_Teff (float): Logarithm of effective temperature (K).
        log_L (float): Logarithm of luminosity (Lsun).

    Returns:
        bool: True if the point is inside the instability strip, False otherwise.
    """
    # Define the bounding box of the instability strip based on the vertices
    min_log_Teff = min(v[0] for v in INSTABILITY_STRIP_VERTICES)
    max_log_Teff = max(v[0] for v in INSTABILITY_STRIP_VERTICES)
    min_log_L = min(v[1] for v in INSTABILITY_STRIP_VERTICES)
    max_log_L = max(v[1] for v in INSTABILITY_STRIP_VERTICES)

    return (min_log_Teff <= log_Teff <= max_log_Teff and
            min_log_L <= log_L <= max_log_L)

def analyze_blue_loop_and_instability(run_path, mesa_output_subdir="LOGS"):
    """
    Analyzes a MESA run's history data for blue loop behavior and instability strip crossings.

    Args:
        run_path (str): The full path to the root directory of a single MESA run.
        mesa_output_subdir (str): The name of the subdirectory within the run_path
                                  where output files (like history.data) are located.

    Returns:
        dict or None: A dictionary containing analysis results (crossing count, state times,
                      and blue loop data points as a DataFrame), or None if analysis cannot be performed.
                      Returns:
                      {
                          'crossing_count': int,
                          'state_times': dict, # Dictionary of star_age at significant events
                          'blue_loop_detail_df': pandas.DataFrame # Contains relevant data points for the loop phase
                      }
    """
    history_df = read_history_data(run_path, mesa_output_subdir)

    if history_df is None:
        return None # File not found or couldn't be read successfully

    # Define required columns for the analysis and for the detailed output.
    # Note: 'center_h1' is only for internal logic, not for final output.
    required_columns_for_logic = ['model_number', 'star_age', 'log_Teff', 'log_L', 'center_h1', 'log_g']
    
    # Check for missing columns and report them explicitly.
    missing_cols = [col for col in required_columns_for_logic if col not in history_df.columns]
    if missing_cols:
        print(f"⚠️ Missing required columns in {run_path} history.data file: {', '.join(missing_cols)}. Skipping analysis.")
        return None

    # --- Step 1: Find Main Sequence (MS) end (H depletion in core) ---
    h1_threshold = 1e-4
    ms_end_idx = history_df[history_df['center_h1'] < h1_threshold].index
    
    if ms_end_idx.empty:
        return None

    ms_end_model_number = history_df.loc[ms_end_idx[0], 'model_number']
    ms_end_star_age = history_df.loc[ms_end_idx[0], 'star_age']

    # --- Step 2: Analyze Post-MS evolution for Blue Loop and Instability Strip crossings ---
    post_ms_df = history_df[history_df['model_number'] >= ms_end_model_number].copy()

    if post_ms_df.empty:
        return None

    min_teff_post_ms = post_ms_df['log_Teff'].min()
    min_teff_post_ms_row = post_ms_df[post_ms_df['log_Teff'] == min_teff_post_ms].iloc[0]

    # Select only the specific columns requested for the detailed output DataFrame
    # 'mass' column will be added in cli.py as it's from the run's metadata
    columns_for_detail_df = ['star_age', 'model_number', 'log_Teff', 'log_L', 'log_g']
    
    # Ensure these columns exist before trying to select them
    missing_detail_cols = [col for col in columns_for_detail_df if col not in post_ms_df.columns]
    if missing_detail_cols:
        print(f"⚠️ Cannot create detailed DataFrame, missing columns: {', '.join(missing_detail_cols)}")
        blue_loop_detail_df = None # Return None or empty DataFrame if essential columns are missing
    else:
        blue_loop_detail_df = post_ms_df[columns_for_detail_df].copy()


    state_times = {
        'ms_end_age': ms_end_star_age,
        'min_teff_post_ms_age': min_teff_post_ms_row['star_age'],
        'first_is_entry_age': None,
        'first_is_exit_age': None,
        'last_is_entry_age': None,
        'last_is_exit_age': None
    }

    is_in_strip = False
    is_crossing_count = 0
    
    # Iterate through the post-MS evolution to track blue loop path and instability strip crossings.
    # This loop is specifically for populating state_times and is_crossing_count.
    for index, row in post_ms_df.iterrows():
        current_log_Teff = row['log_Teff']
        current_log_L = row['log_L']
        current_star_age = row['star_age']

        # Instability Strip Crossing Detection
        currently_in_strip = inside_instability_strip(current_log_Teff, current_log_L)

        if currently_in_strip and not is_in_strip:
            is_in_strip = True
            is_crossing_count += 1
            if state_times['first_is_entry_age'] is None:
                state_times['first_is_entry_age'] = current_star_age
            state_times['last_is_entry_age'] = current_star_age

        elif not currently_in_strip and is_in_strip:
            is_in_strip = False
            if state_times['first_is_exit_age'] is None:
                state_times['first_is_exit_age'] = current_star_age
            state_times['last_is_exit_age'] = current_star_age

    return {
        'crossing_count': is_crossing_count,
        'state_times': state_times,
        'blue_loop_detail_df': blue_loop_detail_df # Now contains only specific columns
    }

# --- Example usage (for local testing of this module) ---
if __name__ == "__main__":
    test_run_path = "/home/tnehezd/workdir/mesa-r23.05.1/STRANGE/nad_convos_mid/run_nad_convos_mid_11.5MSUN_z0.0200"

    print(f"Testing blue loop analysis for: {test_run_path}")
    analysis_results = analyze_blue_loop_and_instability(test_run_path)

    if analysis_results:
        print("\nAnalysis Results:")
        print(f"  Instability Strip Crossing Count: {analysis_results['crossing_count']}")
        print("  Key State Times (Star Age):")
        for key, value in analysis_results['state_times'].items():
            print(f"    {key}: {value:.4e}" if value is not None else f"    {key}: N/A")
        
        # Now printing head of the DataFrame
        if analysis_results['blue_loop_detail_df'] is not None:
            print(f"\nFirst 5 blue loop detail data points (out of {len(analysis_results['blue_loop_detail_df'])}):")
            print(analysis_results['blue_loop_detail_df'].head())
            print("\nColumns in detailed DataFrame:")
            print(analysis_results['blue_loop_detail_df'].columns.tolist())
        else:
            print("\nNo detailed blue loop data points DataFrame.")
    else:
        print("Analysis could not be performed for the test run.")