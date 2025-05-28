import pandas as pd
import numpy as np
from matplotlib.path import Path
import os # Added for potential file operations within analyzer, if needed, though not directly used in this version.

# Define the vertices of the Instability Strip based on your provided values
# The order is important for matplotlib.path.Path: (log_Teff, log_L)
INSTABILITY_STRIP_VERTICES = np.array([
    [3.83, 2.4],    # Blue edge, top (hottest, lowest luminosity of IS relevant for blue loop)
    [3.76, 4.5],    # Blue edge, bottom (hottest, highest luminosity of IS relevant for blue loop)
    [3.65, 4.5],    # Red edge, bottom (coolest, highest luminosity)
    [3.77, 2.4]     # Red edge, top (coolest, lowest luminosity)
])

# Create a Path object for efficient point-in-polygon checks
instability_path = Path(INSTABILITY_STRIP_VERTICES)

def is_in_instability_strip(log_Teff, log_L):
    """
    Checks if a given point (log_Teff, log_L) is inside the defined Instability Strip.
    """
    return instability_path.contains_point((log_Teff, log_L))

def analyze_blue_loop_and_instability(history_df: pd.DataFrame, initial_mass: float, initial_Z: float):
    """
    Analyzes MESA history data for blue loop characteristics and Instability Strip crossings.

    Args:
        history_df (pd.DataFrame): DataFrame containing MESA history data.
                                   Must include 'log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g'.
        initial_mass (float): Initial mass of the star.
        initial_Z (float): Initial metallicity (Z) of the star.

    Returns:
        dict: A dictionary containing analysis results, including:
              - 'crossing_count': Number of times the star enters the Instability Strip during its relevant phase.
              - 'state_times': Dictionary of specific ages (MS end, min Teff post-MS, IS entries/exits).
              - 'blue_loop_detail_df': DataFrame with detailed data points during the relevant blue loop phase.
              Returns a dictionary with NaN values if analysis cannot be performed (e.g., missing columns, no relevant phase).
    """
    # --- REMOVED ALL DEBUG PRINTS FROM HERE ---
    # print(f"\n--- DEBUG: Analyzing history_df for M={initial_mass}, Z={initial_Z} ---")
    # print("History DataFrame head (first 5 rows):")
    # print(history_df.head())
    # print("\nNaN counts per column in history_df:")
    # print(history_df.isnull().sum().to_string()) # .to_string() for better formatting if many columns
    # print("\nHistory DataFrame info (dtypes and non-null counts):")
    # history_df.info()
    # print("\nHistory DataFrame tail (last 5 rows):")
    # print(history_df.tail())
    # --- END REMOVED DEBUG PRINTS ---

    # Add initial_mass and initial_Z columns to the DataFrame for context within the analyzer
    # This is done here now that the analyzer receives the raw df
    history_df['initial_mass'] = initial_mass
    history_df['initial_Z'] = initial_Z

    required_columns = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g']
    
    # Check for presence of required columns
    missing_cols = [col for col in required_columns if col not in history_df.columns]
    if missing_cols:
        print(f"ERROR: Missing required columns in history_df for M={initial_mass}, Z={initial_Z}. Missing: {missing_cols}. Skipping analysis.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    # Ensure data is sorted by star_age for proper time series analysis
    history_df = history_df.sort_values(by='star_age').reset_index(drop=True)

    log_Teff = history_df['log_Teff'].values
    log_L = history_df['log_L'].values
    center_h1 = history_df['center_h1'].values
    star_age = history_df['star_age'].values
    model_number = history_df['model_number'].values
    log_g = history_df['log_g'].values

    # Check for empty data after sorting/resetting index
    if history_df.empty:
        print(f"Warning: History data is empty after processing for M={initial_mass}, Z={initial_Z}.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    # --- 1. Main Sequence End (MS_end_age) ---
    # Find the point where central hydrogen is depleted (H1 < 1e-4)
    hydrogen_exhaustion_idx = np.where(center_h1 < 1e-4)[0]
    
    if len(hydrogen_exhaustion_idx) == 0:
        print(f"Warning: No hydrogen exhaustion found for M={initial_mass}, Z={initial_Z} (star might be too young or still on MS).")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }
    
    ms_end_idx = hydrogen_exhaustion_idx[0]
    ms_end_age = star_age[ms_end_idx]

    # If the MS ends at the very beginning of the track (or very close),
    # it might indicate an incomplete pre-MS or very short MS.
    if ms_end_idx < 10: # Arbitrary threshold, can be adjusted
        print(f"Warning: MS end detected too early in the track for M={initial_mass}, Z={initial_Z}, possibly incomplete data.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    # --- 2. Determine the relevant phase for blue loop/IS crossings (AGB phase onwards) ---
    post_ms_data = history_df.iloc[ms_end_idx:].copy() # Use .copy() to avoid SettingWithCopyWarning
    
    if len(post_ms_data) < 2: # Need at least two points to find a min
        print(f"Warning: Not enough post-MS data for M={initial_mass}, Z={initial_Z} to determine AGB phase start.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    # Find the index of the minimum log_L in the post-MS phase (relative to post_ms_data)
    logL_min_relative_idx = np.argmin(post_ms_data['log_L'].values)
    
    # Absolute index in the original history_df
    logL_min_abs_idx = ms_end_idx + logL_min_relative_idx

    # Find the first point *after* logL_min_abs_idx where log_L increases (local maximum)
    if logL_min_abs_idx >= len(log_L) - 1:
        print(f"Warning: Not enough data after post-MS log_L minimum for M={initial_mass}, Z={initial_Z} to determine AGB phase start.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    # Look for a log_L increase after the minimum, this defines the start of the AGB-like phase
    # np.argmax returns the index of the first occurrence of the maximum value.
    # If all values are False, it returns 0. So we need to check if any are True.
    log_L_increase_after_min = (log_L[logL_min_abs_idx+1:] > log_L[logL_min_abs_idx])
    
    if not log_L_increase_after_min.any():
        print(f"Warning: No clear AGB phase start detected after post-MS log_L minimum for M={initial_mass}, Z={initial_Z}. Likely no blue loop relevant for IS crossings.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }
    
    agb_start_relative_idx = np.argmax(log_L_increase_after_min)
    agb_start_abs_idx = logL_min_abs_idx + 1 + agb_start_relative_idx

    # Define the blue loop candidate phase from this AGB start point to the end of the track.
    blue_loop_candidate_df = history_df.iloc[agb_start_abs_idx:].copy()

    # --- 3. Instability Strip Crossing Count ---
    # Determine if each point is in the instability strip for the blue loop candidate phase
    if blue_loop_candidate_df.empty:
        print(f"Warning: Blue loop candidate DataFrame is empty for M={initial_mass}, Z={initial_Z}.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    is_in_is_series = blue_loop_candidate_df.apply(
        lambda row: is_in_instability_strip(row['log_Teff'], row['log_L']), axis=1
    )

    crossing_count = 0
    first_is_entry_age = np.nan
    first_is_exit_age = np.nan
    last_is_entry_age = np.nan
    last_is_exit_age = np.nan

    currently_inside = False

    # Check the very first point of the relevant phase
    if not is_in_is_series.empty and is_in_is_series.iloc[0]:
        # If the segment starts inside, count it as an entry.
        crossing_count += 1
        currently_inside = True
        first_is_entry_age = blue_loop_candidate_df['star_age'].iloc[0]
        last_is_entry_age = blue_loop_candidate_df['star_age'].iloc[0] # Initialize last_entry

    # Iterate through the series to count entries into the IS and record ages
    for i in range(1, len(is_in_is_series)):
        current_age = blue_loop_candidate_df['star_age'].iloc[i]
        
        if is_in_is_series.iloc[i] and not currently_inside:
            # Transition from outside to inside - this is an entry
            crossing_count += 1
            currently_inside = True
            if np.isnan(first_is_entry_age):
                first_is_entry_age = current_age
            last_is_entry_age = current_age # Always update last entry

        elif not is_in_is_series.iloc[i] and currently_inside:
            # Transition from inside to outside - this is an exit
            currently_inside = False
            if np.isnan(first_is_exit_age): # First exit is the first time we leave
                first_is_exit_age = current_age
            last_is_exit_age = current_age # Always update last exit

    # If the star ends inside the instability strip, the last_is_exit_age will be NaN.
    # This is correct; it hasn't exited yet.

    # --- State Times Dictionary ---
    state_times = {
        'ms_end_age': ms_end_age,
        'min_teff_post_ms_age': star_age[logL_min_abs_idx], # This is the age at the minimum L
        'first_is_entry_age': first_is_entry_age,
        'first_is_exit_age': first_is_exit_age,
        'last_is_entry_age': last_is_entry_age,
        'last_is_exit_age': last_is_exit_age,
    }

    analysis_results = {
        'crossing_count': crossing_count,
        'state_times': state_times,
        'blue_loop_detail_df': blue_loop_candidate_df # This contains all relevant data points for detail
    }

    # --- REMOVED DEBUG PRINT: Final result from analyzer ---
    # print(f"--- DEBUG: Analyzer finished for M={initial_mass}, Z={initial_Z}. Crossing count: {crossing_count}, Detail DF empty: {blue_loop_candidate_df.empty} ---")

    return analysis_results