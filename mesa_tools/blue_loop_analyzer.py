import pandas as pd
import numpy as np
from matplotlib.path import Path

# Define the vertices of the Instability Strip based on your provided values
# The order is important for matplotlib.path.Path: (log_Teff, log_L)
INSTABILITY_STRIP_VERTICES = np.array([
    [3.83, 2.4],   # Blue edge, top (hottest, lowest luminosity of IS relevant for blue loop)
    [3.76, 4.5],   # Blue edge, bottom (hottest, highest luminosity of IS relevant for blue loop)
    [3.65, 4.5],   # Red edge, bottom (coolest, highest luminosity)
    [3.77, 2.4]    # Red edge, top (coolest, lowest luminosity)
])

# Create a Path object for efficient point-in-polygon checks
instability_path = Path(INSTABILITY_STRIP_VERTICES)

def is_in_instability_strip(log_Teff, log_L):
    """
    Checks if a given point (log_Teff, log_L) is inside the defined Instability Strip.
    """
    return instability_path.contains_point((log_Teff, log_L))

def analyze_blue_loop_and_instability(history_data):
    """
    Analyzes MESA history data for blue loop characteristics and Instability Strip crossings.

    Args:
        history_data (pd.DataFrame): DataFrame containing MESA history data.
                                     Must include 'log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g'.

    Returns:
        dict: A dictionary containing analysis results, including:
              - 'crossing_count': Number of times the star enters the Instability Strip during its relevant phase.
              - 'state_times': Dictionary of specific ages (MS end, min Teff post-MS, IS entries/exits).
              - 'blue_loop_detail_df': DataFrame with detailed data points during the relevant blue loop phase.
              Returns None if analysis cannot be performed (e.g., missing columns, no relevant phase).
    """
    required_columns = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g']
    
    # Check for presence of required columns
    if not all(col in history_data.columns for col in required_columns):
        # print(f"Missing required columns in history_data. Required: {required_columns}, Found: {history_data.columns.tolist()}")
        return None # Indicate failure if essential columns are missing

    # Ensure data is sorted by star_age for proper time series analysis
    history_data = history_data.sort_values(by='star_age').reset_index(drop=True)

    log_Teff = history_data['log_Teff'].values
    log_L = history_data['log_L'].values
    center_h1 = history_data['center_h1'].values
    star_age = history_data['star_age'].values
    model_number = history_data['model_number'].values
    log_g = history_data['log_g'].values

    # Check for empty data
    if len(history_data) == 0:
        # print("History data is empty.")
        return None

    # --- 1. Main Sequence End (MS_end_age) ---
    # Find the point where central hydrogen is depleted (H1 < 1e-4)
    hydrogen_exhaustion_idx = np.where(center_h1 < 1e-4)[0]
    
    if len(hydrogen_exhaustion_idx) == 0:
        # print("No hydrogen exhaustion found (star might be too young or still on MS).")
        return None # Star has not reached MS end or model is incomplete
    
    ms_end_idx = hydrogen_exhaustion_idx[0]
    ms_end_age = star_age[ms_end_idx]

    # If the MS ends at the very beginning of the track (or very close),
    # it might indicate an incomplete pre-MS or very short MS.
    if ms_end_idx < 10: # Arbitrary threshold, can be adjusted
        # print("MS end detected too early in the track, possibly incomplete data.")
        return None

    # --- 2. Determine the relevant phase for blue loop/IS crossings (AGB phase onwards) ---
    # Based on your original code's logic:
    # Find the minimum log_L after MS end
    if ms_end_idx >= len(log_L) - 1: # Ensure there's data after MS end
        # print("Not enough data after MS end for blue loop analysis.")
        return None

    # We need to find the local minimum of log_L after MS end for the blue loop.
    # The original code's approach:
    # 1. Find the global minimum of log_L in the post-MS phase.
    # 2. Find the *next* point where log_L increases (local maximum after the minimum).
    # This identifies the start of the AGB phase before the star potentially
    # loops to the blue.

    post_ms_data = history_data.iloc[ms_end_idx:]
    
    if len(post_ms_data) < 2: # Need at least two points to find a min
        # print("Not enough post-MS data to determine AGB phase start.")
        return None

    # Find the index of the minimum log_L in the post-MS phase (relative to post_ms_data)
    logL_min_relative_idx = np.argmin(post_ms_data['log_L'].values)
    
    # Absolute index in the original history_data
    logL_min_abs_idx = ms_end_idx + logL_min_relative_idx

    # Find the first point *after* logL_min_abs_idx where log_L increases (local maximum)
    # This marks the start of the "return to the red" or AGB phase.
    # We are looking for the *first* point where the luminosity starts increasing significantly
    # after the minimum.
    
    # To be robust, let's look for a sustained increase, not just one point.
    # A simpler approach: find the first index after logL_min_abs_idx where log_L is
    # greater than log_L at logL_min_abs_idx. This was the original logic.
    
    # Ensure there are points after logL_min_abs_idx
    if logL_min_abs_idx >= len(log_L) - 1:
        # print("Not enough data after post-MS log_L minimum to determine AGB phase start.")
        return None

    # Look for a log_L increase after the minimum, this defines the start of the AGB-like phase
    agb_start_relative_idx = np.argmax(log_L[logL_min_abs_idx+1:] > log_L[logL_min_abs_idx])
    
    # If argmax returns 0, it means the first point after min is already greater, or no point is.
    # If no point is found greater, it means log_L keeps decreasing or is flat.
    # If no increase is found, the star might not undergo an AGB phase or the track is incomplete.
    if agb_start_relative_idx == 0 and not (log_L[logL_min_abs_idx+1:] > log_L[logL_min_abs_idx]).any():
        # print("No clear AGB phase start detected after post-MS log_L minimum.")
        return None # No significant luminosity increase after the minimum, likely no blue loop relevant for IS crossings

    # The actual absolute index where the relevant AGB-like phase begins
    agb_start_abs_idx = logL_min_abs_idx + 1 + agb_start_relative_idx

    # Define the blue loop candidate phase from this AGB start point to the end of the track.
    blue_loop_candidate_df = history_data.iloc[agb_start_abs_idx:].copy()

    # --- 3. Instability Strip Crossing Count ---
    # Determine if each point is in the instability strip for the blue loop candidate phase
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

    return analysis_results