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
                                     Must include 'log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number'.

    Returns:
        dict: A dictionary containing analysis results, including:
              - 'crossing_count': Number of times the star enters the Instability Strip during its blue loop.
              - 'state_times': Dictionary of specific ages (MS end, min Teff post-MS, IS entries/exits).
              - 'blue_loop_detail_df': DataFrame with detailed data points during the relevant blue loop phase.
              Returns None if analysis cannot be performed (e.g., missing columns, no blue loop).
    """
    required_columns = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g']
    
    # Check for presence of required columns
    if not all(col in history_data.columns for col in required_columns):
        # print(f"Missing required columns in history_data. Required: {required_columns}, Found: {history_data.columns.tolist()}")
        return None # Indicate failure if essential columns are missing

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

    # --- 2. Post-MS Minimum Teff (min_teff_post_ms_age) ---
    # Look for the minimum Teff after MS end
    # This identifies the bluest point before the star turns back towards the red giant branch
    
    # Filter data from MS end onwards
    post_ms_log_Teff = log_Teff[ms_end_idx:]
    post_ms_star_age = star_age[ms_end_idx:]
    post_ms_log_L = log_L[ms_end_idx:]
    post_ms_model_number = model_number[ms_end_idx:]
    post_ms_log_g = log_g[ms_end_idx:]

    if len(post_ms_log_Teff) == 0:
        # print("No post-MS data available.")
        return None

    # Find the index of the minimum Teff in the post-MS phase
    min_teff_post_ms_relative_idx = np.argmin(post_ms_log_Teff)
    min_teff_post_ms_age = post_ms_star_age[min_teff_post_ms_relative_idx]
    
    # The actual index in the original history_data DataFrame
    min_teff_post_ms_abs_idx = ms_end_idx + min_teff_post_ms_relative_idx

    # --- 3. Define Blue Loop Candidate Phase ---
    # The blue loop relevant phase is typically from the point of MS end to the minimum Teff post-MS,
    # and potentially beyond if the star evolves further.
    # A common approach is to consider the segment *after* the bluest point of the blue loop
    # as the relevant part for crossing the instability strip.
    # Let's consider the relevant part for crossings as the segment from MS end up to the end of the track.
    # The Instability Strip is "passed" on the way back to the red.

    # A simpler approach for the "blue loop candidate" is often from the bluest point (min_teff_post_ms_abs_idx)
    # up to the end of the track. This is where most IS crossings occur.
    
    # For blue loop detail: take data from MS end
    blue_loop_candidate_df = history_data.iloc[ms_end_idx:].copy()

    # --- 4. Instability Strip Crossing Count ---
    # Determine if each point is in the instability strip for the blue loop candidate phase
    is_in_is_series = blue_loop_candidate_df.apply(
        lambda row: is_in_instability_strip(row['log_Teff'], row['log_L']), axis=1
    )

    crossing_count = 0
    first_is_entry_age = np.nan
    first_is_exit_age = np.nan
    last_is_entry_age = np.nan
    last_is_exit_age = np.nan

    # Special case: if the track starts inside the IS for the blue loop candidate phase, count it as one entry
    if not is_in_is_series.empty and is_in_is_series.iloc[0]:
        crossing_count += 1
        first_is_entry_age = blue_loop_candidate_df['star_age'].iloc[0]
        # Treat the start as an entry, so we are 'inside'
        currently_inside = True 
    else:
        currently_inside = False

    # Iterate through the series to count entries into the IS and record ages
    for i in range(1, len(is_in_is_series)):
        if is_in_is_series.iloc[i] and not currently_inside:
            # Transition from outside to inside - this is an entry
            crossing_count += 1
            currently_inside = True
            current_age = blue_loop_candidate_df['star_age'].iloc[i]
            if np.isnan(first_is_entry_age):
                first_is_entry_age = current_age
            last_is_entry_age = current_age # Always update last entry

        elif not is_in_is_series.iloc[i] and currently_inside:
            # Transition from inside to outside - this is an exit
            currently_inside = False
            current_age = blue_loop_candidate_df['star_age'].iloc[i]
            if np.isnan(first_is_exit_age): # First exit is the first time we leave
                first_is_exit_age = current_age
            last_is_exit_age = current_age # Always update last exit

    # If the star ends inside the instability strip, the last_is_exit_age will be NaN.
    # This is correct; it hasn't exited yet.

    # --- State Times Dictionary ---
    state_times = {
        'ms_end_age': ms_end_age,
        'min_teff_post_ms_age': min_teff_post_ms_age,
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