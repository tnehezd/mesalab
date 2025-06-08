import pandas as pd
import numpy as np
from matplotlib.path import Path
import os

# Define the vertices of the Instability Strip based on your provided values
# The order is important for matplotlib.path.Path: (log_Teff, log_L)
INSTABILITY_STRIP_VERTICES = np.array([
    [3.83, 2.4],     # Blue edge, top (hottest, lowest luminosity of IS relevant for blue loop)
    [3.76, 4.5],     # Blue edge, bottom (hottest, highest luminosity of IS relevant for blue loop)
    [3.65, 4.5],     # Red edge, bottom (coolest, highest luminosity)
    [3.77, 2.4]      # Red edge, top (coolest, lowest luminosity)
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
              - 'blue_loop_detail_df': DataFrame with detailed data points during the relevant blue loop phase,
                                       filtered to include only points inside or to the blue of the IS.
              Returns a dictionary with NaN values if analysis cannot be performed (e.g., missing columns, no relevant phase).
    """
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
    # This DataFrame will be used for both crossing count and initial filtering for detail_df.
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

    # Check the very first point of the relevant phase for crossing count
    if not is_in_is_series.empty and is_in_is_series.iloc[0]:
        crossing_count += 1
        currently_inside = True
        first_is_entry_age = blue_loop_candidate_df['star_age'].iloc[0]
        last_is_entry_age = blue_loop_candidate_df['star_age'].iloc[0]

    # Iterate through the series to count entries into the IS and record ages
    for i in range(1, len(is_in_is_series)):
        current_age = blue_loop_candidate_df['star_age'].iloc[i]

        if is_in_is_series.iloc[i] and not currently_inside:
            crossing_count += 1
            currently_inside = True
            if np.isnan(first_is_entry_age):
                first_is_entry_age = current_age
            last_is_entry_age = current_age

        elif not is_in_is_series.iloc[i] and currently_inside:
            currently_inside = False
            if np.isnan(first_is_exit_age):
                first_is_exit_age = current_age
            last_is_exit_age = current_age

    # --- State Times Dictionary ---
    state_times = {
        'ms_end_age': ms_end_age,
        'min_teff_post_ms_age': star_age[logL_min_abs_idx],
        'first_is_entry_age': first_is_entry_age,
        'first_is_exit_age': first_is_exit_age,
        'last_is_entry_age': last_is_entry_age,
        'last_is_exit_age': last_is_exit_age,
        'instability_start_age': first_is_entry_age,
        'instability_end_age': last_is_exit_age,
    }

    # --- KEY MODIFICATION HERE: FILTERING FOR blue_loop_detail_df ---
    # We want points INSIDE the IS OR on its REDDER side (cooler side) relevant for the blue loop.


    # 1. Identify the Red Edge of the Instability Strip
    # The red edge is defined by points [3.65, 4.5] (Red edge, bottom) and [3.77, 2.4] (Red edge, top)
    red_edge_y_coords = np.array([INSTABILITY_STRIP_VERTICES[2][1], INSTABILITY_STRIP_VERTICES[3][1]])
    red_edge_x_coords = np.array([INSTABILITY_STRIP_VERTICES[2][0], INSTABILITY_STRIP_VERTICES[3][0]])

    # First, calculate for every point if it's in the instability strip
    blue_loop_candidate_df['is_in_is'] = blue_loop_candidate_df.apply(
        lambda row: is_in_instability_strip(row['log_Teff'], row['log_L']), axis=1
    )

    # Create a 'red_edge_teff' column in blue_loop_candidate_df, initialized to NaN
    blue_loop_candidate_df['red_edge_teff'] = np.nan

    # Calculate log_Teff values on the red edge for the current log_L values.
    # Only for log_L values that fall within the IS log_L range (2.4 and 4.5).
    valid_l_range_mask = (blue_loop_candidate_df['log_L'] >= min(red_edge_y_coords)) & \
                         (blue_loop_candidate_df['log_L'] <= max(red_edge_y_coords))

    # Interpolate the red edge Teff value only for points where relevant
    blue_loop_candidate_df.loc[valid_l_range_mask, 'red_edge_teff'] = np.interp(
        blue_loop_candidate_df.loc[valid_l_range_mask, 'log_L'],
        red_edge_y_coords,
        red_edge_x_coords
    )

    # The new filtering condition:
    # 1. The point is inside the instability strip (blue_loop_candidate_df['is_in_is'] == True)
    # OR
    # 2. The point's log_Teff is LESS than the red edge's log_Teff (i.e., it's on the redder side)
    #    AND its log_L is within the instability strip's L range (to prevent irrelevant cold points from being included)
    #    AND we subtract a small epsilon (e.g., 0.01) for robust boundary handling.

    filter_condition = (blue_loop_candidate_df['is_in_is']) | \
                       ((blue_loop_candidate_df['log_Teff'] > blue_loop_candidate_df['red_edge_teff'] - 0.01) & \
                        valid_l_range_mask)

    # Apply the filter
    filtered_blue_loop_detail_df = blue_loop_candidate_df[filter_condition].copy()

    # Drop the auxiliary columns
    if 'is_in_is' in filtered_blue_loop_detail_df.columns:
        filtered_blue_loop_detail_df = filtered_blue_loop_detail_df.drop(columns=['is_in_is'])
    if 'red_edge_teff' in filtered_blue_loop_detail_df.columns: # Changed from 'blue_edge_teff'
        filtered_blue_loop_detail_df = filtered_blue_loop_detail_df.drop(columns=['red_edge_teff'])

    analysis_results = {
        'crossing_count': crossing_count,
        'state_times': state_times,
        'blue_loop_detail_df': filtered_blue_loop_detail_df # Now return the filtered DataFrame
    }

    return analysis_results