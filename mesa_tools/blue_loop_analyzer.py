import pandas as pd
import numpy as np
from matplotlib.path import Path
import os
import logging # Added logging for better output

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

# Define physical thresholds for blue loop identification (only mass threshold as requested)
BLUELOOP_MASS_THRESHOLD = 1.2 # Msun: Stars below this are typically not classic blue-loopers (as requested)
# New threshold for central helium to define the END of the blue loop phase
# This value helps cut off post-core-helium burning evolution, as requested.
BL_CENTER_HE4_END_THRESHOLD = 1e-4 # Adjusted to a very low value to capture full CHeB phase

# New threshold to define the START of core helium burning for RGB tip search
# This helps to find the RGB tip *before* the blue loop / CHeB phase ends.
BL_CENTER_HE4_START_THRESHOLD = 0.9 # Typical initial He4 is around 0.97, so 0.9 signifies start of burning


def is_in_instability_strip(log_Teff, log_L):
    """
    Checks if a given point (log_Teff, log_L) is inside the defined Instability Strip.
    """
    return instability_path.contains_point((log_Teff, log_L))

def analyze_blue_loop_and_instability(history_df: pd.DataFrame, initial_mass: float, initial_Z: float):
    """
    Analyzes MESA history data for blue loop characteristics and Instability Strip crossings.
    This version includes only the initial mass threshold and central helium exhaustion
    for defining the blue loop relevant phase.

    Args:
        history_df (pd.DataFrame): DataFrame containing MESA history data.
                                   Must include 'log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g', 'center_he4'.
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
    # 1. Initial check for mass threshold to quickly filter out unlikely blue-loopers
    if initial_mass < BLUELOOP_MASS_THRESHOLD:
        logging.info(f"Skipping blue loop analysis for M={initial_mass:.1f} Msun (Z={initial_Z:.4f}) "
                     f"as its mass is below the blue loop threshold ({BLUELOOP_MASS_THRESHOLD} Msun).")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    history_df['initial_mass'] = initial_mass
    history_df['initial_Z'] = initial_Z

    # Added 'center_he4' to required columns for helium exhaustion check
    required_columns = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g', 'center_he4']

    # Check for presence of required columns
    missing_cols = [col for col in required_columns if col not in history_df.columns]
    if missing_cols:
        logging.error(f"ERROR: Missing required columns in history_df for M={initial_mass}, Z={initial_Z}. Missing: {missing_cols}. Skipping analysis.")
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
    center_he4 = history_df['center_he4'].values # New: for helium exhaustion
    star_age = history_df['star_age'].values
    model_number = history_df['model_number'].values
    log_g = history_df['log_g'].values

    # Check for empty data after sorting/resetting index
    if history_df.empty:
        logging.warning(f"Warning: History data is empty after processing for M={initial_mass}, Z={initial_Z}.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    # --- 1. Main Sequence End (MS_end_age) ---
    # Find the point where central hydrogen is depleted (H1 < 1e-4)
    hydrogen_exhaustion_idx = np.where(center_h1 < 1e-4)[0]

    if len(hydrogen_exhaustion_idx) == 0:
        logging.warning(f"Warning: No hydrogen exhaustion found for M={initial_mass}, Z={initial_Z} (star might be too young or still on MS).")
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
        logging.warning(f"Warning: MS end detected too early in the track for M={initial_mass}, Z={initial_Z}, possibly incomplete data.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }

    # --- 2. Find RGB Tip (reddest point after MS) ---
    # We now look for the RGB tip within the phase *before* core helium burning significantly depletes.
    # This helps to avoid confusing the true RGB tip with later, colder points on the AGB.
    he_burning_start_idx_candidates = np.where(history_df['center_he4'].iloc[ms_end_idx:] < BL_CENTER_HE4_START_THRESHOLD)[0]

    if len(he_burning_start_idx_candidates) == 0:
        logging.warning(f"Warning: No significant central helium burning start detected (below {BL_CENTER_HE4_START_THRESHOLD}) after MS end for M={initial_mass}, Z={initial_Z}. Cannot reliably find RGB tip for blue loop analysis.")
        return {
            'crossing_count': 0,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }
    
    # Take the first point where central helium drops below the threshold as the start of CHeB phase
    cheb_start_abs_idx = ms_end_idx + he_burning_start_idx_candidates[0]

    # Define the slice for RGB tip search: from MS end up to the start of CHeB
    df_for_rgb_tip = history_df.iloc[ms_end_idx : cheb_start_abs_idx + 1].copy()
    
    if df_for_rgb_tip.empty or len(df_for_rgb_tip) < 2:
        logging.warning(f"Not enough data to find RGB tip between MS end and CHeB start for M={initial_mass}, Z={initial_Z}. Skipping blue loop analysis.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }
    
    # Find the global minimum of log_Teff in this *limited* post-MS, pre-CHeB phase (true RGB tip)
    rgb_tip_relative_idx = np.argmin(df_for_rgb_tip['log_Teff'].values)
    rgb_tip_abs_idx = ms_end_idx + rgb_tip_relative_idx
    
    rgb_tip_teff = history_df['log_Teff'].iloc[rgb_tip_abs_idx]
    
    # Ensure there's enough data after the RGB tip (now using a less strict check)
    if rgb_tip_abs_idx >= len(history_df) - 1: # Changed from -2 to -1 for more flexibility
        logging.warning(f"RGB tip detected too close to end of track for M={initial_mass}, Z={initial_Z}. Skipping blue loop analysis.")
        return {
            'crossing_count': np.nan,
            'state_times': {},
            'blue_loop_detail_df': pd.DataFrame()
        }
    
    # --- 3. Define the blue loop candidate phase by central helium (partial) exhaustion ---
    # The blue loop relevant phase starts at the *identified* RGB tip,
    # and ends when central helium drops below BL_CENTER_HE4_END_THRESHOLD.
    he_threshold_idx_candidates = np.where(history_df['center_he4'].iloc[rgb_tip_abs_idx:] < BL_CENTER_HE4_END_THRESHOLD)[0] 

    if len(he_threshold_idx_candidates) == 0:
        logging.warning(f"Warning: No central helium depletion below {BL_CENTER_HE4_END_THRESHOLD} found after RGB tip for M={initial_mass}, Z={initial_Z}. Considering entire post-RGB track as candidate.")
        he_end_abs_idx = len(history_df) - 1 # If no clear depletion, take till end of track
    else:
        he_end_abs_idx = rgb_tip_abs_idx + he_threshold_idx_candidates[0]

    # Define the blue loop candidate DataFrame as the segment from the RGB tip up to (and including)
    # the central helium depletion point. This is crucial for excluding later AGB/post-AGB phases.
    blue_loop_candidate_df = history_df.iloc[rgb_tip_abs_idx : he_end_abs_idx + 1].copy()

    # Removed BL_TEFF_EXCURSION_THRESHOLD, BL_MIN_LUMINOSITY_THRESHOLD, BL_MAX_TEFF_THRESHOLD_POST_RGB
    # and their corresponding filtering based on your request for "only mass threshold and central He4 exhaustion".
    # This means the code will now be more liberal in what it considers a "blue loop candidate" within
    # the defined helium-burning window.

    # --- 4. Instability Strip Crossing Count ---
    # Determine if each point is in the instability strip for the blue loop candidate phase
    if blue_loop_candidate_df.empty:
        logging.warning(f"Warning: Blue loop candidate DataFrame is empty after helium depletion windowing for M={initial_mass}, Z={initial_Z}.")
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
        'min_teff_post_ms_age': history_df['star_age'].iloc[rgb_tip_abs_idx], # Age at the detected RGB minimum Teff
        'first_is_entry_age': first_is_entry_age,
        'first_is_exit_age': first_is_exit_age,
        'last_is_entry_age': last_is_entry_age,
        'last_is_exit_age': last_is_exit_age,
        'instability_start_age': first_is_entry_age,
        'instability_end_age': last_is_exit_age,
    }

    # --- Filtering for blue_loop_detail_df (remains largely the same) ---
    # This filters points *for the output DataFrame* to only include those within or redder than the IS.
    red_edge_y_coords = np.array([INSTABILITY_STRIP_VERTICES[2][1], INSTABILITY_STRIP_VERTICES[3][1]])
    red_edge_x_coords = np.array([INSTABILITY_STRIP_VERTICES[2][0], INSTABILITY_STRIP_VERTICES[3][0]])

    blue_loop_candidate_df['is_in_is'] = blue_loop_candidate_df.apply(
        lambda row: is_in_instability_strip(row['log_Teff'], row['log_L']), axis=1
    )

    blue_loop_candidate_df['red_edge_teff'] = np.nan
    valid_l_range_mask = (blue_loop_candidate_df['log_L'] >= min(red_edge_y_coords)) & \
                         (blue_loop_candidate_df['log_L'] <= max(red_edge_y_coords))

    blue_loop_candidate_df.loc[valid_l_range_mask, 'red_edge_teff'] = np.interp(
        blue_loop_candidate_df.loc[valid_l_range_mask, 'log_L'],
        red_edge_y_coords,
        red_edge_x_coords
    )

    filter_condition = (blue_loop_candidate_df['is_in_is']) | \
                       ((blue_loop_candidate_df['log_Teff'] > blue_loop_candidate_df['red_edge_teff'] - 0.01) & \
                        valid_l_range_mask)

    filtered_blue_loop_detail_df = blue_loop_candidate_df[filter_condition].copy()

    if 'is_in_is' in filtered_blue_loop_detail_df.columns:
        filtered_blue_loop_detail_df = filtered_blue_loop_detail_df.drop(columns=['is_in_is'])
    if 'red_edge_teff' in filtered_blue_loop_detail_df.columns:
        filtered_blue_loop_detail_df = filtered_blue_loop_detail_df.drop(columns=['red_edge_teff'])

    analysis_results = {
        'crossing_count': crossing_count,
        'state_times': state_times,
        'blue_loop_detail_df': filtered_blue_loop_detail_df # Now return the filtered DataFrame
    }

    return analysis_results
