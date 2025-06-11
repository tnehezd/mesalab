import pandas as pd
import numpy as np
from matplotlib.path import Path
import os
import logging

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

# Define physical thresholds for blue loop identification
BLUELOOP_MASS_THRESHOLD = 1.2 # In solar masses. Stars below this mass are generally not classic blue-loopers.
# Thresholds for central helium to define the blue loop relevant phase window
# BL_CENTER_HE4_START_THRESHOLD is no longer used for RGB tip finding in this version,
# as RGB tip is found as the reddest point after MS end.
BL_CENTER_HE4_END_THRESHOLD = 1e-4 # Defines the end of core helium burning for the blue loop phase

# Constant for solar surface gravity in cgs units (log10)
LOG_G_SUN = 2.4377 # log10(g_sun) in cgs (cm/s^2)
# Constant for solar effective temperature (log10)
LOG_TEFF_SUN = np.log10(5772.0) # log10(Teff_sun) in K


def is_in_instability_strip(log_Teff, log_L):
    """
    Checks if a given point (log_Teff, log_L) is inside the defined Instability Strip.
    """
    return instability_path.contains_point((log_Teff, log_L))

def analyze_blue_loop_and_instability(history_df: pd.DataFrame, initial_mass: float, initial_Z: float):
    """
    Analyzes MESA history data for blue loop characteristics and Instability Strip crossings,
    applying physical criteria to differentiate true blue loops from other IS crossings.
    This version does NOT include a 'blueward excursion' temperature threshold.
    It attempts to calculate 'log_g' if it's missing (from 'star_mass' and 'log_R')
    and 'log_R' if it's missing (from 'log_L' and 'log_Teff').

    Args:
        history_df (pd.DataFrame): DataFrame containing MESA history data.
                                   Must include 'log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'center_he4'.
                                   'log_g' and 'log_R' are preferred, but can be calculated if their
                                   respective dependencies ('star_mass', 'log_L', 'log_Teff') are present.
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
    # Initialize results. 'crossing_count' defaults to NaN for fundamental errors, 0 for 'no loop found'.
    analysis_results = {
        'crossing_count': np.nan, 
        'state_times': {},
        'blue_loop_detail_df': pd.DataFrame(),
        'blue_loop_duration_yr': np.nan,
        'max_log_L': np.nan, 'max_log_Teff': np.nan, 'max_log_R': np.nan,
        'first_model_number': np.nan, 'last_model_number': np.nan,
        'first_age_yr': np.nan, 'last_age_yr': np.nan,
        'blue_loop_start_age': np.nan, 'blue_loop_end_age': np.nan,
        'instability_start_age': np.nan, 'instability_end_age': np.nan,
        'calculated_blue_loop_duration': np.nan, 'calculated_instability_duration': np.nan
    }

    # 1. Initial check for mass threshold to quickly filter out unlikely blue-loopers
    if initial_mass < BLUELOOP_MASS_THRESHOLD:
        logging.info(f"Skipping blue loop analysis for M={initial_mass:.1f} Msun (Z={initial_Z:.4f}) "
                     f"as its mass is below the blue loop threshold ({BLUELOOP_MASS_THRESHOLD} Msun).")
        analysis_results['crossing_count'] = 0 # No blue loop by definition (mass too low)
        return analysis_results

    history_df['initial_mass'] = initial_mass
    history_df['initial_Z'] = initial_Z

    # Define core required columns. log_g and log_R are handled specially.
    core_required_columns = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'center_he4']
    
    # Check for core required columns first
    missing_core_cols = [col for col in core_required_columns if col not in history_df.columns]
    if missing_core_cols:
        logging.error(f"ERROR: Missing core required columns in history_df for M={initial_mass}, Z={initial_Z}. Missing: {missing_core_cols}. Skipping analysis.")
        return analysis_results # Still NaN, as this is a fundamental data error

    # --- Handle log_R: check if present, else calculate from log_L and log_Teff ---
    if 'log_R' not in history_df.columns:
        if 'log_L' in history_df.columns and 'log_Teff' in history_df.columns:
            logging.warning(f"WARNING: 'log_R' column missing for M={initial_mass}, Z={initial_Z}. Calculating from 'log_L' and 'log_Teff'.")
            # Calculate log_R using Stefan-Boltzmann: log10(R/R_sun) = 0.5 * (log10(L/L_sun) - 4 * (log10(Teff) - log10(Teff_sun)))
            # log_L is already log10(L/L_sun), log_Teff is log10(Teff)
            history_df['log_R'] = 0.5 * (history_df['log_L'].values - 4 * (history_df['log_Teff'].values - LOG_TEFF_SUN))
        else:
            logging.error(f"ERROR: 'log_R' column missing for M={initial_mass}, Z={initial_Z}, and cannot be calculated "
                          f"as 'log_L' or 'log_Teff' are also missing. Skipping analysis.")
            return analysis_results

    # --- Handle log_g: check if present, else calculate from star_mass and log_R ---
    if 'log_g' not in history_df.columns:
        if 'star_mass' in history_df.columns and 'log_R' in history_df.columns: # log_R is now guaranteed if it was calculable
            logging.warning(f"WARNING: 'log_g' column missing for M={initial_mass}, Z={initial_Z}. Calculating from 'star_mass' and 'log_R'.")
            # Calculate log_g. Formula: log10(g) = log10(g_sun) + log10(M/M_sun) - 2 * log10(R/R_sun)
            # star_mass is already in M_sun units, log_R is log10(R/R_sun)
            history_df['log_g'] = LOG_G_SUN + np.log10(history_df['star_mass'].values) - 2 * history_df['log_R'].values
        else:
            # If log_g is missing, and we cannot calculate it, then it's a critical error
            logging.error(f"ERROR: 'log_g' column missing for M={initial_mass}, Z={initial_Z}, and cannot be calculated "
                          f"as 'star_mass' or 'log_R' are also missing. Skipping analysis.")
            return analysis_results # Still NaN, as this is a fundamental data error
    # --- End log_g handling ---

    # Ensure data is sorted by star_age for proper time series analysis
    history_df = history_df.sort_values(by='star_age').reset_index(drop=True)

    log_Teff = history_df['log_Teff'].values
    log_L = history_df['log_L'].values
    center_h1 = history_df['center_h1'].values
    center_he4 = history_df['center_he4'].values
    star_age = history_df['star_age'].values
    model_number = history_df['model_number'].values
    log_g = history_df['log_g'].values # Now guaranteed to exist after the check/calculation
    log_R = history_df['log_R'].values # Now guaranteed to exist after the check/calculation

    # Check for empty data after sorting/resetting index (critical error)
    if history_df.empty:
        logging.warning(f"Warning: History data is empty after processing for M={initial_mass}, Z={initial_Z}.")
        return analysis_results # Still NaN, as this is a fundamental data error

    # --- 1. Main Sequence End (MS_end_age) ---
    # Find the point where central hydrogen is depleted (H1 < 1e-4)
    hydrogen_exhaustion_idx = np.where(center_h1 < 1e-4)[0]

    if len(hydrogen_exhaustion_idx) == 0:
        logging.warning(f"Warning: No hydrogen exhaustion found for M={initial_mass}, Z={initial_Z} (star might be too young or still on MS).")
        analysis_results['crossing_count'] = 0 # Set to 0 because analysis confirms no MS exhaustion (incomplete or still on MS)
        return analysis_results

    ms_end_idx = hydrogen_exhaustion_idx[0]
    ms_end_age = star_age[ms_end_idx]

    # Ensure enough data points after MS end for further analysis (critical error)
    if ms_end_idx >= len(history_df) - 2: # At least 2 points after MS end needed for subsequent checks
        logging.warning(f"Warning: MS end detected too close to the end of the track for M={initial_mass}, Z={initial_Z}. Skipping blue loop analysis.")
        analysis_results['crossing_count'] = 0 # Set to 0 because track is incomplete for BL analysis
        return analysis_results

    # --- 2. Find RGB Tip (reddest point after MS) ---
    # Find the global minimum of log_Teff in the entire post-MS phase.
    # This is a robust way to identify the RGB tip, independent of CHeB start thresholds.
    df_post_ms_for_rgb_tip = history_df.iloc[ms_end_idx:].copy()
    
    if df_post_ms_for_rgb_tip.empty or len(df_post_ms_for_rgb_tip) < 2:
        logging.warning(f"Not enough post-MS data for RGB tip search for M={initial_mass}, Z={initial_Z}. Skipping blue loop analysis.")
        analysis_results['crossing_count'] = 0 
        return analysis_results
        
    rgb_tip_relative_idx = np.argmin(df_post_ms_for_rgb_tip['log_Teff'].values)
    rgb_tip_abs_idx = ms_end_idx + rgb_tip_relative_idx
    
    # Ensure there's enough data after the RGB tip for blue loop analysis (critical error)
    if rgb_tip_abs_idx >= len(history_df) - 1: # At least 1 point after RGB tip needed
        logging.warning(f"Warning: RGB tip detected too close to end of track for M={initial_mass}, Z={initial_Z}. Skipping blue loop analysis.")
        analysis_results['crossing_count'] = 0 # Set to 0 because track is incomplete for BL analysis
        return analysis_results

    # The blueward excursion check has been removed as requested.

    # --- 3. Define the blue loop candidate phase by central helium exhaustion ---
    # The blue loop relevant phase starts at the *identified* RGB tip,
    # and ends when central helium drops below BL_CENTER_HE4_END_THRESHOLD.
    he_threshold_idx_candidates = np.where(history_df['center_he4'].iloc[rgb_tip_abs_idx:] < BL_CENTER_HE4_END_THRESHOLD)[0] 

    if len(he_threshold_idx_candidates) == 0:
        logging.warning(f"Warning: No central helium depletion below {BL_CENTER_HE4_END_THRESHOLD} found after RGB tip for M={initial_mass}, Z={initial_Z}. Considering entire post-RGB track (until end of history) as candidate.")
        he_end_abs_idx = len(history_df) - 1 # If no clear depletion, take till end of track
    else:
        he_end_abs_idx = rgb_tip_abs_idx + he_threshold_idx_candidates[0]

    # Define the blue loop candidate DataFrame as the segment from the RGB tip up to (and including)
    # the central helium depletion point. This is crucial for excluding later AGB/post-AGB phases.
    blue_loop_candidate_df = history_df.iloc[rgb_tip_abs_idx : he_end_abs_idx + 1].copy()

    # If the candidate DataFrame is empty after defining the CHeB window, it means no blue loop.
    if blue_loop_candidate_df.empty:
        logging.info(f"Blue loop candidate DataFrame is empty after CHeB windowing for M={initial_mass}, Z={initial_Z}. No blue loop found.")
        analysis_results['crossing_count'] = 0 # Analysis ran, no loop found
        return analysis_results

    # --- 4. Instability Strip Crossing Count ---
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

    # Populate state_times and blue_loop_detail_df
    analysis_results['crossing_count'] = crossing_count # This will be 0 if no crossings found in the loop
    analysis_results['state_times'] = {
        'ms_end_age': ms_end_age,
        'min_teff_post_ms_age': star_age[rgb_tip_abs_idx], # Age at the detected RGB minimum Teff
        'first_is_entry_age': first_is_entry_age,
        'first_is_exit_age': first_is_exit_age,
        'last_is_entry_age': last_is_entry_age,
        'last_is_exit_age': last_is_exit_age,
        'instability_start_age': first_is_entry_age, # Often the same as first_is_entry_age for instability
        'instability_end_age': last_is_exit_age,     # Often the same as last_is_exit_age for instability
    }
    
    # Only populate detailed metrics (max_log_L, etc.) and blue_loop_detail_df if a loop was actually found
    if crossing_count > 0:
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

        analysis_results['blue_loop_detail_df'] = filtered_blue_loop_detail_df

        if not analysis_results['blue_loop_detail_df'].empty:
            bl_df_for_metrics = analysis_results['blue_loop_detail_df']
            analysis_results['max_log_L'] = bl_df_for_metrics['log_L'].max()
            analysis_results['max_log_Teff'] = bl_df_for_metrics['log_Teff'].max()
            # 'log_R' is now guaranteed to exist due to the calculation at the start of the function.
            # No fallback needed here.
            analysis_results['max_log_R'] = bl_df_for_metrics['log_R'].max()

            analysis_results['first_model_number'] = bl_df_for_metrics['model_number'].min()
            analysis_results['last_model_number'] = bl_df_for_metrics['model_number'].max()
            analysis_results['first_age_yr'] = bl_df_for_metrics['star_age'].min()
            analysis_results['last_age_yr'] = bl_df_for_metrics['star_age'].max()
            
            # Calculate durations if possible
            if pd.notna(analysis_results['state_times'].get('first_is_entry_age')) and \
               pd.notna(analysis_results['state_times'].get('last_is_exit_age')):
                analysis_results['calculated_blue_loop_duration'] = analysis_results['state_times']['last_is_exit_age'] - \
                                                                    analysis_results['state_times']['first_is_entry_age']
                analysis_results['blue_loop_duration_yr'] = analysis_results['calculated_blue_loop_duration'] # For consistency with old column name
            
            if pd.notna(analysis_results['state_times'].get('instability_start_age')) and \
               pd.notna(analysis_results['state_times'].get('instability_end_age')):
                analysis_results['calculated_instability_duration'] = analysis_results['state_times']['instability_end_age'] - \
                                                                       analysis_results['state_times']['instability_start_age']
        else:
            # If blue_loop_detail_df is empty despite crossings, still set metrics to NaN
            analysis_results['max_log_L'] = np.nan
            analysis_results['max_log_Teff'] = np.nan
            analysis_results['max_log_R'] = np.nan
            analysis_results['first_model_number'] = np.nan
            analysis_results['last_model_number'] = np.nan
            analysis_results['first_age_yr'] = np.nan
            analysis_results['last_age_yr'] = np.nan
            analysis_results['blue_loop_duration_yr'] = np.nan
            analysis_results['calculated_blue_loop_duration'] = np.nan
            analysis_results['calculated_instability_duration'] = np.nan
    else: # crossing_count is 0, ensure all detailed metrics are NaN
        analysis_results['max_log_L'] = np.nan
        analysis_results['max_log_Teff'] = np.nan
        analysis_results['max_log_R'] = np.nan
        analysis_results['first_model_number'] = np.nan
        analysis_results['last_model_number'] = np.nan
        analysis_results['first_age_yr'] = np.nan
        analysis_results['last_age_yr'] = np.nan
        analysis_results['blue_loop_duration_yr'] = np.nan
        analysis_results['calculated_blue_loop_duration'] = np.nan
        analysis_results['calculated_instability_duration'] = np.nan
        
        # Ensure state_times are also consistent (only MS end and RGB tip age, others NaN)
        temp_ms_end_age = analysis_results['state_times'].get('ms_end_age', np.nan) if 'ms_end_age' in analysis_results['state_times'] else np.nan
        temp_min_teff_age = analysis_results['state_times'].get('min_teff_post_ms_age', np.nan) if 'min_teff_post_ms_age' in analysis_results['state_times'] else np.nan

        analysis_results['state_times'] = {
            'ms_end_age': temp_ms_end_age,
            'min_teff_post_ms_age': temp_min_teff_age,
            'first_is_entry_age': np.nan,
            'first_is_exit_age': np.nan,
            'last_is_entry_age': np.nan,
            'last_is_exit_age': np.nan,
            'instability_start_age': np.nan,
            'instability_end_age': np.nan,
        }
        analysis_results['blue_loop_detail_df'] = pd.DataFrame() # Ensure it's empty

    return analysis_results