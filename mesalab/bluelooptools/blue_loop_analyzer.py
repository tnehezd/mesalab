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
BLUELOOP_MASS_THRESHOLD = 2.0 # In solar masses. Stars below this mass are generally not classic blue-loopers.
# Thresholds for central helium to define the blue loop relevant phase window
BL_CENTER_HE4_START_THRESHOLD = 0.9 # Defines the start of significant core helium burning for RGB tip search
BL_CENTER_HE4_END_THRESHOLD = 1e-4 # Defines the end of core helium burning for the blue loop phase

def safe_duration(start, end):
    """
    Safely computes the duration between two time points, ensuring both are valid and ordered.

    Args:
        start (float): Starting time value (e.g., age in years).
        end (float): Ending time value.

    Returns:
        float: Duration (end - start) if both values are non-NaN and end > start; otherwise NaN.
    """
    return end - start if pd.notna(start) and pd.notna(end) and end > start else np.nan


def is_in_instability_strip(log_Teff, log_L):
    """
    Checks if a given stellar point (log_Teff, log_L) is inside the predefined Instability Strip.

    The instability strip is defined by a hardcoded polygon (instability_path)
    in the HR Diagram (log_Teff vs log_L). The log_Teff axis is assumed to be
    inverted as is common in HR diagrams.

    Args:
        log_Teff (float): Logarithm of the effective temperature (log10(Teff)).
        log_L (float): Logarithm of the luminosity (log10(L/L_solar)).

    Returns:
        bool: True if the point is inside the instability strip, False otherwise.

    Example:
        >>> # Assuming instability_path is globally defined or accessible
        >>> # (defined at the module level in this file)
        >>> is_in_instability_strip(3.7, 3.0) # Example point inside the strip
        True
        >>> is_in_instability_strip(4.0, 3.0) # Example point outside
        False
    """
    return instability_path.contains_point((log_Teff, log_L))

def compute_true_instability_duration(df: pd.DataFrame, is_in_is_series: pd.Series) -> float:
    """
    Computes the total time the star spends inside the Instability Strip,
    by summing all entry-exit intervals within the provided phase.

    Args:
        df (pd.DataFrame): DataFrame containing 'star_age' column.
        is_in_is_series (pd.Series): Boolean Series indicating IS membership per row.

    Returns:
        float: Total duration spent inside the Instability Strip (in years).
    """
    duration = 0.0
    currently_inside = False
    entry_age = None

    for i in range(len(is_in_is_series)):
        age = df['star_age'].iloc[i]
        inside = is_in_is_series.iloc[i]

        if inside and not currently_inside:
            entry_age = age
            currently_inside = True

        elif not inside and currently_inside:
            exit_age = age
            duration += exit_age - entry_age
            currently_inside = False
            entry_age = None

    if currently_inside and entry_age is not None:
        duration += df['star_age'].iloc[-1] - entry_age

    return duration




def analyze_blue_loop_and_instability(history_df: pd.DataFrame, initial_mass: float, initial_Z: float, initial_Y: float):
    """
    Analyzes MESA history data for blue loop characteristics and Instability Strip crossings,
    applying physical criteria to differentiate true blue loops from other IS crossings.

    Args:
        history_df (pd.DataFrame): DataFrame containing MESA history data.
                                   Must include 'log_Teff', 'log_L', 'center_h1', 'star_age',
                                   'model_number', 'log_g', 'center_he4'.
        initial_mass (float): Initial mass of the star.
        initial_Z (float): Initial metallicity (Z) of the star.
        initial_Y (float): Initial helium abundance (Y) of the star.

    Returns:
        dict: A dictionary containing analysis results, including:
              - 'crossing_count': Number of times the star enters the Instability Strip during its relevant phase.
              - 'state_times': Dictionary of specific ages (MS end, min Teff post-MS, IS entries/exits).
              - 'blue_loop_detail_df': DataFrame with detailed data points during the relevant blue loop phase,
                                       filtered to include only points inside or to the blue of the IS.

        Returns a dictionary with NaN values if analysis cannot be performed (e.g., missing columns, no relevant phase).

    Example:
        >>> from mesalab.bluelooptools.blue_loop_analyzer import analyze_blue_loop_and_instability
        >>> import pandas as pd
        >>> import os
        # Load data from a MESA history.data file for a star that exhibits a blue loop.
        # This is the recommended approach for this function.
        # For this example, we'll assume 'history.data' is a pre-existing, valid file.
        >>> file_path = 'path/to/your/history.data' 
        >>> if os.path.exists(file_path):
        ...     history_df = pd.read_csv(file_path, delim_whitespace=True, skiprows=5)
        ...     # Set initial parameters based on the MESA run, like:
        ...     initial_mass = 5.0
        ...     initial_Z = 0.006
        ...     initial_Y = 0.28
        ...     result = analyze_blue_loop_and_instability(history_df, initial_mass, initial_Z, initial_Y)
        ...     print(f"Crossing count: {result['crossing_count']}")
        ... else:
        ...     print("MESA history.data file not found. Please provide a valid file.")


    """
    analysis_results = {
        'crossing_count': np.nan, 
        'state_times': {},
        'blue_loop_detail_df': pd.DataFrame(),
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
    history_df['initial_Y'] = initial_Y 

    required_columns = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g', 'center_he4']

    # Check for presence of required columns (critical error if missing)
    missing_cols = [col for col in required_columns if col not in history_df.columns]
    if missing_cols:
        logging.error(f"ERROR: Missing required columns in history_df for M={initial_mass}, Z={initial_Z}. Missing: {missing_cols}. Skipping analysis.")
        return analysis_results # NaN, as this is a fundamental data error

    # Ensure data is sorted by star_age for proper time series analysis
    history_df = history_df.sort_values(by='star_age').reset_index(drop=True)

    log_Teff = history_df['log_Teff'].values
    log_L = history_df['log_L'].values
    center_h1 = history_df['center_h1'].values
    center_he4 = history_df['center_he4'].values
    star_age = history_df['star_age'].values
    model_number = history_df['model_number'].values
    log_g = history_df['log_g'].values

    # Check for empty data after sorting/resetting index (critical error)
    if history_df.empty:
        logging.warning(f"Warning: History data is empty after processing for M={initial_mass}, Z={initial_Z}.")
        return analysis_results # NaN, as this is a fundamental data error

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

    # --- 2. Find RGB Tip (reddest point after MS before significant CHeB) ---
    # We now look for the RGB tip within the phase *before* core helium burning significantly depletes.
    he_burning_start_idx_candidates = np.where(history_df['center_he4'].iloc[ms_end_idx:] < BL_CENTER_HE4_START_THRESHOLD)[0]

    if len(he_burning_start_idx_candidates) == 0:
        logging.warning(f"Warning: No significant central helium burning start detected (below {BL_CENTER_HE4_START_THRESHOLD}) after MS end for M={initial_mass}, Z={initial_Z}. Cannot reliably find RGB tip for blue loop analysis.")
        analysis_results['crossing_count'] = 0 # Set to 0 because CHeB phase isn't reached or track is too short for reliable RGB tip
        return analysis_results

    # Take the first point where central helium drops below the threshold as the start of CHeB phase
    # This step is needed, as ms_end_idx is a relitve point, and we need the exact index, where the He-burning phase begins
    # on the stellar evolutionary track
    cheb_start_abs_idx = ms_end_idx + he_burning_start_idx_candidates[0]

    # Define the slice for RGB tip search: from MS end up to the start of CHeB
    # This is for searching the Tip: go through from the MS up to the coolest point until CHeb (aka sub-giant, giant branches)
    df_for_rgb_tip = history_df.iloc[ms_end_idx : cheb_start_abs_idx + 1].copy()
    
    if df_for_rgb_tip.empty or len(df_for_rgb_tip) < 2:
        logging.warning(f"Not enough data to find RGB tip between MS end and CHeB start for M={initial_mass}, Z={initial_Z}. Skipping blue loop analysis.")
        analysis_results['crossing_count'] = 0 # Set to 0 because data is insufficient for RGB tip search
        return analysis_results
    
    # Find the global minimum of log_Teff in this *limited* post-MS, pre-CHeB phase (true RGB tip)
    rgb_tip_relative_idx = np.argmin(df_for_rgb_tip['log_Teff'].values)
    rgb_tip_abs_idx = ms_end_idx + rgb_tip_relative_idx
    
    rgb_tip_teff = history_df['log_Teff'].iloc[rgb_tip_abs_idx]
    rgb_tip_log_L = history_df['log_L'].iloc[rgb_tip_abs_idx] 
    
    # --- Check if the RGB tip is on the red (cooler) side of the Instability Strip ---
    # Define red edge points for interpolation (log_L, log_Teff).
    # L values must be sorted for np.interp.
    red_edge_points_L = [INSTABILITY_STRIP_VERTICES[0][1], INSTABILITY_STRIP_VERTICES[1][1]] # [2.4, 4.5]
    red_edge_points_Teff = [INSTABILITY_STRIP_VERTICES[3][0], INSTABILITY_STRIP_VERTICES[2][0]] # [3.77, 3.65] (red edge Teffs corresponding to L)

    # Calculate the red edge Teff at the luminosity of the detected RGB tip
    # np.interp extrapolates if rgb_tip_log_L is outside the defined range, which is fine for this check.
    interpolated_red_edge_teff = np.interp(rgb_tip_log_L, red_edge_points_L, red_edge_points_Teff)
    
    # If the RGB tip's log_Teff is GREATER (bluer) than the interpolated red edge Teff,
    # it means the RGB tip is on the blue side or inside the Instability Strip.
    # This is NOT a valid RGB tip for a blue loop starting from the red side as per definition.
    # A small tolerance (e.g., + 0.005) can be added for floating-point precision/small deviations.
    if rgb_tip_teff > interpolated_red_edge_teff + 0.005:
        logging.info(f"RGB tip (Teff={rgb_tip_teff:.3f}, L={rgb_tip_log_L:.3f}) for M={initial_mass}, Z={initial_Z} "
                     f"is not redder than the IS red edge (interpolated Teff={interpolated_red_edge_teff:.3f} at same L). "
                     f"Skipping blue loop analysis as no valid RGB tip was found on the red side of the IS.")
        analysis_results['crossing_count'] = 0 # No valid RGB tip for blue loop definition
        return analysis_results
    # --- END RGB LOGTEFF CHECK ---

    # Ensure there's enough data after the RGB tip for blue loop analysis (critical error)
    if rgb_tip_abs_idx >= len(history_df) - 1: # At least 1 point after RGB tip needed
        logging.warning(f"RGB tip detected too close to end of track for M={initial_mass}, Z={initial_Z}. Skipping blue loop analysis.")
        analysis_results['crossing_count'] = 0 # Set to 0 because track is incomplete for BL analysis
        return analysis_results

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

    # Check the first point of the phase
    if not is_in_is_series.empty and is_in_is_series.iloc[0]:
        crossing_count += 1
        currently_inside = True
        first_is_entry_age = blue_loop_candidate_df['star_age'].iloc[0]
        last_is_entry_age = blue_loop_candidate_df['star_age'].iloc[0]

    # Iterate to count IS entries and record ages
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

    # Populate crossing metadata
    analysis_results['crossing_count'] = crossing_count
    analysis_results['state_times'] = {
        'ms_end_age': ms_end_age,
        'min_teff_post_ms_age': star_age[rgb_tip_abs_idx],
        'first_is_entry_age': first_is_entry_age,
        'first_is_exit_age': first_is_exit_age,
        'last_is_entry_age': last_is_entry_age,
        'last_is_exit_age': last_is_exit_age,
        'instability_start_age': first_is_entry_age,
        'instability_end_age': last_is_exit_age,
    }

    # --- 5. Duration Calculations ---
    # Blue loop duration = full CHeB phase (RGB tip → He exhaustion)
    analysis_results['calculated_blue_loop_duration'] = history_df['star_age'].iloc[he_end_abs_idx] - history_df['star_age'].iloc[rgb_tip_abs_idx]

    # Instability duration = time spent inside IS during CHeB
    instability_phase_df = history_df.iloc[rgb_tip_abs_idx : he_end_abs_idx + 1].copy()
    instability_mask = instability_phase_df.apply(
        lambda row: is_in_instability_strip(row['log_Teff'], row['log_L']), axis=1
    )
    analysis_results['calculated_instability_duration'] = compute_true_instability_duration(
        instability_phase_df,
        instability_mask
    )

    # --- 6. Blue Loop Detail Filtering ---
    red_edge_y_coords = np.array([INSTABILITY_STRIP_VERTICES[2][1], INSTABILITY_STRIP_VERTICES[3][1]])
    red_edge_x_coords = np.array([INSTABILITY_STRIP_VERTICES[2][0], INSTABILITY_STRIP_VERTICES[3][0]])

    blue_loop_candidate_df['is_in_is'] = is_in_is_series
    blue_loop_candidate_df['red_edge_teff'] = np.nan

    valid_l_range_mask = (blue_loop_candidate_df['log_L'] >= min(red_edge_y_coords)) & \
                         (blue_loop_candidate_df['log_L'] <= max(red_edge_y_coords))

    blue_loop_candidate_df.loc[valid_l_range_mask, 'red_edge_teff'] = np.interp(
        blue_loop_candidate_df.loc[valid_l_range_mask, 'log_L'],
        red_edge_y_coords,
        red_edge_x_coords
    )

    filter_condition = (blue_loop_candidate_df['is_in_is']) | \
                       ((blue_loop_candidate_df['log_Teff'] > blue_loop_candidate_df['red_edge_teff'] - 0.01) & valid_l_range_mask)

    filtered_blue_loop_detail_df = blue_loop_candidate_df[filter_condition].copy()
    filtered_blue_loop_detail_df.drop(columns=['is_in_is', 'red_edge_teff'], inplace=True, errors='ignore')
    analysis_results['blue_loop_detail_df'] = filtered_blue_loop_detail_df

    # --- 7. Final Metrics ---
    if not filtered_blue_loop_detail_df.empty:
        bl_df_for_metrics = filtered_blue_loop_detail_df
        analysis_results['max_log_L'] = bl_df_for_metrics['log_L'].max()
        analysis_results['max_log_Teff'] = bl_df_for_metrics['log_Teff'].max()
        analysis_results['max_log_R'] = bl_df_for_metrics.get('log_R', history_df['log_R'].max())
        analysis_results['min_log_L'] = bl_df_for_metrics['log_L'].min()
        analysis_results['min_log_Teff'] = bl_df_for_metrics['log_Teff'].min()
        analysis_results['min_log_R'] = bl_df_for_metrics.get('log_R', history_df['log_R'].min())
        analysis_results['first_model_number'] = history_df['model_number'].iloc[rgb_tip_abs_idx]
        analysis_results['last_model_number'] = history_df['model_number'].iloc[he_end_abs_idx]
        analysis_results['first_age_yr'] = history_df['star_age'].iloc[rgb_tip_abs_idx]
        analysis_results['last_age_yr'] = history_df['star_age'].iloc[he_end_abs_idx]
    else:
        # If no valid blue loop detail, set metrics to NaN
        analysis_results.update({
            'max_log_L': np.nan,
            'max_log_Teff': np.nan,
            'max_log_R': np.nan,
            'min_log_L': np.nan,
            'min_log_Teff': np.nan,
            'min_log_R': np.nan,            
            'first_model_number': np.nan,
            'last_model_number': np.nan,
            'first_age_yr': np.nan,
            'last_age_yr': np.nan,
            'calculated_blue_loop_duration': np.nan,
            'calculated_instability_duration': np.nan,
            'blue_loop_detail_df': pd.DataFrame()
        })

        # Preserve MS and RGB tip ages if available
        temp_ms_end_age = analysis_results['state_times'].get('ms_end_age', np.nan)
        temp_min_teff_age = analysis_results['state_times'].get('min_teff_post_ms_age', np.nan)

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

    return analysis_results
