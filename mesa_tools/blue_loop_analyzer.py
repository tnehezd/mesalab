import numpy as np
import pandas as pd
import os
import re
from matplotlib.path import Path

# Definition of the Instability Strip (replicated from your original code, needed here)
instability_strip_vertices = np.array([
    [3.83, 2.4],   # Top-left (Blue edge)
    [3.76, 4.5],   # Bottom-left
    [3.65, 4.5],   # Bottom-right (Red edge)
    [3.77, 2.4]    # Top-right
])
instability_path = Path(instability_strip_vertices)

def inside_instability_strip(log_Teff, log_L):
    """Checks if a star's (log_Teff, log_L) point is within the instability strip."""
    return instability_path.contains_point((log_Teff, log_L))

def analyze_blue_loop_and_instability(run_path):
    """
    Analyzes a MESA run's history.data file for blue loop crossings and instability strip data.

    Args:
        run_path (str): The path to the root directory of a MESA run (e.g., 'path/to/run_Z0.014_M10.0').

    Returns:
        dict: A dictionary containing the analysis results:
              - 'crossing_count' (int): The number of instability strip crossings.
              - 'state_times' (list): A list of entry/exit ages (max 6 values).
              - 'blue_loop_detail_df' (pd.DataFrame): DataFrame with data points during the blue loop phase,
                filtered as per your original 'detail' file logic.
              - 'ms_end_age' (float): Age at the end of the main sequence (if identifiable).
              - 'min_teff_post_ms_age' (float): Corresponds to the LogL local minimum used in your original script.
              If an error occurs or data is incomplete, appropriate values will be None or NaN.
    """
    history_file_path = os.path.join(run_path, 'LOGS', 'history.data')
    
    result = {
        'crossing_count': np.nan,
        'state_times': [], # This list will store the 6 age points
        'blue_loop_detail_df': None,
        'ms_end_age': np.nan,
        'min_teff_post_ms_age': np.nan,
    }

    if not os.path.exists(history_file_path):
        # print(f"Warning: history.data not found at {history_file_path}") # Suppress verbose warnings
        return result

    try:
        data = np.genfromtxt(history_file_path, names=True, comments="#", skip_header=5)

        required_columns = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g']
        if not all(col in data.dtype.names for col in required_columns):
            # print(f"Warning: Missing required columns in {history_file_path}")
            return result

        log_Teff = np.array(data['log_Teff'])
        log_L = np.array(data['log_L'])
        center_h1 = np.array(data['center_h1'])
        star_age = np.array(data['star_age'])
        model_number = np.array(data['model_number'])
        log_g = np.array(data['log_g'])

        if len(log_Teff) == 0:
            return result

        # 1. Identify the end of the Main Sequence (MS) (center_h1 < 1e-4)
        hydrogen_exhaustion_idx = np.argmax(center_h1 < 1e-4)
        if hydrogen_exhaustion_idx == 0 and center_h1[0] >= 1e-4: # If MS has not ended yet
            return result
        
        result['ms_end_age'] = star_age[hydrogen_exhaustion_idx] if hydrogen_exhaustion_idx > 0 else np.nan

        # 2. Find the local minimum of LogL after MS end (to identify blue loop start)
        # This part mimics your original script's logL_min_idx and logL_max_idx logic
        if hydrogen_exhaustion_idx < len(log_L) - 1:
            post_ms_log_L = log_L[hydrogen_exhaustion_idx:]
            if len(post_ms_log_L) > 0:
                # Find the local minimum of log_L in the post-MS phase
                logL_min_idx_relative = np.argmin(post_ms_log_L)
                logL_min_abs_idx = hydrogen_exhaustion_idx + logL_min_idx_relative
                
                result['min_teff_post_ms_age'] = star_age[logL_min_abs_idx] 

                # Define the Blue Loop section for analysis: starting from the LogL minimum found
                start_analysis_idx = logL_min_abs_idx 
                
                logTeff_bl = log_Teff[start_analysis_idx:]
                logL_bl = log_L[start_analysis_idx:]
                star_age_bl = star_age[start_analysis_idx:]
                model_number_bl = model_number[start_analysis_idx:]
                log_g_bl = log_g[start_analysis_idx:]

                # Check if points are inside the instability strip
                inside_flags = [inside_instability_strip(teff, l) for teff, l in zip(logTeff_bl, logL_bl)]

                state_changes = 0
                # Initialize with NaN to differentiate from 0 if no crossing happens
                current_state_times = [np.nan] * 6 

                # Search for entry/exit changes
                # Requires at least two points to detect a change (transition)
                if len(inside_flags) > 1:
                    for i in range(1, len(inside_flags)):
                        # Entry: if current point is inside and previous was outside (False -> True)
                        if inside_flags[i] and not inside_flags[i-1]:
                            state_changes += 1
                            if state_changes <= 6:
                                current_state_times[state_changes - 1] = star_age_bl[i]

                        # Exit: if current point is outside and previous was inside (True -> False)
                        elif not inside_flags[i] and inside_flags[i-1]:
                            state_changes += 1
                            if state_changes <= 6:
                                current_state_times[state_changes - 1] = star_age_bl[i]
                
                # The number of crossings is half the number of state changes (each pair is one full crossing)
                crossing_count = state_changes // 2
                result['crossing_count'] = crossing_count
                result['state_times'] = current_state_times

                # Prepare data for the 'detail' DataFrame, mimicking your original script's filtering:
                # Only include points on the bluer side of the IS blue edge.
                blue_edge_y = np.array([instability_strip_vertices[0][1], instability_strip_vertices[1][1]]) 
                blue_edge_x = np.array([instability_strip_vertices[0][0], instability_strip_vertices[1][0]]) 

                detail_data = []
                for i in range(len(logTeff_bl)):
                    log_teff_point = logTeff_bl[i]
                    log_l_point = logL_bl[i]
                    
                    # Interpolate the log_Teff value on the blue edge for the current log_L
                    if log_l_point >= min(blue_edge_y) and log_l_point <= max(blue_edge_y):
                        blue_edge_teff = np.interp(log_l_point, blue_edge_y, blue_edge_x)
                        # Add a small buffer (0.01) as in your original code
                        if log_teff_point > blue_edge_teff + 0.01:
                            detail_data.append({
                                'star_age': star_age_bl[i],
                                'model_number': model_number_bl[i],
                                'log_Teff': log_teff_point,
                                'log_L': log_l_point,
                                'log_g': log_g_bl[i],
                                # Note: nu_radial and eta_radial are not included here as they come from
                                # profile.data, which this analyzer currently does not read.
                            })
                
                if detail_data:
                    result['blue_loop_detail_df'] = pd.DataFrame(detail_data)

            else:
                # print(f"Warning: No post-MS data for {history_file_path}")
                return result # No sufficient data to process

    except Exception as e:
        # print(f"Error processing {history_file_path}: {e}")
        pass # Return the initialized 'result' dict

    return result