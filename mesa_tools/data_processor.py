# mesa_tools/data_processor.py

import os
import pandas as pd
import numpy as np
import re
from tqdm import tqdm
import json

from mesa_tools.blue_loop_analyzer import analyze_blue_loop_and_instability

def _extract_params_from_path(path):
    """
    (Internal helper) Extracts initial mass and metallicity from a MESA run path.
    Assumes a format like 'run_nad_convos_mid_15.0MSUN_z0.0015'
    """
    mass = None
    z = None
    
    # Try to find mass (e.g., 15.0MSUN)
    mass_match = re.search(r'(\d+\.?\d*)MSUN', path)
    if mass_match:
        mass = float(mass_match.group(1))

    # Try to find metallicity (e.g., z0.0015)
    z_match = re.search(r'z(\d+\.?\d*)', path)
    if z_match:
        z = float(z_match.group(1))
        
    return mass, z

def scan_mesa_runs_dirs(input_dir, inlist_name):
    """
    Scans the input directory for MESA run subdirectories based on the presence of an inlist file.

    Args:
        input_dir (str): The root directory to scan.
        inlist_name (str): The name of the inlist file to identify MESA run directories.

    Returns:
        list: A list of dictionaries, where each dict contains 'path', 'mass', and 'z' for a run.
    """
    mesa_run_dirs = []
    print(f"Scanning directory: {input_dir} for MESA runs identified by '{inlist_name}'...")
    for root, dirs, files in os.walk(input_dir):
        if inlist_name in files:
            mass, z = _extract_params_from_path(root)
            if mass is not None and z is not None:
                mesa_run_dirs.append({
                    'path': root,
                    'mass': mass,
                    'z': z
                })
            else:
                print(f"Warning: Could not extract mass/Z from path: {root}. Skipping.")
    if not mesa_run_dirs:
        print("No MESA run directories found with specified inlist file.")
    return mesa_run_dirs

def process_grid_analysis(mesa_run_dirs, output_dir, regenerate_summary):
    """
    Processes the MESA grid, analyzes blue loops, and generates summary/detail CSVs.

    Args:
        mesa_run_dirs (list): List of dictionaries with 'path', 'mass', 'z' for each MESA run.
        output_dir (str): Base output directory.
        regenerate_summary (bool): If True, forces re-analysis even if summary exists.

    Returns:
        tuple: (pd.DataFrame for internal heatmap use, dict of run_paths_data).
               Returns (None, None) if no runs are processed or an error occurs.
    """
    analysis_results_output_dir = os.path.join(output_dir, "analysis_results") 
    os.makedirs(analysis_results_output_dir, exist_ok=True) # Ensure this exists for output files

    summary_csv_path = os.path.join(analysis_results_output_dir, "mesa_grid_analysis_summary.csv")
    run_paths_json_path = os.path.join(analysis_results_output_dir, "run_paths.json") 
    combined_detail_output_path = os.path.join(analysis_results_output_dir, "mesa_grid_all_blue_loop_detail.csv")
    
    final_results_df_for_heatmap = None # This will store the data in a format suitable for plotting
    run_paths_data = {}
    all_blue_loop_detail_dfs = [] 

    if not regenerate_summary and os.path.exists(summary_csv_path) and os.path.exists(run_paths_json_path):
        print(f"Loading existing summary data from {summary_csv_path} (assuming legacy format)...")
        try:
            temp_df_for_heatmap_conv = pd.read_csv(summary_csv_path, index_col=0)
            temp_df_for_heatmap_conv.columns = temp_df_for_heatmap_conv.columns.astype(float)
            temp_df_for_heatmap_conv.index = temp_df_for_heatmap_conv.index.astype(float)
            
            data_for_heatmap_conversion = {}
            for z_val in temp_df_for_heatmap_conv.index:
                for m_val in temp_df_for_heatmap_conv.columns:
                    val_str = temp_df_for_heatmap_conv.loc[z_val, m_val]
                    if isinstance(val_str, str) and val_str.startswith('['):
                        try:
                            val_list = json.loads(val_str.replace("'", '"')) 
                            data_for_heatmap_conversion[(z_val, m_val)] = val_list[0] # First element is the count
                        except Exception as e:
                            data_for_heatmap_conversion[(z_val, m_val)] = np.nan
                    else:
                        data_for_heatmap_conversion[(z_val, m_val)] = np.nan

            processed_heatmap_data = pd.DataFrame.from_dict(data_for_heatmap_conversion, orient='index', columns=['blue_loop_crossing_count'])
            processed_heatmap_data.index.names = ['initial_Z', 'initial_mass']
            final_results_df_for_heatmap = processed_heatmap_data.sort_index()
            print("Successfully loaded existing summary.")

            # Load run_paths_data from JSON file
            print(f"Loading run paths from {run_paths_json_path}...")
            with open(run_paths_json_path, 'r') as f:
                run_paths_data = json.load(f)
            print("Successfully loaded run paths.")

        except FileNotFoundError:
            print(f"Warning: {run_paths_json_path} not found. Proceeding with full analysis.")
            # If JSON is missing, we must regenerate everything
            regenerate_summary = True 
        except Exception as e:
            print(f"Error loading summary CSV or run paths JSON: {e}. Proceeding with full analysis.")
            regenerate_summary = True # Force regeneration on error
            final_results_df_for_heatmap = None
            run_paths_data = {}

    if regenerate_summary or final_results_df_for_heatmap is None: # Perform full analysis if needed
        print("Performing full analysis from MESA run directories...")
        if not mesa_run_dirs:
            print("No MESA run directories provided for analysis.")
            return None, None, None, None # Ensure all return values are None

        unique_zs = sorted(list(set(run['z'] for run in mesa_run_dirs)))
        unique_masses = sorted(list(set(run['mass'] for run in mesa_run_dirs)))

        result_table_for_summary = np.full((len(unique_zs), len(unique_masses)), 'N/A', dtype=object)
        z_to_idx = {z: i for i, z in enumerate(unique_zs)}
        mass_to_idx = {m: i for i, m in enumerate(unique_masses)}
        
        total_runs = len(mesa_run_dirs)
        print(f"Starting analysis of {total_runs} MESA runs...")

        with tqdm(total=total_runs, desc="Analyzing MESA runs") as pbar:
            for i, run_info in enumerate(mesa_run_dirs):
                run_path = run_info['path']
                current_mass = run_info['mass']
                current_z = run_info['z']
                
                # Store path mapping for JSON
                z_key = f"Z{current_z:.4f}"
                mass_key = f"M{current_mass:.1f}"
                if z_key not in run_paths_data:
                    run_paths_data[z_key] = {}
                run_paths_data[z_key][mass_key] = run_path

                analysis_result = analyze_blue_loop_and_instability(run_path)

                if analysis_result:
                    crossing_count = analysis_result.get('crossing_count', np.nan)
                    state_times_list = analysis_result.get('state_times', [np.nan]*6)
                    
                    # Ensure all 6 elements are present and are floats or NaN for consistency
                    state_times_list = [float(x) if x is not np.nan else np.nan for x in state_times_list]
                    
                    # Combine count and state_times into a single list
                    # Format as string for CSV cell, mimicking the original cross_model.csv
                    formatted_list_elements = [str(int(crossing_count)) if not np.isnan(crossing_count) else '0'] + \
                                              [f"{t:.6f}" if not np.isnan(t) else '0' for t in state_times_list]
                    result_table_for_summary[z_to_idx[current_z], mass_to_idx[current_mass]] = f"[{', '.join(formatted_list_elements)}]"

                    # If the detail DataFrame was created, add it to the list for combined saving
                    if analysis_result['blue_loop_detail_df'] is not None and not analysis_result['blue_loop_detail_df'].empty:
                        detailed_df_for_saving = analysis_result['blue_loop_detail_df'].copy()
                        detailed_df_for_saving.insert(0, 'initial_mass', current_mass)
                        detailed_df_for_saving.insert(0, 'initial_Z', current_z)
                        all_blue_loop_detail_dfs.append(detailed_df_for_saving)
                else: # analysis_result is None (e.g., file not found, incomplete data)
                    result_table_for_summary[z_to_idx[current_z], mass_to_idx[current_mass]] = 'N/A'

                pbar.update(1)

        print("\nFull analysis complete. Compiling summary results...")
        
        # Always save in the legacy format now
        with open(summary_csv_path, mode='w', newline='') as cross_file:
            df_to_save = pd.DataFrame(result_table_for_summary, index=unique_zs, columns=unique_masses)
            df_to_save.index.name = "Z \\ M"
            df_to_save.columns = [f'{m:.1f}' for m in unique_masses]
            df_to_save.to_csv(cross_file, na_rep='N/A')
        print(f"Main summary results (legacy format) saved to {summary_csv_path}")
        
        # Load the saved legacy CSV to create 'final_results_df_for_heatmap' for internal use
        try:
            temp_df_for_heatmap_conv = pd.read_csv(summary_csv_path, index_col=0)
            temp_df_for_heatmap_conv.columns = temp_df_for_heatmap_conv.columns.astype(float)
            temp_df_for_heatmap_conv.index = temp_df_for_heatmap_conv.index.astype(float)
            
            data_for_heatmap_conversion = {}
            for z_val in temp_df_for_heatmap_conv.index:
                for m_val in temp_df_for_heatmap_conv.columns:
                    val_str = temp_df_for_heatmap_conv.loc[z_val, m_val]
                    if isinstance(val_str, str) and val_str.startswith('['):
                        try:
                            val_list = json.loads(val_str.replace("'", '"')) 
                            data_for_heatmap_conversion[(z_val, m_val)] = val_list[0] # First element is the count
                        except Exception as e:
                            data_for_heatmap_conversion[(z_val, m_val)] = np.nan
                    else:
                        data_for_heatmap_conversion[(z_val, m_val)] = np.nan

            processed_heatmap_data = pd.DataFrame.from_dict(data_for_heatmap_conversion, orient='index', columns=['blue_loop_crossing_count'])
            processed_heatmap_data.index.names = ['initial_Z', 'initial_mass']
            final_results_df_for_heatmap = processed_heatmap_data.sort_index()

        except Exception as e:
            print(f"Error converting legacy summary for internal use: {e}. Some operations might be incomplete.")
            final_results_df_for_heatmap = pd.DataFrame(columns=['initial_Z', 'initial_mass', 'blue_loop_crossing_count']) # Empty DataFrame


        # Save run_paths_data to JSON file
        try:
            sorted_run_paths_data = {}
            for z_key in sorted(run_paths_data.keys()):
                sorted_mass_data = {}
                mass_keys_sorted = sorted(run_paths_data[z_key].keys(), 
                                          key=lambda x: float(x.replace('M', '')))
                for mass_key in mass_keys_sorted:
                    sorted_mass_data[mass_key] = run_paths_data[z_key][mass_key]
                sorted_run_paths_data[z_key] = sorted_mass_data

            with open(run_paths_json_path, 'w') as f:
                json.dump(sorted_run_paths_data, f, indent=4)
            print(f"Run paths saved to {run_paths_json_path}")
        except Exception as e:
            print(f"Error saving run paths to JSON: {e}")

        # Save the aggregated detailed dataframes for each Z
        if all_blue_loop_detail_dfs:
            print(f"Saving aggregated detailed files to '{combined_detail_output_path}'...")
            combined_detail_df = pd.concat(all_blue_loop_detail_dfs, ignore_index=True)
            combined_detail_df = combined_detail_df.sort_values(by=['initial_Z', 'initial_mass', 'star_age'])
            
            combined_detail_df.to_csv(combined_detail_output_path, index=False)
            print(f"  Saved: {combined_detail_output_path}")

        else:
            print("No detailed blue loop data found to save.")
            
    return final_results_df_for_heatmap, run_paths_data, summary_csv_path, combined_detail_output_path