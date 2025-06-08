import os
import pandas as pd
import numpy as np
import yaml
import logging
from tqdm import tqdm

# Import necessary functions from other modules
from .data_reader import scan_mesa_runs, get_data_from_history_file
from .blue_loop_analyzer import analyze_blue_loop_and_instability


def perform_mesa_analysis(args, analysis_results_sub_dir, detail_files_output_dir):
    """
    Coordinates the analysis of MESA runs, including blue loop analysis,
    data aggregation, and saving summary and detailed results.

    Args:
        args (argparse.Namespace): Command-line arguments containing input_dir,
                                   inlist_name, analyze_blue_loop, blue_loop_output_type,
                                   force_reanalysis.
        analysis_results_sub_dir (str): Path to the directory for summary/cross-grid CSVs.
        detail_files_output_dir (str): Path to the directory for detailed blue loop CSVs.

    Returns:
        tuple: A tuple containing:
            - pd.DataFrame: The main summary DataFrame of analysis results.
            - dict: A dictionary where keys are metallicities (Z) and values are
                    lists of detailed DataFrames for plotting (combined_detail_data_for_plotting),
                    which are filtered for the blue loop analysis.
            - dict: A dictionary where keys are metallicities (Z) and values are
                    lists of **full, untrimmed** history DataFrames for plotting (full_history_data_for_plotting).
                    Returns empty DataFrames/dicts if analysis cannot be performed or is skipped.
    """
    input_dir = args.input_dir
    inlist_name = args.inlist_name
    analyze_blue_loop = args.analyze_blue_loop
    blue_loop_output_type = args.blue_loop_output_type
    force_reanalysis = args.force_reanalysis

    summary_csv_path = os.path.join(analysis_results_sub_dir, "summary_results.csv")
    cross_csv_path = os.path.join(analysis_results_sub_dir, "crossing_count_grid.csv")

    reanalysis_needed = force_reanalysis or \
                        not os.path.exists(summary_csv_path) or \
                        not os.path.exists(cross_csv_path)

    logging.info(f"Analysis started. Reanalysis needed: {reanalysis_needed}")

    summary_df = pd.DataFrame()
    combined_detail_data_for_plotting = pd.DataFrame()
    full_history_data_for_plotting = {} # Initialized here

    if not reanalysis_needed:
        logging.info("Summary and cross-grid CSV files already exist. Attempting to load existing data.")
        try:
            summary_df = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])
            logging.info("Successfully loaded existing summary CSV.")
            return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting # Return loaded summary_df and empty detail/full_history if no reanalysis
        except FileNotFoundError:
            logging.warning(f"Existing summary or cross-grid CSVs not found. Forcing full reanalysis.")
            reanalysis_needed = True
        except Exception as e:
            logging.error(f"Error loading existing summary CSV: {e}. Forcing full reanalysis.")
            reanalysis_needed = True

    if reanalysis_needed:
        logging.info("Starting full analysis of MESA runs...")
        
        mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)
        if not mesa_run_infos:
            logging.info("No MESA runs found for full analysis. Returning empty DataFrames.")
            return pd.DataFrame(), pd.DataFrame(), {}

        unique_masses = sorted(set(run['mass'] for run in mesa_run_infos))
        unique_zs = sorted(set(run['z'] for run in mesa_run_infos))

        if not unique_masses or not unique_zs:
            logging.error("Error: Could not determine unique masses or metallicities from runs. Returning empty DataFrames.")
            return pd.DataFrame(), pd.DataFrame(), {}

        cross_data_matrix = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses)
        cross_data_matrix.index.name = "Z"
        cross_data_matrix.columns.name = "Mass"

        summary_data = []
        grouped_detailed_dfs_for_analysis_raw = {z_val: [] for z_val in unique_zs}
        full_history_data_for_plotting = {z_val: [] for z_val in unique_zs} # Initialized with Z keys

        # Generate YAML overview
        yaml_data = {}
        for run_info in sorted(mesa_run_infos, key=lambda x: (x['z'], x['mass'])):
            z_key = f"Z_{run_info['z']:.4f}"
            if z_key not in yaml_data:
                yaml_data[z_key] = {}
            mass_key = f"M_{run_info['mass']:.1f}"
            yaml_data[z_key][mass_key] = {
                'run_directory': os.path.basename(run_info['run_dir_path']),
                'history_file': os.path.basename(run_info['history_file_path']),
            }
        yaml_file_path = os.path.join(analysis_results_sub_dir, "processed_runs_overview.yaml")
        try:
            with open(yaml_file_path, 'w') as f:
                yaml.dump(yaml_data, f, indent=4, sort_keys=False)
            logging.info(f"Generated YAML overview of processed runs: {yaml_file_path}")
        except Exception as e:
            logging.error(f"Could not write YAML overview file: {e}")

        skipped_runs_log_path = os.path.join(analysis_results_sub_dir, "skipped_runs_log.txt")
        if os.path.exists(skipped_runs_log_path):
            os.remove(skipped_runs_log_path) # Clear previous log

        total_runs_for_analysis = len(mesa_run_infos)
        with tqdm(total=total_runs_for_analysis, desc="Performing MESA Run Analysis") as pbar:
            for run_info in mesa_run_infos:
                current_mass = run_info['mass']
                current_z = run_info['z']
                history_file_path = run_info['history_file_path']

                analysis_result_summary = {
                    'initial_mass': current_mass, 'initial_Z': current_z, 'blue_loop_crossing_count': np.nan,
                    'blue_loop_duration_yr': np.nan, 'max_log_L': np.nan, 'max_log_Teff': np.nan,
                    'max_log_R': np.nan, 'first_model_number': np.nan, 'last_model_number': np.nan,
                    'first_age_yr': np.nan, 'last_age_yr': np.nan, 'blue_loop_start_age': np.nan,
                    'blue_loop_end_age': np.nan, 'instability_start_age': np.nan,
                    'instability_end_age': np.nan, 'calculated_blue_loop_duration': np.nan,
                    'calculated_instability_duration': np.nan
                }
                current_detail_df = pd.DataFrame() # Initialize here for safety

                try:
                    df_full_history = get_data_from_history_file(history_file_path)
                    df_full_history['initial_mass'] = current_mass
                    df_full_history['initial_Z'] = current_z

                    if current_z not in full_history_data_for_plotting:
                        full_history_data_for_plotting[current_z] = []
                    full_history_data_for_plotting[current_z].append(df_full_history.copy())

                    # Initialize filtered_combined_df_bl before any conditional assignment
                    filtered_combined_df_bl = pd.DataFrame() # <--- FIX: Initialize here

                    if analyze_blue_loop:
                        analyzer_output = analyze_blue_loop_and_instability(df_full_history, current_mass, current_z)

                        if not pd.isna(analyzer_output['crossing_count']):
                            analysis_result_summary['blue_loop_crossing_count'] = int(analyzer_output['crossing_count'])
                            state_times = analyzer_output['state_times']

                            analysis_result_summary['blue_loop_start_age'] = state_times.get('first_is_entry_age', np.nan)
                            analysis_result_summary['blue_loop_end_age'] = state_times.get('last_is_exit_age', np.nan)
                            if pd.notna(analysis_result_summary['blue_loop_start_age']) and pd.notna(analysis_result_summary['blue_loop_end_age']):
                                analysis_result_summary['calculated_blue_loop_duration'] = analysis_result_summary['blue_loop_end_age'] - analysis_result_summary['blue_loop_start_age']
                                analysis_result_summary['blue_loop_duration_yr'] = analysis_result_summary['calculated_blue_loop_duration']

                            analysis_result_summary['instability_start_age'] = state_times.get('instability_start_age', np.nan)
                            analysis_result_summary['instability_end_age'] = state_times.get('instability_end_age', np.nan)
                            if pd.notna(analysis_result_summary['instability_start_age']) and pd.notna(analysis_result_summary['instability_end_age']):
                                analysis_result_summary['calculated_instability_duration'] = analysis_result_summary['instability_end_age'] - analysis_result_summary['instability_start_age']
                            
                            if not analyzer_output['blue_loop_detail_df'].empty:
                                bl_df = analyzer_output['blue_loop_detail_df'].copy()
                                if blue_loop_output_type == 'all':
                                    filtered_combined_df_bl = bl_df # <--- Now assigned
                                    analysis_result_summary['max_log_L'] = bl_df['log_L'].max()
                                    analysis_result_summary['max_log_Teff'] = bl_df['log_Teff'].max()
                                    if 'log_R' in bl_df.columns:
                                        analysis_result_summary['max_log_R'] = bl_df['log_R'].max()
                                    elif 'log_R' in df_full_history.columns:
                                        analysis_result_summary['max_log_R'] = df_full_history['log_R'].max()
                                    analysis_result_summary['first_model_number'] = bl_df['model_number'].min()
                                    analysis_result_summary['last_model_number'] = bl_df['model_number'].max()
                                    analysis_result_summary['first_age_yr'] = bl_df['star_age'].min()
                                    analysis_result_summary['last_age_yr'] = bl_df['star_age'].max()
                                else: # 'summary' output type for blue loop details
                                    concise_detail_columns_local = [ # Use local variable for clarity
                                        'initial_mass', 'initial_Z', 'star_age', 'model_number',
                                        'log_Teff', 'log_L', 'log_g', 'profile_number'
                                    ]
                                    existing_desired_cols = [col for col in concise_detail_columns_local if col in bl_df.columns]
                                    if existing_desired_cols:
                                        filtered_combined_df_bl = bl_df[existing_desired_cols] # <--- Now assigned
                                    else:
                                        logging.warning(f"No desired columns found for concise detail for M={current_mass}, Z={current_z}. Detail DF might remain empty.")
                                
                                current_detail_df = filtered_combined_df_bl # Assign the filtered BL df for saving

                            cross_data_matrix.at[current_z, current_mass] = analysis_result_summary['blue_loop_crossing_count']
                        else:
                            logging.warning(f"Blue loop analysis failed or returned no valid crossings for M={current_mass}, Z={current_z}. Results for this run will be NaN in summary.")
                    else:
                        logging.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.")

                    summary_data.append(analysis_result_summary)

                    if analyze_blue_loop and not current_detail_df.empty:
                        if current_z not in grouped_detailed_dfs_for_analysis_raw:
                            grouped_detailed_dfs_for_analysis_raw[current_z] = []
                        grouped_detailed_dfs_for_analysis_raw[current_z].append(current_detail_df)
                        
                        # Only concatenate to combined_detail_data_for_plotting if current_detail_df is not empty
                        if combined_detail_data_for_plotting.empty:
                            combined_detail_data_for_plotting = current_detail_df.copy() # <--- FIX: use current_detail_df
                        else:
                            combined_detail_data_for_plotting = pd.concat([combined_detail_data_for_plotting, current_detail_df], ignore_index=True)         

                except Exception as err:
                    with open(skipped_runs_log_path, 'a') as log_file:
                        log_file.write(f"Skipped run {run_info['run_dir_path']} due to error: {err}\n")
                    logging.error(f"Skipped run {run_info['run_dir_path']} due to error: {err}")

                pbar.update(1)

        summary_df = pd.DataFrame(summary_data)
        summary_df.sort_values(['initial_Z', 'initial_mass'], inplace=True)
        summary_df.set_index(['initial_Z', 'initial_mass'], inplace=True)
        summary_df.to_csv(summary_csv_path)
        logging.info(f"Summary CSV written to {summary_csv_path}")

        cross_data_matrix.to_csv(cross_csv_path)
        logging.info(f"Cross-grid CSV written to {cross_csv_path}")

        # This block is for writing detail CSVs to disk, separate from in-memory processing
        if analyze_blue_loop:
            # Re-define concise_detail_columns here to ensure it's available if this part is reached
            concise_detail_columns = [
                'initial_mass', 'initial_Z', 'star_age', 'model_number',
                'log_Teff', 'log_L', 'log_g', 'profile_number'
            ]
            for z_val, dfs_list in grouped_detailed_dfs_for_analysis_raw.items():
                if dfs_list:
                    try:
                        combined_df_bl = pd.concat(dfs_list, ignore_index=True)

                        if blue_loop_output_type == 'all':
                            df_to_save = combined_df_bl
                            output_type_label = "all columns"
                        else:
                            existing_desired_cols = [col for col in concise_detail_columns if col in combined_df_bl.columns]
                            if not existing_desired_cols:
                                logging.warning(f"No desired columns found for concise detail CSV for Z={z_val}. Skipping detail CSV write.")
                                continue
                            df_to_save = combined_df_bl[existing_desired_cols]
                            output_type_label = "selected columns"

                        detail_filename = os.path.join(detail_files_output_dir, f"detail_z{z_val:.4f}.csv")
                        df_to_save.to_csv(detail_filename, index=False)
                        logging.info(f"Written concatenated detail CSV for Z={z_val} with {output_type_label} to {detail_filename}")

                    except Exception as e:
                        logging.error(f"Error writing detail CSV for Z={z_val}: {e}")
                else:
                    logging.info(f"No detailed data to write for Z={z_val}.")
    
    return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting
