import os
import pandas as pd
import numpy as np
import yaml
import logging
from tqdm import tqdm

# Import necessary functions from other modules
from .data_reader import scan_mesa_runs, get_data_from_history_file
from .bluelooptools.blue_loop_analyzer import analyze_blue_loop_and_instability


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
            - pd.DataFrame: A combined DataFrame of detailed blue loop data for plotting
                            (combined_detail_data_for_plotting), filtered for the blue loop analysis.
            - dict: A dictionary where keys are metallicities (Z) and values are
                    lists of **full, untrimmed** history DataFrames for plotting (full_history_data_for_plotting).
                    Returns empty Dataframes/dicts if analysis cannot be performed or is skipped.
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
    combined_detail_data_for_plotting = pd.DataFrame() # This will hold the combined data for plotting
    full_history_data_for_plotting = {} # Initialized here

    if not reanalysis_needed:
        logging.info("Summary and cross-grid CSV files already exist. Attempting to load existing data.")
        try:
            # We will load, filter, and then return. If filtering results in empty DF, it means no valid loops.
            loaded_summary_df = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])
            
            # --- Load-time Filtering ---
            # Filter out rows where 'blue_loop_crossing_count' is NaN or 0, when loading existing data
            filtered_loaded_summary_df = loaded_summary_df[
                (loaded_summary_df['blue_loop_crossing_count'].notna()) &
                (loaded_summary_df['blue_loop_crossing_count'] > 0)
            ].copy()
            # --- End Load-time Filtering ---

            if filtered_loaded_summary_df.empty and not loaded_summary_df.empty:
                logging.warning("Loaded summary CSV contained no valid blue loop entries after filtering. Forcing reanalysis.")
                reanalysis_needed = True
            elif filtered_loaded_summary_df.empty and loaded_summary_df.empty:
                logging.info("Loaded summary CSV was empty. No valid blue loop entries.")
                # No reanalysis needed if it was already empty and nothing valid.
                return pd.DataFrame(), pd.DataFrame(), {} # Return empty DataFrames if nothing to analyze
            else:
                summary_df = filtered_loaded_summary_df
                logging.info("Successfully loaded and filtered existing summary CSV.")
                # If not reanalyzing, we don't regenerate detailed data in memory for plotting
                # So, combined_detail_data_for_plotting and full_history_data_for_plotting remain empty/as initialized.
                return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting 
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
                run_dir_path = run_info['run_dir_path'] # Extract the full run directory path

                analysis_result_summary = {
                    'initial_mass': current_mass, 
                    'initial_Z': current_z, 
                    'run_dir_path': run_dir_path, # NEW: Add the full run directory path
                    'blue_loop_crossing_count': np.nan,
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

                    filtered_combined_df_bl = pd.DataFrame() # Initialize here

                    if analyze_blue_loop:
                        analyzer_output = analyze_blue_loop_and_instability(df_full_history, current_mass, current_z)

                        # Determine if analysis was successful or found a 0-crossing loop
                        if pd.notna(analyzer_output['crossing_count']): # This means it's either 0 or >0, not a fundamental error NaN
                            analysis_result_summary['blue_loop_crossing_count'] = int(analyzer_output['crossing_count'])
                            
                            if analysis_result_summary['blue_loop_crossing_count'] > 0: # Only populate detailed metrics if a loop was actually found
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
                                
                                # Populate detailed metrics if blue_loop_output_type is 'all'
                                if not analyzer_output['blue_loop_detail_df'].empty:
                                    bl_df = analyzer_output['blue_loop_detail_df'].copy()
                                    if blue_loop_output_type == 'all':
                                        filtered_combined_df_bl = bl_df 
                                        analysis_result_summary['max_log_L'] = bl_df['log_L'].max()
                                        analysis_result_summary['max_log_Teff'] = bl_df['log_Teff'].max()
                                        if 'log_R' in bl_df.columns:
                                            analysis_result_summary['max_log_R'] = bl_df['log_R'].max()
                                        elif 'log_R' in df_full_history.columns: # Fallback to full history if log_R not in detail_df
                                            analysis_result_summary['max_log_R'] = df_full_history['log_R'].max()
                                        analysis_result_summary['first_model_number'] = bl_df['model_number'].min()
                                        analysis_result_summary['last_model_number'] = bl_df['model_number'].max()
                                        analysis_result_summary['first_age_yr'] = bl_df['star_age'].min()
                                        analysis_result_summary['last_age_yr'] = bl_df['star_age'].max()
                                    else: # 'summary' output type for blue loop details
                                        # When blue_loop_output_type is 'summary', we still need to populate 
                                        # filtered_combined_df_bl with concise columns for plotting purposes,
                                        # even if the summary CSV does not contain the detailed metrics.
                                        concise_detail_columns_local = [ 
                                            'initial_mass', 'initial_Z', 'star_age', 'model_number',
                                            'log_Teff', 'log_L', 'log_g', 'profile_number'
                                        ]
                                        existing_desired_cols = [col for col in concise_detail_columns_local if col in bl_df.columns]
                                        if existing_desired_cols:
                                            filtered_combined_df_bl = bl_df[existing_desired_cols]
                                        else:
                                            logging.warning(f"No desired columns found for concise detail for M={current_mass}, Z={current_z}. Detail DF for plotting might remain empty.")
                                            filtered_combined_df_bl = pd.DataFrame() # Ensure it's empty if no columns found
                                        
                                    current_detail_df = filtered_combined_df_bl 
                                else:
                                    # If blue_loop_detail_df is empty even if crossing_count > 0 (shouldn't happen often)
                                    logging.warning(f"Detail DataFrame empty despite blue loop found for M={current_mass}, Z={current_z}. Detailed metrics will be NaN.")
                                    current_detail_df = pd.DataFrame() # Explicitly set to empty DataFrame
                            else: # crossing_count is 0
                                pass # No specific action needed here as we will filter later
                        else: # analyzer_output['crossing_count'] is NaN, meaning fundamental error
                            pass # No specific action needed here as we will filter later

                    else: # analyze_blue_loop is False
                        logging.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.")
                        analysis_result_summary['blue_loop_crossing_count'] = np.nan # Ensure it's NaN if analysis is off
                        pass # This run will also be filtered out

                    # Append to summary_data regardless, then filter later
                    summary_data.append(analysis_result_summary)

                    if analyze_blue_loop and not current_detail_df.empty:
                        if current_z not in grouped_detailed_dfs_for_analysis_raw:
                            grouped_detailed_dfs_for_analysis_raw[current_z] = []
                        grouped_detailed_dfs_for_analysis_raw[current_z].append(current_detail_df)
                        
                        # Concatenate current_detail_df to combined_detail_data_for_plotting
                        if combined_detail_data_for_plotting.empty:
                            combined_detail_data_for_plotting = current_detail_df.copy()
                        else:
                            combined_detail_data_for_plotting = pd.concat([combined_detail_data_for_plotting, current_detail_df], ignore_index=True)         

                except Exception as err:
                    with open(skipped_runs_log_path, 'a') as log_file:
                        log_file.write(f"Skipped run {run_info['run_dir_path']} due to error: {err}\n")
                    logging.error(f"Skipped run {run_info['run_dir_path']} due to error: {err}")

                pbar.update(1)

        summary_df_raw = pd.DataFrame(summary_data)
        summary_df_raw.sort_values(['initial_Z', 'initial_mass'], inplace=True)
        summary_df_raw.set_index(['initial_Z', 'initial_mass'], inplace=True)

        # --- FINAL FILTERING BEFORE SAVING SUMMARY CSV ---
        # Keep only rows where blue_loop_crossing_count is NOT NaN AND is greater than 0
        summary_df_to_save = summary_df_raw[
            (summary_df_raw['blue_loop_crossing_count'].notna()) &
            (summary_df_raw['blue_loop_crossing_count'] > 0)
        ].copy()
        # --- END FINAL FILTERING ---

        # If after filtering, the summary_df_to_save is empty, set it to an empty DataFrame to avoid issues later
        if summary_df_to_save.empty:
            logging.info("No valid blue loop entries found after filtering for summary CSV.")
            summary_df = pd.DataFrame(columns=[
                'blue_loop_crossing_count', 'blue_loop_duration_yr',
                'blue_loop_start_age', 'blue_loop_end_age',
                'instability_start_age', 'instability_end_age',
                'calculated_blue_loop_duration', 'calculated_instability_duration',
                'run_dir_path' # Ensure run_dir_path is also in empty df for consistency
            ], index=pd.MultiIndex.from_tuples([], names=['initial_Z', 'initial_mass']))
        else:
            summary_df = summary_df_to_save.copy() # Assign filtered data to summary_df
            # --- Column Filtering based on blue_loop_output_type ---
            if blue_loop_output_type == 'summary':
                logging.info("Applying 'summary' output type filtering for summary CSV columns.")
                summary_columns_for_summary_output = [
                    'blue_loop_crossing_count', 'blue_loop_duration_yr',
                    'blue_loop_start_age', 'blue_loop_end_age',
                    'instability_start_age', 'instability_end_age',
                    'calculated_blue_loop_duration', 'calculated_instability_duration',
                    'run_dir_path' # Ensure run_dir_path is included in summary output
                ]
                # Filter for columns that actually exist in the DataFrame before selecting
                existing_summary_cols = [col for col in summary_columns_for_summary_output if col in summary_df.columns]
                summary_df = summary_df[existing_summary_cols].copy()
            # If blue_loop_output_type is 'all', summary_df remains as is (includes all populated columns)
            # --- End Column Filtering ---


        summary_df.to_csv(summary_csv_path, na_rep='NaN')
        logging.info(f"Summary CSV written to {summary_csv_path}")

        # The cross_data_matrix needs to include all original runs (with 0s and NaNs for heatmap)
        # So we use summary_df_raw to build it, then convert to numeric and reindex if needed
        cross_data_matrix = summary_df_raw['blue_loop_crossing_count'].unstack()
        if not cross_data_matrix.empty:
            cross_data_matrix.columns = pd.to_numeric(cross_data_matrix.columns, errors='coerce')
            cross_data_matrix.index = pd.to_numeric(cross_data_matrix.index, errors='coerce')
            cross_data_matrix = cross_data_matrix.reindex(index=sorted(cross_data_matrix.index.unique()), columns=sorted(cross_data_matrix.columns.unique()))
            # Ensure NaN for non-existent combinations that were not in summary_data_raw (e.g. if a run was never scanned)
            cross_data_matrix = cross_data_matrix.where(pd.notna(cross_data_matrix), np.nan)
        else:
            logging.warning("No data to build cross-grid matrix. It will be empty.")
            cross_data_matrix = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses) # Ensure empty structure if no data

        cross_data_matrix.to_csv(cross_csv_path, na_rep='NaN')
        logging.info(f"Cross-grid CSV written to {cross_csv_path}")

        # This block is for writing detail CSVs to disk, separate from in-memory processing
        if analyze_blue_loop:
            # Define concise_detail_columns_for_saving (used for detail CSVs when blue_loop_output_type='summary')
            concise_detail_columns_for_saving = [ 
                'initial_mass', 'initial_Z', 'star_age', 'model_number',
                'log_Teff', 'log_L', 'log_g', 'profile_number'
            ]
            for z_val, dfs_list in grouped_detailed_dfs_for_analysis_raw.items():
                if dfs_list:
                    try:
                        combined_df_bl = pd.concat(dfs_list, ignore_index=True)
                        # Explicitly sort by initial_mass and star_age
                        combined_df_bl = combined_df_bl.sort_values(by=['initial_mass', 'star_age']).reset_index(drop=True)

                        if blue_loop_output_type == 'all':
                            df_to_save = combined_df_bl
                            output_type_label = "all columns"
                        else: # blue_loop_output_type is 'summary' for saving detailed CSVs
                            existing_desired_cols = [col for col in concise_detail_columns_for_saving if col in combined_df_bl.columns]
                            if not existing_desired_cols:
                                logging.warning(f"No desired columns found for concise detail CSV for Z={z_val}. Skipping detail CSV write.")
                                continue
                            df_to_save = combined_df_bl[existing_desired_cols]
                            output_type_label = "selected columns"

                        detail_filename = os.path.join(detail_files_output_dir, f"detail_z{z_val:.4f}.csv") 
                        df_to_save.to_csv(detail_filename, index=False, na_rep='NaN')
                        logging.info(f"Written concatenated detail CSV for Z={z_val} with {output_type_label} to {detail_filename}")

                    except Exception as e:
                        logging.error(f"Error writing detail CSV for Z={z_val}: {e}")
                else:
                    logging.info(f"No detailed data to write for Z={z_val}.")
            
    # Ensure combined_detail_data_for_plotting is also sorted consistently for the return value
    if not combined_detail_data_for_plotting.empty:
        combined_detail_data_for_plotting = combined_detail_data_for_plotting.sort_values(by=['initial_Z', 'initial_mass', 'star_age']).reset_index(drop=True)

    # --- START NEW LOGIC FOR MIN/MAX MODEL NUMBER CSV ---
    # This section processes the combined detailed blue loop data to generate a summary
    # of min/max model numbers for each initial_mass and initial_Z combination.
    # It now also includes the 'run_dir_path'.
    if not combined_detail_data_for_plotting.empty and analyze_blue_loop:
        logging.info("Generating sorted min/max model number CSV...")
        try:
            # Create a copy to work with, to avoid potential SettingWithCopyWarning
            temp_df_for_min_max = combined_detail_data_for_plotting.copy()

            # Ensure run_dir_path is added to temp_df_for_min_max from the original summary_df_raw
            # We need to merge or map the run_dir_path based on initial_mass and initial_Z
            # Since summary_df_raw already contains run_dir_path indexed by initial_Z, initial_mass,
            # we can reset index and merge.
            
            # Get the unique initial_mass, initial_Z, run_dir_path combinations from summary_df_raw
            # This is safer than relying on columns that might not always be there in combined_detail_data_for_plotting
            run_paths_info = summary_df_raw[['run_dir_path']].reset_index()

            # Merge this information into combined_detail_data_for_plotting
            # We need to ensure 'initial_mass' and 'initial_Z' are in combined_detail_data_for_plotting for merging
            if 'initial_mass' in temp_df_for_min_max.columns and 'initial_Z' in temp_df_for_min_max.columns:
                temp_df_for_min_max = pd.merge(
                    temp_df_for_min_max,
                    run_paths_info,
                    on=['initial_mass', 'initial_Z'],
                    how='left'
                )
            else:
                logging.warning("Cannot merge 'run_dir_path' into min/max model number CSV as 'initial_mass' or 'initial_Z' are missing in detailed data.")

            # Sort by initial_mass and initial_Z (important for consistent grouping)
            temp_df_for_min_max_sorted = temp_df_for_min_max.sort_values(
                by=['initial_mass', 'initial_Z'], ascending=[True, True]
            )

            # Group by initial_mass, initial_Z, AND run_dir_path to find min and max model_number
            # Grouping by run_dir_path ensures that if for some reason two identical mass/Z
            # have different run paths, they are treated separately.
            result_df = temp_df_for_min_max_sorted.groupby(
                ['initial_mass', 'initial_Z', 'run_dir_path'], as_index=False
            ).agg(
                min_model_number=('model_number', 'min'),
                max_model_number=('model_number', 'max')
            )

            # Format the initial_Z column to 4 decimal places for the output CSV, as a string
            result_df['initial_Z'] = result_df['initial_Z'].apply(lambda x: f"{x:.4f}")

            # Save the result to a CSV file in the analysis_results_sub_dir
            # This CSV will now contain the run_dir_path
            output_filename = os.path.join(analysis_results_sub_dir, "sorted_mass_Z_min_max.csv")
            result_df.to_csv(output_filename, index=False, na_rep='NaN')
            logging.info(f"Sorted min/max model number data (including run_dir_path) saved to: {output_filename}")

        except Exception as e:
            logging.error(f"Error generating sorted min/max model number CSV: {e}")
    elif analyze_blue_loop and combined_detail_data_for_plotting.empty:
        logging.info("Skipping sorted min/max model number CSV generation: No detailed blue loop data available or blue loop analysis yielded no loops.")
    else:
        logging.info("Skipping sorted min/max model number CSV generation: Blue loop analysis was not enabled for detailed data collection.")

    # --- END NEW LOGIC ---

    # Ensure combined_detail_data_for_plotting is also sorted consistently for the return value
    if not combined_detail_data_for_plotting.empty:
        combined_detail_data_for_plotting = combined_detail_data_for_plotting.sort_values(by=['initial_Z', 'initial_mass', 'star_age']).reset_index(drop=True)


    return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting
