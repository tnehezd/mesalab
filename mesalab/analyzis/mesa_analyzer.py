# mesalab/analyzis/mesa_analyzer.py

import os
import pandas as pd
import numpy as np
import yaml
import logging
from tqdm import tqdm

# Import necessary functions from other modules
from .data_reader import scan_mesa_runs, get_data_from_history_file
from ..bluelooptools.blue_loop_analyzer import analyze_blue_loop_and_instability


def perform_mesa_analysis(args, analysis_results_sub_dir, detail_files_output_dir, gyre_input_csv_name: str = 'sorted_mass_Z_min_max.csv'):
    """
    Coordinates the analysis of MESA runs, including blue loop analysis,
    data aggregation, and saving summary and detailed results.

    Args:
        args (argparse.Namespace): Command-line arguments containing input_dir,
                                   inlist_name, analyze_blue_loop, blue_loop_output_type,
                                   force_reanalysis.
        analysis_results_sub_dir (str): Path to the directory for summary/cross-grid CSVs.
        detail_files_output_dir (str): Path to the directory for detailed blue loop CSVs.
        gyre_input_csv_name (str): The desired filename for the CSV containing profiles
                                   information for the GYRE workflow. Defaults to 'sorted_mass_Z_min_max.csv'.

    Returns:
        tuple: A tuple containing:
            - pd.DataFrame: The main summary DataFrame of analysis results.
            - pd.DataFrame: A combined DataFrame of detailed blue loop data for plotting
                            (combined_detail_data_for_plotting), filtered for the blue loop analysis.
            - dict: A dictionary where keys are metallicities (Z) and values are
                    lists of **full, untrimmed** history DataFrames for plotting (full_history_data_for_plotting).
            - str: The full path to the generated GYRE input CSV file. Returns an empty string
                   if the CSV was not generated.
    """
    input_dir = args.input_dir
    inlist_name = args.inlist_name
    analyze_blue_loop = args.analyze_blue_loop
    blue_loop_output_type = args.blue_loop_output_type
    force_reanalysis = args.force_reanalysis

    summary_csv_path = os.path.join(analysis_results_sub_dir, "summary_results.csv")
    cross_csv_path = os.path.join(analysis_results_sub_dir, "crossing_count_grid.csv")
    gyre_input_csv_path = os.path.join(analysis_results_sub_dir, gyre_input_csv_name) # Define GYRE CSV path

    reanalysis_needed = force_reanalysis or \
                        not os.path.exists(summary_csv_path) or \
                        not os.path.exists(cross_csv_path) or \
                        (args.run_gyre_workflow and not os.path.exists(gyre_input_csv_path)) # Added check for GYRE CSV

    logging.info(f"Analysis started. Reanalysis needed: {reanalysis_needed}")
    if not os.path.exists(gyre_input_csv_path) and args.run_gyre_workflow and not force_reanalysis:
        logging.info(f"GYRE input CSV '{gyre_input_csv_name}' not found. Forcing reanalysis to generate it for GYRE workflow.")

    summary_df = pd.DataFrame()
    combined_detail_data_for_plotting = pd.DataFrame() # This will hold the combined data for plotting
    full_history_data_for_plotting = {} # Initialized here
    gyre_output_csv_path_returned = "" # Initialize the path to return

    if not reanalysis_needed:
        logging.info("Summary and cross-grid CSV files already exist. Attempting to load existing data.")
        try:
            loaded_summary_df = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])
            
            # --- Load-time Filtering ---
            # Filter out rows where 'blue_loop_crossing_count' is NaN or 0, when loading existing data
            # This logic should be applied only if analyze_blue_loop is True
            if analyze_blue_loop:
                filtered_loaded_summary_df = loaded_summary_df[
                    (loaded_summary_df['blue_loop_crossing_count'].notna()) &
                    (loaded_summary_df['blue_loop_crossing_count'] > 0)
                ].copy()
            else: # If analyze_blue_loop is False, we take all relevant entries (e.g., all successful runs)
                  # For GYRE, we might need all successful runs, regardless of blue loop.
                  # Assume all loaded runs are potentially valid for GYRE if no blue loop analysis is done.
                filtered_loaded_summary_df = loaded_summary_df.copy()
                logging.info(f"Blue loop analysis is OFF. All entries from '{summary_csv_path}' will be considered for GYRE input.")

            if filtered_loaded_summary_df.empty and not loaded_summary_df.empty:
                logging.warning("Loaded summary CSV contained no valid blue loop entries after filtering. Forcing reanalysis if blue loop analysis is on.")
                if analyze_blue_loop: # Only force reanalysis if blue loop analysis is active and no loops found
                    reanalysis_needed = True
                else: # If analyze_blue_loop is false, an empty filtered_loaded_summary_df means no successful runs for GYRE
                    logging.info("No successful MESA runs found in loaded summary CSV. Cannot generate GYRE input.")
                    return pd.DataFrame(), pd.DataFrame(), {}, "" # Return empty DataFrames if nothing to analyze/process for GYRE
            elif filtered_loaded_summary_df.empty and loaded_summary_df.empty:
                logging.info("Loaded summary CSV was empty. No valid entries found.")
                return pd.DataFrame(), pd.DataFrame(), {}, "" # Return empty DataFrames if nothing to analyze
            else:
                summary_df = filtered_loaded_summary_df
                logging.info("Successfully loaded and filtered existing summary CSV.")
                # We also need to check if the GYRE input CSV exists and is up-to-date
                if os.path.exists(gyre_input_csv_path) and not force_reanalysis:
                    logging.info(f"Existing GYRE input CSV '{gyre_input_csv_name}' found. Using it.")
                    gyre_output_csv_path_returned = gyre_input_csv_path
                else:
                    logging.info(f"Existing GYRE input CSV '{gyre_input_csv_name}' not found or reanalysis forced. Will generate it.")
                    reanalysis_needed = True # Force reanalysis to regenerate the GYRE CSV

                # If not reanalyzing, we don't regenerate detailed data in memory for plotting
                # So, combined_detail_data_for_plotting and full_history_data_for_plotting remain empty/as initialized.
                if not reanalysis_needed: # Only return here if NO reanalysis is needed for anything
                     return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting, gyre_output_csv_path_returned
        except FileNotFoundError:
            logging.warning(f"Existing summary or cross-grid CSVs not found. Forcing full reanalysis.")
            reanalysis_needed = True
        except Exception as e:
            logging.error(f"Error loading existing summary CSV: {e}. Forcing full reanalysis.")
            logging.exception("Error details:") # Log full traceback
            reanalysis_needed = True

    if reanalysis_needed:
        logging.info("Starting full analysis of MESA runs...")
        
        mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)
        if not mesa_run_infos:
            logging.info("No MESA runs found for full analysis. Returning empty DataFrames.")
            return pd.DataFrame(), pd.DataFrame(), {}, ""

        unique_masses = sorted(set(run['mass'] for run in mesa_run_infos))
        unique_zs = sorted(set(run['z'] for run in mesa_run_infos))

        if not unique_masses or not unique_zs:
            logging.error("Error: Could not determine unique masses or metallicities from runs. Returning empty DataFrames.")
            return pd.DataFrame(), pd.DataFrame(), {}, ""

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
                                    logging.warning(f"Detail DataFrame empty despite blue loop found for M={current_mass}, Z={current_z}. Detailed metrics will be NaN.")
                                    current_detail_df = pd.DataFrame() # Explicitly set to empty DataFrame
                            else: # crossing_count is 0
                                logging.info(f"No blue loop found (0 crossings) for M={current_mass}, Z={current_z}. Skipping detailed blue loop data collection.")
                                pass 
                        else: # analyzer_output['crossing_count'] is NaN, meaning fundamental error
                            logging.warning(f"Blue loop analysis failed for M={current_mass}, Z={current_z}. Skipping detailed blue loop data collection.")
                            pass 

                    else: # analyze_blue_loop is False
                        logging.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.")
                        analysis_result_summary['blue_loop_crossing_count'] = np.nan # Ensure it's NaN if analysis is off
                        pass # This run will also be filtered out for blue loop specific plotting, but still summarized

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
                    logging.exception(f"Exception details for run {run_info['run_dir_path']}:") # Log full traceback

                pbar.update(1)

        summary_df_raw = pd.DataFrame(summary_data)
        summary_df_raw.sort_values(['initial_Z', 'initial_mass'], inplace=True)
        summary_df_raw.set_index(['initial_Z', 'initial_mass'], inplace=True)

        # --- FINAL FILTERING BEFORE SAVING SUMMARY CSV ---
        # If analyze_blue_loop is True, we only keep runs with >0 blue loop crossings
        # If analyze_blue_loop is False, we keep all runs (assuming they are valid for GYRE input)
        if analyze_blue_loop:
            summary_df_to_save = summary_df_raw[
                (summary_df_raw['blue_loop_crossing_count'].notna()) &
                (summary_df_raw['blue_loop_crossing_count'] > 0)
            ].copy()
            if summary_df_to_save.empty:
                logging.info("No valid blue loop entries found after filtering for summary CSV.")
            else:
                 logging.info(f"Generated summary CSV will contain {len(summary_df_to_save)} blue loop entries.")
        else:
            summary_df_to_save = summary_df_raw.copy()
            logging.info(f"Blue loop analysis is OFF. Generated summary CSV will contain all {len(summary_df_to_save)} successful MESA runs.")
        # --- END FINAL FILTERING ---

        # If after filtering, the summary_df_to_save is empty, set it to an empty DataFrame to avoid issues later
        if summary_df_to_save.empty:
            summary_df = pd.DataFrame(columns=[
                'initial_mass', 'initial_Z', 'run_dir_path', # Ensure these are present
                'blue_loop_crossing_count', 'blue_loop_duration_yr',
                'blue_loop_start_age', 'blue_loop_end_age',
                'instability_start_age', 'instability_end_age',
                'calculated_blue_loop_duration', 'calculated_instability_duration',
                'max_log_L', 'max_log_Teff', 'max_log_R',
                'first_model_number', 'last_model_number',
                'first_age_yr', 'last_age_yr'
            ], index=pd.MultiIndex.from_tuples([], names=['initial_Z', 'initial_mass']))
        else:
            summary_df = summary_df_to_save.copy() # Assign filtered data to summary_df
            # --- Column Filtering based on blue_loop_output_type ---
            if blue_loop_output_type == 'summary' and analyze_blue_loop: # Only apply if blue loop analysis was active
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
        if analyze_blue_loop: # Only save detail CSVs if blue loop analysis was performed
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

    # --- START NEW LOGIC FOR MIN/MAX MODEL NUMBER CSV (GYRE INPUT) ---
    # This section processes the combined detailed blue loop data or raw summary_df_raw
    # to generate a summary of min/max model numbers for each initial_mass and initial_Z combination.
    # It now also includes the 'run_dir_path'.
    # This CSV should ALWAYS be generated if analysis was successful, regardless of blue loop analysis status,
    # because GYRE needs a list of profiles for all valid runs.

    gyre_input_df = pd.DataFrame()
    if not summary_df_raw.empty: # Use the raw summary_df which contains all successful MESA runs
        logging.info(f"Generating GYRE input CSV '{gyre_input_csv_name}' from MESA summary data...")
        try:
            # We need initial_mass, initial_Z, run_dir_path.
            # And min_model_number, max_model_number for the profiles if available.
            # If blue loop analysis was ON, we get these from combined_detail_data_for_plotting.
            # If blue loop analysis was OFF, we should derive them from the full history files,
            # or simply use first/last model_number from the summary_df_raw if it contains them.

            # Let's standardize the columns needed for GYRE input.
            # GYRE needs to know the MESA run directory and the range of profile numbers.
            # The simplest is to give it the first and last profile number of the *entire run*
            # if no blue loop analysis was done, or the blue loop specific range if it was.

            # For now, let's assume 'run_dir_path', 'initial_mass', 'initial_Z' are fundamental.
            # And 'min_model_number', 'max_model_number' (or similar like 'first_model_number', 'last_model_number')
            # from the summary_df, which represents the entire run's profile range if blue_loop_analysis is False,
            # or the blue loop's range if True.

            # Use summary_df (which is already filtered for blue loops if analyze_blue_loop is True)
            # or summary_df_raw if analyze_blue_loop is False.
            
            df_for_gyre_csv = summary_df.reset_index().copy() # Use the filtered/processed summary_df
            
            # Select relevant columns for GYRE input
            # Ensure 'first_model_number' and 'last_model_number' are present.
            # If blue_loop_analysis is False, these columns might not be filled in analysis_result_summary.
            # In that case, we need to explicitly get min/max model_number from full history files or use 'profile_number' from summary_df_raw.
            
            if 'first_model_number' not in df_for_gyre_csv.columns or 'last_model_number' not in df_for_gyre_csv.columns:
                 # If these are missing (e.g., if analyze_blue_loop was False), we need to populate them.
                 # This means iterating through run_dir_path and finding min/max profile_number in their profile.data files.
                 # This is more complex and might be better handled directly within gyre_modules if it needs *all* profiles.
                 # For now, let's assume summary_df either has these or gyre_modules can work without them,
                 # or it processes the 'run_dir_path' directly to find all profiles.
                 
                 # Alternative: If 'profile_number' in summary_df_raw is the final profile, we could use that.
                 # For safety and consistency with GYRE's expectation of a *range*, let's add placeholder logic.
                 logging.warning("'first_model_number' or 'last_model_number' not found in summary_df. GYRE input CSV will only contain 'run_dir_path', 'initial_mass', 'initial_Z'.")
                 gyre_input_df = df_for_gyre_csv[['initial_mass', 'initial_Z', 'run_dir_path']].copy()
                 gyre_input_df.rename(columns={'run_dir_path': 'mesa_run_directory'}, inplace=True) # Renaming for GYRE's config
                 gyre_input_df['min_model_number'] = np.nan # Placeholder
                 gyre_input_df['max_model_number'] = np.nan # Placeholder
            else:
                 gyre_input_df = df_for_gyre_csv[['initial_mass', 'initial_Z', 'run_dir_path', 'first_model_number', 'last_model_number']].copy()
                 gyre_input_df.rename(columns={
                     'run_dir_path': 'mesa_run_directory',
                     'first_model_number': 'min_model_number',
                     'last_model_number': 'max_model_number'
                 }, inplace=True)
            
            # Ensure 'initial_Z' is formatted as string for consistency with GYRE expectations
            gyre_input_df['initial_Z'] = gyre_input_df['initial_Z'].apply(lambda x: f"{x:.4f}")

            gyre_input_df.to_csv(gyre_input_csv_path, index=False, na_rep='NaN')
            logging.info(f"GYRE input CSV saved to: {gyre_input_csv_path}")
            gyre_output_csv_path_returned = gyre_input_csv_path # Set the path to return

        except Exception as e:
            logging.error(f"Error generating GYRE input CSV '{gyre_input_csv_name}': {e}")
            logging.exception("GYRE input CSV generation exception details:")
            gyre_output_csv_path_returned = "" # Indicate failure to generate

    else:
        logging.info("Skipping GYRE input CSV generation: No successful MESA runs found in summary data.")
        gyre_output_csv_path_returned = "" # No CSV generated

    # --- END NEW LOGIC FOR MIN/MAX MODEL NUMBER CSV ---

    return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting, gyre_output_csv_path_returned