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
                            (combined_detail_data_for_plotting).
            - dict: A dictionary where keys are metallicities (Z) and values are
                    lists of full, untrimmed history DataFrames for plotting (full_history_data_for_plotting).
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
    gyre_input_csv_path = os.path.join(analysis_results_sub_dir, gyre_input_csv_name)

    # Determine if reanalysis is needed for any reason (force_reanalysis, missing main CSVs, or missing GYRE CSV if workflow is active)
    reanalysis_needed = force_reanalysis or \
                        not os.path.exists(summary_csv_path) or \
                        not os.path.exists(cross_csv_path) or \
                        (args.run_gyre_workflow and not os.path.exists(gyre_input_csv_path))
    
    # Check if detail CSVs exist. If not, and blue loop analysis is on, force reanalysis
    # even if summary and cross CSVs exist. This is the crucial part that was missing.
    detail_csvs_exist = True
    if analyze_blue_loop:
        # Check if at least one detail CSV exists. If not, we need to re-run to generate them.
        # This is a heuristic; a more robust check might verify all expected Z detail CSVs.
        # For simplicity, we check if the detail_files_output_dir exists and is not empty.
        if not os.path.exists(detail_files_output_dir) or not os.listdir(detail_files_output_dir):
            detail_csvs_exist = False
            if not force_reanalysis: # If not already forced, force it due to missing detail CSVs
                logging.info(f"Detailed blue loop CSVs not found in '{detail_files_output_dir}'. Forcing reanalysis to generate them for plotting.")
                reanalysis_needed = True

    logging.info(f"Analysis started. Reanalysis needed: {reanalysis_needed}")
    if not os.path.exists(gyre_input_csv_path) and args.run_gyre_workflow and not force_reanalysis:
        logging.info(f"GYRE input CSV '{gyre_input_csv_name}' not found. Forcing reanalysis to generate it for GYRE workflow.")

    summary_df = pd.DataFrame()
    combined_detail_data_for_plotting = pd.DataFrame()
    full_history_data_for_plotting = {}
    gyre_output_csv_path_returned = ""

    if not reanalysis_needed:
        logging.info("Summary and cross-grid CSV files already exist. Attempting to load existing data.")
        try:
            loaded_summary_df = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])

            # --- Load-time Filtering ---
            if analyze_blue_loop:
                filtered_loaded_summary_df = loaded_summary_df[
                    (loaded_summary_df['blue_loop_crossing_count'].notna()) &
                    (loaded_summary_df['blue_loop_crossing_count'] > 0)
                ].copy()
            else:
                filtered_loaded_summary_df = loaded_summary_df.copy()
                logging.info(f"Blue loop analysis is OFF. All entries from '{summary_csv_path}' will be considered for GYRE input.")

            if filtered_loaded_summary_df.empty and not loaded_summary_df.empty:
                logging.warning("Loaded summary CSV contained no valid blue loop entries after filtering. Forcing reanalysis if blue loop analysis is on.")
                if analyze_blue_loop:
                    reanalysis_needed = True
                else:
                    logging.info("No successful MESA runs found in loaded summary CSV. Cannot generate GYRE input.")
                    return pd.DataFrame(), pd.DataFrame(), {}, ""
            elif filtered_loaded_summary_df.empty and loaded_summary_df.empty:
                logging.info("Loaded summary CSV was empty. No valid entries found.")
                return pd.DataFrame(), pd.DataFrame(), {}, ""
            else:
                summary_df = filtered_loaded_summary_df
                logging.info("Successfully loaded and filtered existing summary CSV.")
                
                if os.path.exists(gyre_input_csv_path) and not force_reanalysis:
                    logging.info(f"Existing GYRE input CSV '{gyre_input_csv_name}' found. Using it.")
                    gyre_output_csv_path_returned = gyre_input_csv_path
                else:
                    logging.info(f"Existing GYRE input CSV '{gyre_input_csv_name}' not found or reanalysis forced. Will generate it.")
                    reanalysis_needed = True

                # If not reanalyzing, we need to explicitly load detail data for plotting
                # and history data for HR diagrams if they exist on disk.
                if not reanalysis_needed:
                    logging.info("Detail data not in memory; attempting to load from disk for plotting...")
                    # Load combined_detail_data_for_plotting
                    combined_detail_dfs = []
                    if os.path.exists(detail_files_output_dir):
                        logging.info(f"Loading CSV files from '{detail_files_output_dir}'...")
                        for f_name in os.listdir(detail_files_output_dir):
                            if f_name.endswith(".csv"):
                                try:
                                    df = pd.read_csv(os.path.join(detail_files_output_dir, f_name))
                                    combined_detail_dfs.append(df)
                                except Exception as e:
                                    logging.warning(f"Failed to load detail CSV '{f_name}': {e}")
                        if combined_detail_dfs:
                            combined_detail_data_for_plotting = pd.concat(combined_detail_dfs, ignore_index=True)
                            logging.info(f"Successfully loaded {len(combined_detail_dfs)} detail CSVs.")
                            # Ensure sorting for consistency
                            combined_detail_data_for_plotting = combined_detail_data_for_plotting.sort_values(
                                by=['initial_Z', 'initial_mass', 'star_age']
                            ).reset_index(drop=True)
                        else:
                            logging.error(f"No CSV files loaded from '{detail_files_output_dir}'.")
                    else:
                        logging.error(f"Detail files output directory '{detail_files_output_dir}' does not exist.")

                    # Load full_history_data_for_plotting (for HR diagrams)
                    # This would require saving history dataframes as pickles or separate CSVs per run
                    # during the initial analysis if not already done.
                    # For now, we assume if reanalysis is not needed, this data might not be present.
                    # This part needs custom implementation if full history data is large and not kept in memory.
                    # Current setup generates HRDs during reanalysis, not by loading.
                    # So, if not reanalyzing, full_history_data_for_plotting remains empty, leading to HRD warning.
                    # This is intentional based on the previous output.
                    pass # Placeholder for future history data loading logic if necessary

                    if not reanalysis_needed:
                        return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting, gyre_output_csv_path_returned

        except FileNotFoundError:
            logging.warning(f"Existing summary or cross-grid CSVs not found. Forcing full reanalysis.")
            reanalysis_needed = True
        except Exception as e:
            logging.error(f"Error loading existing summary CSV: {e}. Forcing full reanalysis.")
            logging.exception("Error details:")
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
        # Stores detailed DataFrames grouped by Z for individual file saving
        grouped_detailed_dfs_for_analysis_raw = {z_val: [] for z_val in unique_zs}
        full_history_data_for_plotting = {z_val: [] for z_val in unique_zs}

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
                run_dir_path = run_info['run_dir_path']

                analysis_result_summary = {
                    'initial_mass': current_mass,
                    'initial_Z': current_z,
                    'run_dir_path': run_dir_path,
                    'blue_loop_crossing_count': np.nan,
                    'blue_loop_duration_yr': np.nan, 'max_log_L': np.nan, 'max_log_Teff': np.nan,
                    'max_log_R': np.nan, 'first_model_number': np.nan, 'last_model_number': np.nan,
                    'first_age_yr': np.nan, 'last_age_yr': np.nan, 'blue_loop_start_age': np.nan,
                    'blue_loop_end_age': np.nan, 'instability_start_age': np.nan,
                    'instability_end_age': np.nan, 'calculated_blue_loop_duration': np.nan,
                    'calculated_instability_duration': np.nan
                    }
                
                # Initialize current_detail_df for the current run. Will be populated if blue loop analysis is active.
                current_detail_df = pd.DataFrame() 

                try:
                    df_full_history = get_data_from_history_file(history_file_path)
                    df_full_history['initial_mass'] = current_mass
                    df_full_history['initial_Z'] = current_z

                    # Store full history data for plotting (e.g., HR diagrams)
                    if current_z not in full_history_data_for_plotting:
                        full_history_data_for_plotting[current_z] = []
                    full_history_data_for_plotting[current_z].append(df_full_history.copy())

                    if analyze_blue_loop:
                        analyzer_output = analyze_blue_loop_and_instability(df_full_history, current_mass, current_z)

                        # Check if blue_loop_detail_df was successfully created, regardless of crossing count
                        if not analyzer_output['blue_loop_detail_df'].empty:
                            bl_df = analyzer_output['blue_loop_detail_df'].copy()
                            # Filter columns based on blue_loop_output_type
                            if blue_loop_output_type == 'all':
                                current_detail_df = bl_df # Keep all columns from the detail DataFrame
                            else: # 'summary' output type, only keep specific columns for detailed CSVs
                                concise_detail_columns_local = [
                                    'initial_mass', 'initial_Z', 'star_age', 'model_number',
                                    'log_Teff', 'log_L', 'log_g', 'profile_number'
                                ]
                                existing_desired_cols = [col for col in concise_detail_columns_local if col in bl_df.columns]
                                if existing_desired_cols:
                                    current_detail_df = bl_df[existing_desired_cols]
                                else:
                                    logging.warning(f"No desired columns found for concise detail for M={current_mass}, Z={current_z}. Detail DF for plotting might remain empty.")
                                    current_detail_df = pd.DataFrame()
                        else:
                            logging.info(f"analyzer_output['blue_loop_detail_df'] was empty for M={current_mass}, Z={current_z}. No detailed data for this run.")
                            # current_detail_df remains an empty DataFrame as initialized or explicitly set here.

                        # Now, populate analysis_result_summary based on blue loop crossing count
                        if pd.notna(analyzer_output['crossing_count']):
                            analysis_result_summary['blue_loop_crossing_count'] = int(analyzer_output['crossing_count'])

                            if analysis_result_summary['blue_loop_crossing_count'] > 0: # Only populate detailed summary metrics if a loop was actually found
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

                                # Populate other detailed summary metrics from current_detail_df if it has data
                                if not current_detail_df.empty:
                                    analysis_result_summary['max_log_L'] = current_detail_df['log_L'].max()
                                    analysis_result_summary['max_log_Teff'] = current_detail_df['log_Teff'].max()
                                    if 'log_R' in current_detail_df.columns:
                                        analysis_result_summary['max_log_R'] = current_detail_df['log_R'].max()
                                    elif 'log_R' in df_full_history.columns: # Fallback to full history if log_R not in detail_df
                                        analysis_result_summary['max_log_R'] = df_full_history['log_R'].max()
                                    analysis_result_summary['first_model_number'] = current_detail_df['model_number'].min()
                                    analysis_result_summary['last_model_number'] = current_detail_df['model_number'].max()
                                    analysis_result_summary['first_age_yr'] = current_detail_df['star_age'].min()
                                    analysis_result_summary['last_age_yr'] = current_detail_df['star_age'].max()
                                else:
                                    logging.warning(f"current_detail_df is empty for M={current_mass}, Z={current_z} despite blue loop found (count > 0). Detailed summary metrics will be NaN.")
                            else: # crossing_count is 0
                                logging.info(f"No blue loop found (0 crossings) for M={current_mass}, Z={current_z}. Blue loop summary metrics will be NaN.")
                        else: # analyzer_output['crossing_count'] is NaN, meaning fundamental error in analysis
                            logging.warning(f"Blue loop analysis failed for M={current_mass}, Z={current_z}. Blue loop summary metrics will be NaN.")
                            current_detail_df = pd.DataFrame() # Ensure empty if analysis failed entirely
                    else: # analyze_blue_loop is False
                        logging.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.")
                        analysis_result_summary['blue_loop_crossing_count'] = np.nan
                        current_detail_df = pd.DataFrame() # Explicitly empty if blue loop analysis is off

                    # Append to summary_data regardless of blue loop analysis success, then filter later
                    summary_data.append(analysis_result_summary)

                    # Only add to grouped_detailed_dfs_for_analysis_raw and combined_detail_data_for_plotting
                    # if blue loop analysis is active AND current_detail_df is not empty.
                    if analyze_blue_loop and not current_detail_df.empty:
                        if current_z not in grouped_detailed_dfs_for_analysis_raw:
                            grouped_detailed_dfs_for_analysis_raw[current_z] = []
                        grouped_detailed_dfs_for_analysis_raw[current_z].append(current_detail_df)

                        if combined_detail_data_for_plotting.empty:
                            combined_detail_data_for_plotting = current_detail_df.copy()
                        else:
                            combined_detail_data_for_plotting = pd.concat([combined_detail_data_for_plotting, current_detail_df], ignore_index=True)

                except Exception as err:
                    with open(skipped_runs_log_path, 'a') as log_file:
                        log_file.write(f"Skipped run {run_info['run_dir_path']} due to error: {err}\n")
                    logging.error(f"Skipped run {run_info['run_dir_path']} due to error: {err}")
                    logging.exception(f"Exception details for run {run_info['run_dir_path']}:")

                pbar.update(1)

        summary_df_raw = pd.DataFrame(summary_data)
        summary_df_raw.sort_values(['initial_mass', 'initial_Z'], inplace=True)
        summary_df_raw.set_index(['initial_Z', 'initial_mass'], inplace=True)

        # --- FINAL FILTERING BEFORE SAVING SUMMARY CSV ---
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

        if summary_df_to_save.empty:
            summary_df = pd.DataFrame(columns=[
                'initial_mass', 'initial_Z', 'run_dir_path',
                'blue_loop_crossing_count', 'blue_loop_duration_yr',
                'blue_loop_start_age', 'blue_loop_end_age',
                'instability_start_age', 'instability_end_age',
                'calculated_blue_loop_duration', 'calculated_instability_duration',
                'max_log_L', 'max_log_Teff', 'max_log_R',
                'first_model_number', 'last_model_number',
                'first_age_yr', 'last_age_yr'
            ], index=pd.MultiIndex.from_tuples([], names=['initial_Z', 'initial_mass']))
        else:
            summary_df = summary_df_to_save.copy()
            if blue_loop_output_type == 'summary' and analyze_blue_loop:
                logging.info("Applying 'summary' output type filtering for summary CSV columns.")
                summary_columns_for_summary_output = [
                    'blue_loop_crossing_count', 'blue_loop_duration_yr',
                    'blue_loop_start_age', 'blue_loop_end_age',
                    'instability_start_age', 'instability_end_age',
                    'calculated_blue_loop_duration', 'calculated_instability_duration',
                    'run_dir_path'
                ]
                existing_summary_cols = [col for col in summary_columns_for_summary_output if col in summary_df.columns]
                summary_df = summary_df[existing_summary_cols].copy()

        summary_df.to_csv(summary_csv_path, na_rep='NaN')
        logging.info(f"Summary CSV written to {summary_csv_path}")

        cross_data_matrix = summary_df_raw['blue_loop_crossing_count'].unstack()
        if not cross_data_matrix.empty:
            cross_data_matrix.columns = pd.to_numeric(cross_data_matrix.columns, errors='coerce')
            cross_data_matrix.index = pd.to_numeric(cross_data_matrix.index, errors='coerce')
            cross_data_matrix = cross_data_matrix.reindex(index=sorted(cross_data_matrix.index.unique()), columns=sorted(cross_data_matrix.columns.unique()))
            cross_data_matrix = cross_data_matrix.where(pd.notna(cross_data_matrix), np.nan)
        else:
            logging.warning("No data to build cross-grid matrix. It will be empty.")
            cross_data_matrix = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses)

        cross_data_matrix.to_csv(cross_csv_path, na_rep='NaN')
        logging.info(f"Cross-grid CSV written to {cross_csv_path}")

        # This block is for writing detail CSVs to disk (per Z value)
        if analyze_blue_loop: # Only save detail CSVs if blue loop analysis was performed
            os.makedirs(detail_files_output_dir, exist_ok=True) # Ensure output directory exists
            concise_detail_columns_for_saving = [
                'initial_mass', 'initial_Z', 'star_age', 'model_number',
                'log_Teff', 'log_L', 'log_g', 'profile_number'
            ]
            for z_val, dfs_list in grouped_detailed_dfs_for_analysis_raw.items():
                if dfs_list:
                    try:
                        combined_df_bl = pd.concat(dfs_list, ignore_index=True)
                        combined_df_bl = combined_df_bl.sort_values(by=['initial_mass', 'star_age']).reset_index(drop=True)

                        if blue_loop_output_type == 'all':
                            df_to_save = combined_df_bl
                            output_type_label = "all columns"
                        else:
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

    # --- START LOGIC FOR MIN/MAX MODEL NUMBER CSV (GYRE INPUT) ---
    gyre_input_df = pd.DataFrame()
    if not summary_df_raw.empty:
        logging.info(f"Generating GYRE input CSV '{gyre_input_csv_name}' from MESA summary data...")
        try:
            df_for_gyre_csv = summary_df_raw.reset_index().copy()

            if 'first_model_number' not in df_for_gyre_csv.columns or 'last_model_number' not in df_for_gyre_csv.columns:
                logging.warning("'first_model_number' or 'last_model_number' not found in summary_df_raw. GYRE input CSV will only contain 'run_dir_path', 'initial_mass', 'initial_Z'.")
                gyre_input_df = df_for_gyre_csv[['initial_mass', 'initial_Z', 'run_dir_path']].copy()
                gyre_input_df['min_model_number'] = np.nan
                gyre_input_df['max_model_number'] = np.nan
            else:
                gyre_input_df = df_for_gyre_csv[['initial_mass', 'initial_Z', 'run_dir_path', 'first_model_number', 'last_model_number']].copy()
            
            gyre_input_df.rename(columns={
                'run_dir_path': 'mesa_run_directory',
                'first_model_number': 'min_model_number',
                'last_model_number': 'max_model_number'
            }, inplace=True)

            gyre_input_df['initial_Z'] = gyre_input_df['initial_Z'].apply(lambda x: f"{x:.4f}")
            gyre_input_df.sort_values(['initial_mass', 'initial_Z'], inplace=True)
            
            gyre_input_df.to_csv(gyre_input_csv_path, index=False, na_rep='NaN')
            logging.info(f"GYRE input CSV saved to: {gyre_input_csv_path}")
            gyre_output_csv_path_returned = gyre_input_csv_path

        except Exception as e:
            logging.error(f"Error generating GYRE input CSV '{gyre_input_csv_name}': {e}")
            logging.exception("GYRE input CSV generation exception details:")
            gyre_output_csv_path_returned = ""

    else:
        logging.info("Skipping GYRE input CSV generation: No successful MESA runs found in summary data.")
        gyre_output_csv_path_returned = ""
    # --- END LOGIC FOR MIN/MAX MODEL NUMBER CSV ---

    return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting, gyre_output_csv_path_returned
