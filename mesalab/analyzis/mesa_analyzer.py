import os
import pandas as pd
import numpy as np
import yaml
import logging
from tqdm import tqdm
import sys

# --- CORRECTED IMPORTS ---
# Import the actual functions from their correct respective modules
from mesalab.analyzis.data_reader import scan_mesa_runs, get_data_from_history_file
from mesalab.bluelooptools.blue_loop_analyzer import analyze_blue_loop_and_instability
from mesalab.rsptools import generate_mesa_rsp_inlists
from mesalab.io.output_manager import create_output_directories
from mesalab.io.inlist_parser import get_mesa_params_from_inlist # NEW: Import to get initial_y

logger = logging.getLogger(__name__)

def perform_mesa_analysis(args, analysis_results_sub_dir, detail_files_output_dir,
                          gyre_input_csv_name: str = 'sorted_blue_loop_profiles.csv',
                          rsp_output_subdir: str = None):
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
                                   information for the GYRE workflow. Defaults to 'sorted_blue_loop_profiles.csv'.
        rsp_output_subdir (str, optional): Base directory where MESA RSP inlists should be saved.
                                                   Defaults to None, in which case the RSP inlists will be
                                                   generated relative to the original MESA run directories.

    Returns:
        tuple: A tuple containing:
        
            - pd.DataFrame: The main summary DataFrame of analysis results.
            - pd.DataFrame: A combined DataFrame of detailed blue loop data for plotting
                            (combined_detail_data_for_plotting).
            - dict: A dictionary where keys are metallicities (Z) and values are
                    lists of full, untrimmed history DataFrames for plotting (full_history_data_for_plotting).
            - str: The full path to the generated GYRE input CSV file. Returns an empty string
                    if the CSV was not generated.
            - list: A list of paths to the generated RSP inlist files.
    """
    # Directory creation logic is now handled by output_manager.create_output_directories
    # The `create_output_directories` function from `output_manager.py`
    # should be called in `cli.py` *before* calling `perform_mesa_analysis`.
    # This function `perform_mesa_analysis` then receives the *already created*
    # paths.

    skipped_runs_log_path = os.path.join(analysis_results_sub_dir, "skipped_runs_log.txt")

    # Clear old skipped runs log if reanalysis is forced
    if args.general_settings.force_reanalysis and os.path.exists(skipped_runs_log_path):
        try:
            os.remove(skipped_runs_log_path)
            logger.info(f"Removed old skipped runs log: {skipped_runs_log_path}")
        except OSError as e:
            logger.warning(f"Could not remove old skipped runs log {skipped_runs_log_path}: {e}")

    input_dir = args.general_settings.input_dir
    inlist_name = args.general_settings.inlist_name
    analyze_blue_loop = args.blue_loop_analysis.analyze_blue_loop
    blue_loop_output_type = args.blue_loop_analysis.blue_loop_output_type
    force_reanalysis = args.general_settings.force_reanalysis

    # These paths should ideally come from output_manager.get_analysis_file_paths in cli.py
    # or be passed into this function. For now, they are redefined here for a self-contained
    # update, but consider refactoring if these are also managed by output_manager.
    summary_csv_path = os.path.join(analysis_results_sub_dir, "summary_results.csv")
    # cross_csv_path will now be generated inside the loop for each Y value,
    # so we'll store the paths in a list if needed to return multiple.
    # For now, we'll keep `cross_csv_path` as a base for generating the filenames.
    base_cross_csv_path = os.path.join(analysis_results_sub_dir, "crossing_count_grid")


    # Initialize gyre_input_csv_name_from_config
    gyre_input_csv_name_from_config = None

    if args.gyre_workflow.run_gyre_workflow:
        gyre_input_csv_name_from_config = args.gyre_workflow.filtered_profiles_csv_name
        if gyre_input_csv_name_from_config is None:
            logger.warning("GYRE workflow enabled but 'filtered_profiles_csv_name' not specified in config. Using 'sorted_blue_loop_profiles.csv' as default.")
            gyre_input_csv_name_from_config = "sorted_blue_loop_profiles.csv"

    gyre_input_csv_path = None
    if gyre_input_csv_name_from_config is not None:
        gyre_input_csv_path = os.path.join(analysis_results_sub_dir, gyre_input_csv_name_from_config)
    else:
        logger.info("GYRE input CSV path not initialized as GYRE workflow is disabled or name is missing.")

    # Reanalysis logic needs to be updated to account for multiple cross-grid files
    # For simplicity, we'll assume if *any* cross-grid exists, and force_reanalysis is False,
    # we try to load. A more robust solution might check all expected Y-specific cross-grids.
    reanalysis_needed = force_reanalysis or \
                        not os.path.exists(summary_csv_path)

    # We can't easily check for all potential cross_csv_paths here without knowing unique_ys beforehand.
    # So, if reanalysis_needed is false based on summary_csv, we'll proceed, but cross-grids will be
    # generated if they don't exist later.

    detail_csvs_exist = True
    if analyze_blue_loop:
        if not os.path.exists(detail_files_output_dir) or not os.listdir(detail_files_output_dir):
            detail_csvs_exist = False
            if not force_reanalysis:
                logger.info(f"Detailed blue loop CSVs not found in '{detail_files_output_dir}'. Forcing reanalysis to generate them for plotting.")
                reanalysis_needed = True

    logger.info(f"Analysis started. Full reanalysis needed: {reanalysis_needed}")

    summary_df = pd.DataFrame()
    combined_detail_data_for_plotting = pd.DataFrame()
    full_history_data_for_plotting = {}
    gyre_output_csv_path_returned = ""
    generated_rsp_inlists_paths = []
    generated_cross_csv_paths = [] # NEW: To store paths of generated cross-grids

    if not reanalysis_needed:
        logger.info("Summary and cross-grid CSV files already exist. Attempting to load existing data.")
        try:
            # Load summary_df
            loaded_summary_df = pd.read_csv(summary_csv_path) # Load as regular DataFrame first
            # Check for initial_Y in columns before setting index
            index_cols = ['initial_Z', 'initial_mass']
            if 'initial_Y' in loaded_summary_df.columns:
                index_cols.insert(1, 'initial_Y') # Insert Y after Z

            if all(col in loaded_summary_df.columns for col in index_cols):
                loaded_summary_df.set_index(index_cols, inplace=True)
            else:
                logger.warning(f"Loaded summary CSV is missing one or more expected index columns ({index_cols}). Cannot set index properly. Reanalysis might be needed.")
                reanalysis_needed = True # Force reanalysis if index columns are missing

            if not reanalysis_needed: # Only proceed if index setting was successful or not needed
                if analyze_blue_loop:
                    filtered_loaded_summary_df = loaded_summary_df[
                        (loaded_summary_df['blue_loop_crossing_count'].notna()) &
                        (loaded_summary_df['blue_loop_crossing_count'] > 0)
                    ].copy()
                else:
                    filtered_loaded_summary_df = loaded_summary_df.copy()
                    logger.info(f"Blue loop analysis is OFF. All entries from '{summary_csv_path}' will be considered for GYRE input.")

                if filtered_loaded_summary_df.empty and not loaded_summary_df.empty:
                    logger.warning("Loaded summary CSV contained no valid blue loop entries after filtering. Forcing reanalysis if blue loop analysis is on.")
                    if analyze_blue_loop:
                        reanalysis_needed = True
                    else:
                        logger.info("No successful MESA runs found in loaded summary CSV. Cannot generate GYRE input.")
                        return pd.DataFrame(), pd.DataFrame(), {}, "", [], [] # Return empty list for cross_csv_paths
                elif filtered_loaded_summary_df.empty and loaded_summary_df.empty:
                    logger.info("Loaded summary CSV was empty. No valid entries found.")
                    return pd.DataFrame(), pd.DataFrame(), {}, "", [], [] # Return empty list for cross_csv_paths
                else:
                    summary_df = filtered_loaded_summary_df
                    logger.info("Successfully loaded and filtered existing summary CSV.")

                if args.gyre_workflow.get('run_gyre_workflow', False) and gyre_input_csv_path:
                    if os.path.exists(gyre_input_csv_path):
                        try:
                            gyre_input_df_loaded = pd.read_csv(gyre_input_csv_path)
                            logger.info(f"Successfully loaded existing GYRE input CSV from {gyre_input_csv_path}")
                            gyre_output_csv_path_returned = gyre_input_csv_path
                        except Exception as e:
                            logger.error(f"Failed to load existing GYRE input CSV from {gyre_input_csv_path}: {e}")
                            logger.exception("GYRE input CSV loading exception details:")
                            gyre_output_csv_path_returned = ""
                    else:
                        logger.warning(f"GYRE workflow enabled, but existing GYRE input CSV '{gyre_input_csv_name_from_config}' not found at {gyre_input_csv_path}. It will not be generated unless a full reanalysis is triggered.")
                        gyre_output_csv_path_returned = ""
                else:
                    logger.info("GYRE workflow is disabled or GYRE input CSV path not defined. Skipping loading of existing GYRE input CSV.")
                    gyre_output_csv_path_returned = ""

                if not reanalysis_needed:
                    logger.info("Detail data not in memory; attempting to load from disk for plotting...")
                    combined_detail_dfs = []
                    if os.path.exists(detail_files_output_dir):
                        logger.info(f"Loading CSV files from '{detail_files_output_dir}'...")
                        for f_name in os.listdir(detail_files_output_dir):
                            if f_name.endswith(".csv"):
                                try:
                                    df = pd.read_csv(os.path.join(detail_files_output_dir, f_name))
                                    combined_detail_dfs.append(df)
                                except Exception as e:
                                    logger.warning(f"Failed to load detail CSV '{f_name}': {e}")
                        if combined_detail_dfs:
                            combined_detail_data_for_plotting = pd.concat(combined_detail_dfs, ignore_index=True)
                            logger.info(f"Successfully loaded {len(combined_detail_dfs)} detail CSVs.")
                            # Ensure sorting by initial_Y as well if it's in the data
                            sort_cols = ['initial_Z', 'initial_mass', 'star_age']
                            if 'initial_Y' in combined_detail_data_for_plotting.columns:
                                sort_cols.insert(1, 'initial_Y') # Insert Y after Z
                            combined_detail_data_for_plotting = combined_detail_data_for_plotting.sort_values(
                                by=sort_cols
                            ).reset_index(drop=True)
                        else:
                            logger.error(f"No CSV files loaded from '{detail_files_output_dir}'.")
                    else:
                        logger.error(f"Detail files output directory '{detail_files_output_dir}' does not exist.")
                
                # Also try to load existing cross-grid CSVs.
                # This will require scanning for files matching the new naming convention.
                if os.path.exists(analysis_results_sub_dir):
                    for fname in os.listdir(analysis_results_sub_dir):
                        if fname.startswith("crossing_count_grid_Y") and fname.endswith(".csv"):
                            generated_cross_csv_paths.append(os.path.join(analysis_results_sub_dir, fname))
                    if not generated_cross_csv_paths:
                        logger.warning("No existing Y-specific cross-grid CSVs found. Full reanalysis might be needed to generate them.")
                        reanalysis_needed = True # If we didn't find them, force reanalysis to generate.

                # This return needs to be inside the "if not reanalysis_needed" block to actually return
                if not reanalysis_needed:
                    return summary_df, combined_detail_data_for_plotting, full_history_data_for_plotting, gyre_output_csv_path_returned, generated_rsp_inlists_paths, generated_cross_csv_paths # NEW: return list of cross_csv_paths

        except FileNotFoundError:
            logger.warning(f"Existing summary or cross-grid CSVs not found. Forcing full reanalysis.")
            reanalysis_needed = True
        except Exception as e:
            logger.error(f"Error loading existing summary CSV: {e}. Forcing full reanalysis.")
            logger.exception("Error details:")
            reanalysis_needed = True

    if reanalysis_needed:
        logger.info("Starting full analysis of MESA runs...")

        mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)
        if not mesa_run_infos:
            logger.info("No MESA runs found for full analysis. Returning empty DataFrames.")
            return pd.DataFrame(), pd.DataFrame(), {}, "", [], [] # Return empty list for cross_csv_paths

        # Extract all unique parameters for index/columns
        unique_masses = sorted(set(run['mass'] for run in mesa_run_infos))
        unique_zs = sorted(set(run['z'] for run in mesa_run_infos))
        unique_ys = sorted(set(run['y'] for run in mesa_run_infos)) # NEW: Get unique Y values

        if not unique_masses or not unique_zs or not unique_ys: # NEW: Check Y as well
            logger.error("Error: Could not determine unique masses, metallicities, or helium abundances from runs. Returning empty DataFrames.")
            return pd.DataFrame(), pd.DataFrame(), {}, "", [], [] # Return empty list for cross_csv_paths

        # We will generate `cross_data_matrix` per Y value later.
        # cross_data_matrix = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses)
        # cross_data_matrix.index.name = "Z"
        # cross_data_matrix.columns.name = "Mass"

        summary_data = []
        # Group detailed data by Z and Y
        grouped_detailed_dfs_for_analysis_raw = {z_val: {y_val: [] for y_val in unique_ys} for z_val in unique_zs}
        full_history_data_for_plotting = {z_val: {y_val: [] for y_val in unique_ys} for z_val in unique_zs} # Nested dict for Z and Y

        yaml_data = {}
        for run_info in sorted(mesa_run_infos, key=lambda x: (x['z'], x['y'], x['mass'])): # Sort by Y too
            z_key = f"Z_{run_info['z']:.4f}"
            y_key = f"Y_{run_info['y']:.3f}" # NEW: Y key for YAML
            if z_key not in yaml_data:
                yaml_data[z_key] = {}
            if y_key not in yaml_data[z_key]: # NEW: Nested Y under Z in YAML
                yaml_data[z_key][y_key] = {}
            mass_key = f"M_{run_info['mass']:.1f}"
            yaml_data[z_key][y_key][mass_key] = { # Store under Z, Y, Mass
                'run_directory': os.path.basename(run_info['run_dir_path']),
                'history_file': os.path.basename(run_info['history_file_path']),
            }
        yaml_file_path = os.path.join(analysis_results_sub_dir, "processed_runs_overview.yaml")
        try:
            with open(yaml_file_path, 'w') as f:
                yaml.dump(yaml_data, f, indent=4, sort_keys=False)
            logger.info(f"Generated YAML overview of processed runs: {yaml_file_path}")
        except Exception as e:
            logger.error(f"Could not write YAML overview file: {e}")

        if os.path.exists(skipped_runs_log_path):
            os.remove(skipped_runs_log_path) # Clear previous log

        total_runs_for_analysis = len(mesa_run_infos)
        with tqdm(total=total_runs_for_analysis, desc="Performing MESA Run Analysis") as pbar:
            for run_info in mesa_run_infos:
                current_mass = run_info['mass']
                current_z = run_info['z']
                current_y = run_info['y'] # NEW: Get current_y from run_info
                history_file_path = run_info['history_file_path']
                run_dir_path = run_info['run_dir_path']

                analysis_result_summary = {
                    'initial_mass': current_mass,
                    'initial_Z': current_z,
                    'initial_Y': current_y, # NEW: Add initial_Y to summary
                    'run_dir_path': run_dir_path,
                    'blue_loop_crossing_count': np.nan,
                    'blue_loop_duration_yr': np.nan, 'max_log_L': np.nan, 'max_log_Teff': np.nan,
                    'max_log_R': np.nan, 'first_model_number': np.nan, 'last_model_number': np.nan,
                    'first_age_yr': np.nan, 'last_age_yr': np.nan, 'blue_loop_start_age': np.nan,
                    'blue_loop_end_age': np.nan, 'instability_start_age': np.nan,
                    'instability_end_age': np.nan, 'calculated_blue_loop_duration': np.nan,
                    'calculated_instability_duration': np.nan,
                    'analysis_status': 'Success' # Default status
                }

                current_detail_df = pd.DataFrame()

                try:
                    df_full_history = get_data_from_history_file(history_file_path)
                    df_full_history['initial_mass'] = current_mass
                    df_full_history['initial_Z'] = current_z
                    df_full_history['initial_Y'] = current_y # NEW: Add initial_Y to full history DataFrame
                    df_full_history['run_dir_path'] = run_dir_path # Add run_dir_path to full history DataFrame

                    if current_z not in full_history_data_for_plotting:
                        full_history_data_for_plotting[current_z] = {} # Ensure nested dict
                    if current_y not in full_history_data_for_plotting[current_z]: # Ensure nested dict
                        full_history_data_for_plotting[current_z][current_y] = []
                    full_history_data_for_plotting[current_z][current_y].append(df_full_history.copy()) # Store by Z and Y

                    if analyze_blue_loop:
                        # --- THIS CALL IS NOW TO THE CORRECTLY IMPORTED FUNCTION WITH 'initial_Y' ---
                        analyzer_output = analyze_blue_loop_and_instability(df_full_history, current_mass, current_z, current_y)

                        if not analyzer_output['blue_loop_detail_df'].empty:
                            bl_df = analyzer_output['blue_loop_detail_df'].copy()
                            bl_df['run_dir_path'] = run_dir_path # Ensure run_dir_path is also in bl_df for consistency
                            bl_df['initial_mass'] = current_mass # Add mass to detail df
                            bl_df['initial_Z'] = current_z # Add Z to detail df
                            bl_df['initial_Y'] = current_y # NEW: Add initial_Y to bl_df

                            if blue_loop_output_type == 'all':
                                current_detail_df = bl_df
                            else:
                                concise_detail_columns_local = [
                                    'initial_mass', 'initial_Z', 'initial_Y', 'star_age', 'model_number', # NEW: Add initial_Y here
                                    'log_Teff', 'log_L', 'log_g', 'profile_number',
                                    'run_dir_path'
                                    ]
                                existing_desired_cols = [col for col in concise_detail_columns_local if col in bl_df.columns]
                                if existing_desired_cols:
                                    current_detail_df = bl_df[existing_desired_cols]
                                else:
                                    logger.warning(f"No desired columns found for concise detail for M={current_mass}, Z={current_z}, Y={current_y}. Detail DF for plotting might remain empty.") # Add Y to warning
                                    current_detail_df = pd.DataFrame()
                        else:
                            logger.info(f"analyzer_output['blue_loop_detail_df'] was empty for M={current_mass}, Z={current_z}, Y={current_y}. No detailed data for this run.") # Add Y to message

                        if pd.notna(analyzer_output['crossing_count']):
                            analysis_result_summary['blue_loop_crossing_count'] = int(analyzer_output['crossing_count'])

                            if analysis_result_summary['blue_loop_crossing_count'] > 0:
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

                                if not current_detail_df.empty:
                                    analysis_result_summary['max_log_L'] = current_detail_df['log_L'].max()
                                    analysis_result_summary['max_log_Teff'] = current_detail_df['log_Teff'].max()
                                    if 'log_R' in current_detail_df.columns:
                                        analysis_result_summary['max_log_R'] = current_detail_df['log_R'].max()
                                    elif 'log_R' in df_full_history.columns:
                                        analysis_result_summary['max_log_R'] = df_full_history['log_R'].max()
                                    analysis_result_summary['first_model_number'] = current_detail_df['model_number'].min()
                                    analysis_result_summary['last_model_number'] = current_detail_df['model_number'].max()
                                    analysis_result_summary['first_age_yr'] = current_detail_df['star_age'].min()
                                    analysis_result_summary['last_age_yr'] = current_detail_df['star_age'].max()
                                else:
                                    logger.warning(f"current_detail_df is empty for M={current_mass}, Z={current_z}, Y={current_y} despite blue loop found (count > 0). Detailed summary metrics will be NaN.") # Add Y to warning
                            else:
                                logger.info(f"No blue loop found (0 crossings) for M={current_mass}, Z={current_z}, Y={current_y}. Blue loop summary metrics will be NaN.") # Add Y to message
                        else:
                            logger.warning(f"Blue loop analysis failed for M={current_mass}, Z={current_z}, Y={current_y}. Blue loop summary metrics will be NaN.") # Add Y to warning
                            current_detail_df = pd.DataFrame()
                    else:
                        logger.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z}, Y={current_y} as analyze_blue_loop is False.") # Add Y to message
                        analysis_result_summary['blue_loop_crossing_count'] = np.nan
                        current_detail_df = pd.DataFrame()

                    summary_data.append(analysis_result_summary)

                    if analyze_blue_loop and not current_detail_df.empty:
                        # Ensure proper nested structure for grouped_detailed_dfs_for_analysis_raw
                        if current_z not in grouped_detailed_dfs_for_analysis_raw:
                            grouped_detailed_dfs_for_analysis_raw[current_z] = {}
                        if current_y not in grouped_detailed_dfs_for_analysis_raw[current_z]:
                            grouped_detailed_dfs_for_analysis_raw[current_z][current_y] = []
                        grouped_detailed_dfs_for_analysis_raw[current_z][current_y].append(current_detail_df)

                        # This part ensures that `combined_detail_data_for_plotting` is built correctly
                        # by concatenating all `current_detail_df` instances as they are processed.
                        if combined_detail_data_for_plotting.empty:
                            combined_detail_data_for_plotting = current_detail_df.copy()
                        else:
                            combined_detail_data_for_plotting = pd.concat([combined_detail_data_for_plotting, current_detail_df], ignore_index=True)

                except Exception as err:
                    with open(skipped_runs_log_path, 'a') as log_file:
                        log_file.write(f"Skipped run {run_info['run_dir_path']} due to error: {err}\n")
                    logger.error(f"Skipped run {run_info['run_dir_path']} due to error: {err}")
                    logger.exception(f"Exception details for run {run_info['run_dir_path']}:")
                    # Append a summary entry even if there was an error, marking it as such
                    error_summary = {
                        'initial_mass': current_mass,
                        'initial_Z': current_z,
                        'initial_Y': current_y, # Ensure Y is recorded even on error
                        'run_dir_path': run_dir_path,
                        'blue_loop_crossing_count': np.nan,
                        'blue_loop_duration_yr': np.nan, 'max_log_L': np.nan, 'max_log_Teff': np.nan,
                        'max_log_R': np.nan, 'first_model_number': np.nan, 'last_model_number': np.nan,
                        'first_age_yr': np.nan, 'last_age_yr': np.nan, 'blue_loop_start_age': np.nan,
                        'blue_loop_end_age': np.nan, 'instability_start_age': np.nan,
                        'instability_end_age': np.nan, 'calculated_blue_loop_duration': np.nan,
                        'calculated_instability_duration': np.nan,
                        'analysis_status': f"Error: {str(err)[:100]}" # Truncate error message
                    }
                    summary_data.append(error_summary)

                pbar.update(1)

        summary_df_raw = pd.DataFrame(summary_data)
        # Ensure 'initial_Y' is included in the sort and index for comprehensive results
        summary_df_raw.sort_values(['initial_Z', 'initial_Y', 'initial_mass'], inplace=True)
        summary_df_raw.set_index(['initial_Z', 'initial_Y', 'initial_mass'], inplace=True) # NEW: Set Y as part of the index

        if analyze_blue_loop:
            summary_df_to_save = summary_df_raw[
                (summary_df_raw['blue_loop_crossing_count'].notna()) &
                (summary_df_raw['blue_loop_crossing_count'] > 0)
            ].copy()
            if summary_df_to_save.empty:
                logger.info("No valid blue loop entries found after filtering for summary CSV.")
            else:
                logger.info(f"Generated summary CSV will contain {len(summary_df_to_save)} blue loop entries.")
        else:
            summary_df_to_save = summary_df_raw.copy()
            logger.info(f"Blue loop analysis is OFF. Generated summary CSV will contain all {len(summary_df_to_save)} successful MESA runs.")

        if summary_df_to_save.empty:
            # Re-define columns including 'initial_Y'
            summary_df = pd.DataFrame(columns=[
                'initial_mass', 'initial_Z', 'initial_Y', 'run_dir_path', # Added initial_Y here
                'blue_loop_crossing_count', 'blue_loop_duration_yr',
                'blue_loop_start_age', 'blue_loop_end_age',
                'instability_start_age', 'instability_end_age',
                'calculated_blue_loop_duration', 'calculated_instability_duration',
                'max_log_L', 'max_log_Teff', 'max_log_R',
                'first_model_number', 'last_model_number',
                'first_age_yr', 'last_age_yr', 'analysis_status' # Include analysis_status
            ], index=pd.MultiIndex.from_tuples([], names=['initial_Z', 'initial_Y', 'initial_mass'])) # Added initial_Y to MultiIndex
        else:
            summary_df = summary_df_to_save.copy()
            if blue_loop_output_type == 'summary' and analyze_blue_loop:
                logger.info("Applying 'summary' output type filtering for summary CSV columns.")
                summary_columns_for_summary_output = [
                    'blue_loop_crossing_count', 'blue_loop_duration_yr',
                    'blue_loop_start_age', 'blue_loop_end_age',
                    'instability_start_age', 'instability_end_age',
                    'calculated_blue_loop_duration', 'calculated_instability_duration',
                    'run_dir_path', # 'initial_Y' should be part of the index, not listed here as a regular column for summary type
                    'analysis_status' # Keep status
                ]
                existing_summary_cols = [col for col in summary_columns_for_summary_output if col in summary_df.columns]
                summary_df = summary_df[existing_summary_cols].copy()

        summary_df.to_csv(summary_csv_path, na_rep='NaN')
        logger.info(f"Summary CSV written to {summary_csv_path}")

        # --- MODIFIED CROSS-GRID GENERATION LOGIC ---
        # Iterate through each unique Y value to create a separate cross-grid.
        # This resolves the "cannot reindex on an axis with duplicate labels" error.
        generated_cross_csv_paths = [] # Reset for reanalysis case
        if 'blue_loop_crossing_count' in summary_df_raw.columns:
            for y_val in unique_ys:
                logger.info(f"Generating cross-grid for initial_Y = {y_val:.3f}...")
                
                # Filter the summary data for the current Y value
                # We need to reset index first to filter on 'initial_Y' column, then set it back or unstack.
                # A simpler way is to use .xs() if 'initial_Y' is part of a MultiIndex, then unstack.
                # If summary_df_raw is already multi-indexed by (Z, Y, Mass):
                try:
                    summary_filtered_by_y = summary_df_raw.xs(y_val, level='initial_Y', drop_level=False)
                    # Now, create the cross-grid (Z vs Mass) for this specific Y
                    # We need to drop the 'initial_Y' level after filtering, then unstack 'initial_mass'
                    cross_data_matrix_for_y = summary_filtered_by_y['blue_loop_crossing_count'].droplevel('initial_Y').unstack(level='initial_mass')
                    
                    if not cross_data_matrix_for_y.empty:
                        # Ensure columns and index are numeric for sorting
                        cross_data_matrix_for_y.columns = pd.to_numeric(cross_data_matrix_for_y.columns, errors='coerce')
                        cross_data_matrix_for_y.index = pd.to_numeric(cross_data_matrix_for_y.index, errors='coerce')
                        
                        # Sort the index and columns. The reindex on unique values is now safe
                        # because we've filtered by Y, removing the source of duplicates on the Z axis.
                        cross_data_matrix_for_y = cross_data_matrix_for_y.reindex(
                            index=sorted(cross_data_matrix_for_y.index.unique()),
                            columns=sorted(cross_data_matrix_for_y.columns.unique())
                        )
                        cross_data_matrix_for_y = cross_data_matrix_for_y.where(pd.notna(cross_data_matrix_for_y), np.nan)
                    else:
                        logger.warning(f"Cross-grid matrix is empty for initial_Y={y_val:.3f}. It might be due to no blue loop crossings for this Y or data structure issues.")
                        # Still create an empty DataFrame with expected dimensions if no data
                        cross_data_matrix_for_y = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses)

                    # Construct a new filename including the Y value
#                    cross_csv_path_for_y = f"{base_cross_csv_path}_Y{y_val:.3f}.csv" # Example: crossing_count_grid_Y0.256.csv
                    cross_csv_path_for_y = f"{base_cross_csv_path}.csv"
                    
                    cross_data_matrix_for_y.to_csv(cross_csv_path_for_y, na_rep='NaN')
                    generated_cross_csv_paths.append(cross_csv_path_for_y)
                    logger.info(f"Cross-grid CSV for Y={y_val:.3f} written to {cross_csv_path_for_y}")

                except KeyError as ke:
                    logger.warning(f"Skipping cross-grid generation for Y={y_val:.3f} due to missing key in summary data: {ke}")
                except Exception as e:
                    logger.error(f"Critical error during cross-grid generation for Y={y_val:.3f}: {e}", exc_info=True)
        else:
            logger.warning("No 'blue_loop_crossing_count' in summary_df_raw. No cross-grid matrices will be generated.")


        # Combine all detailed data for plotting/RSP generation
        combined_detail_data_for_plotting = pd.DataFrame()
        if analyze_blue_loop:
            concise_detail_columns_for_saving = [
                'initial_mass', 'initial_Z', 'initial_Y', 'star_age', 'model_number', # NEW: Add initial_Y here
                'log_Teff', 'log_L', 'log_g', 'profile_number',
                'run_dir_path'
            ]
            for z_val, y_dict in grouped_detailed_dfs_for_analysis_raw.items(): # Iterate through Z and Y
                for y_val, dfs_list in y_dict.items():
                    if dfs_list:
                        try:
                            combined_df_bl = pd.concat(dfs_list, ignore_index=True)
                            # Sort by Y, then Mass, then Age for consistent output
                            combined_df_bl = combined_df_bl.sort_values(by=['initial_Y', 'initial_mass', 'star_age']).reset_index(drop=True)

                            if blue_loop_output_type == 'all':
                                df_to_save = combined_df_bl
                                output_type_label = "all columns"
                            else:
                                existing_desired_cols = [col for col in concise_detail_columns_for_saving if col in combined_df_bl.columns]
                                if not existing_desired_cols:
                                    logger.warning(f"No desired columns found for concise detail CSV for Z={z_val}, Y={y_val}. Skipping detail CSV write.")
                                    continue
                                df_to_save = combined_df_bl[existing_desired_cols]
                                output_type_label = "selected columns"

                            detail_filename = os.path.join(detail_files_output_dir, f"detail_z{z_val:.4f}_y{y_val:.3f}.csv") # NEW: Include Y in filename
                            df_to_save.to_csv(detail_filename, index=False, na_rep='NaN')
                            logger.info(f"Written concatenated detail CSV for Z={z_val}, Y={y_val} with {output_type_label} to {detail_filename}") # Log with Y

                            # Accumulate into combined_detail_data_for_plotting only if it hasn't been populated from disk already
                            # This block is for building it during reanalysis
                            if combined_detail_data_for_plotting.empty:
                                combined_detail_data_for_plotting = df_to_save.copy()
                            else:
                                combined_detail_data_for_plotting = pd.concat([combined_detail_data_for_plotting, df_to_save], ignore_index=True)

                        except Exception as e:
                            logger.error(f"Error writing detail CSV for Z={z_val}, Y={y_val}: {e}", exc_info=True) # Log with Y
                    else:
                        logger.info(f"No detailed data to write for Z={z_val}, Y={y_val}.") # Log with Y
        else:
            logger.info("Skipping detailed blue loop CSV generation: Blue loop analysis is disabled.")

    # Logic for RSP inlists generation
    if args.rsp_workflow.run_rsp_workflow:
        logger.info("Generating MESA RSP inlists.")
        try:
            rsp_template_path = args.rsp_workflow.rsp_inlist_template_path
            rsp_output_dir = rsp_output_subdir
            try:
                generated_rsp_inlists_paths = generate_mesa_rsp_inlists(
                    detail_df=combined_detail_data_for_plotting, # Use the combined df which now includes initial_Y
                    mesa_output_base_dir=input_dir, # The root directory of your MESA runs
                    rsp_inlist_template_path=rsp_template_path,
                    rsp_output_subdir=rsp_output_dir
                )
            except TypeError as e:
                logger.error(f"Error during MESA RSP inlist generation: {e}")
                # Return a value that indicates failure or exit the program
                return {"status": "error", "message": str(e)}

            # Check if the function returned an error status, and handle it gracefully
            if isinstance(generated_rsp_inlists_paths, dict) and generated_rsp_inlists_paths.get("status") == "error":
                return {"successful": False, "message": "RSP inlist generation failed."}            
            if generated_rsp_inlists_paths:
                logger.info(f"Successfully generated {len(generated_rsp_inlists_paths)} MESA RSP inlist files.")
            else:
                logger.warning("No MESA RSP inlist files were generated.")

        except Exception as e:
            logger.error(f"Error during MESA RSP inlist generation: {e}", exc_info=True)
    else:
        logger.info("Skipping MESA RSP inlist generation: RSP workflow is disabled.")

# --- START LOGIC FOR GYRE INPUT CSV GENERATION/LOADING ---
    if args.gyre_workflow.run_gyre_workflow:
        logger.info("GYRE workflow is enabled. Checking GYRE input CSV status.")
        gyre_input_df = pd.DataFrame()

        if gyre_input_csv_path is None:
            logger.warning("GYRE input CSV path not defined, skipping GYRE input CSV load/generation.")
            gyre_output_csv_path_returned = ""
        else:
            if os.path.exists(gyre_input_csv_path):
                try:
                    gyre_input_df = pd.read_csv(gyre_input_csv_path)
                    logger.info(f"Successfully loaded existing GYRE input CSV from {gyre_input_csv_path}")
                    gyre_output_csv_path_returned = gyre_input_csv_path
                except Exception as e:
                    logger.error(f"Failed to load existing GYRE input CSV from {gyre_input_csv_path}: {e}")
                    logger.exception("GYRE input CSV loading exception details:")
                    gyre_output_csv_path_returned = ""
                    gyre_input_df = pd.DataFrame()
            else:
                logger.info(f"GYRE input CSV '{gyre_input_csv_name_from_config}' not found. Checking if generation is needed.")
                gyre_output_csv_path_returned = ""

            if gyre_input_df.empty:
                logger.info("Attempting to generate GYRE input CSV from MESA runs (independent path).")
                try:
                    source_df_for_gyre = pd.DataFrame()
                    if 'summary_df_raw' in locals() and not summary_df_raw.empty:
                        # Reset index to make 'initial_Y' a column again for filtering
                        source_df_for_gyre = summary_df_raw.reset_index().copy()
                        logger.info("Using data from recently generated summary_df_raw for GYRE input.")
                    elif not reanalysis_needed:
                        logger.info("Scanning MESA runs to generate missing GYRE input CSV.")
                        # These calls are now to the correctly imported functions
                        mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)
                        if mesa_run_infos:
                            temp_gyre_data = []
                            for run_info in mesa_run_infos:
                                history_file_path = run_info['history_file_path']
                                try:
                                    df_full_history = get_data_from_history_file(history_file_path)
                                    if not df_full_history.empty:
                                        temp_gyre_data.append({
                                            'initial_mass': run_info['mass'],
                                            'initial_Z': run_info['z'],
                                            'initial_Y': run_info['y'], # NEW: Add initial_Y
                                            'run_dir_path': run_info['run_dir_path'],
                                            'first_model_number': df_full_history['model_number'].min(),
                                            'last_model_number': df_full_history['model_number'].max(),
                                            'first_age_yr': df_full_history['star_age'].min(),
                                            'last_age_yr': df_full_history['star_age'].max()
                                        })
                                except Exception as e:
                                    logger.warning(f"Could not extract history data for GYRE input from {run_info['run_dir_path']}: {e}")
                            if temp_gyre_data:
                                source_df_for_gyre = pd.DataFrame(temp_gyre_data)
                                logger.info(f"Successfully scanned {len(temp_gyre_data)} runs for GYRE input.")
                            else:
                                logger.warning("No runs found or data extracted for GYRE input from independent scan.")
                        else:
                            logger.info("No MESA runs found during independent scan for GYRE input.")
                    else:
                        pass # No source_df_for_gyre if reanalysis not needed and no summary_df_raw

                    if not source_df_for_gyre.empty:
                        if args.blue_loop_analysis.analyze_blue_loop:
                            original_rows = len(source_df_for_gyre)
                            if 'blue_loop_crossing_count' in source_df_for_gyre.columns:
                                source_df_for_gyre = source_df_for_gyre[
                                    (source_df_for_gyre['blue_loop_crossing_count'].notna()) &
                                    (source_df_for_gyre['blue_loop_crossing_count'] > 0)
                                ].copy()
                                if len(source_df_for_gyre) < original_rows:
                                    logger.info(f"Filtered GYRE input CSV: Removed {original_rows - len(source_df_for_gyre)} entries with no blue loop crossings (based on summary data).")
                            else:
                                logger.warning("Blue loop analysis is enabled, but 'blue_loop_crossing_count' not available in source data for GYRE input. Skipping blue loop filtering for GYRE input.")
                        else:
                            logger.info("Blue loop analysis is OFF. All successful MESA runs will be included in GYRE input CSV.")

                        # Include 'initial_Y' in the columns for GYRE input
                        gyre_input_cols = ['initial_mass', 'initial_Z', 'initial_Y', 'run_dir_path']
                        if 'first_model_number' in source_df_for_gyre.columns and 'last_model_number' in source_df_for_gyre.columns:
                            gyre_input_cols.extend(['first_model_number', 'last_model_number'])
                            gyre_input_df = source_df_for_gyre[gyre_input_cols].copy()
                        else:
                            logger.warning("'first_model_number' or 'last_model_number' not found in source data for GYRE. GYRE input CSV will only contain 'run_dir_path', 'initial_mass', 'initial_Z', 'initial_Y'.")
                            gyre_input_df = source_df_for_gyre[[col for col in gyre_input_cols if col in source_df_for_gyre.columns]].copy()
                            gyre_input_df['min_model_number'] = np.nan
                            gyre_input_df['max_model_number'] = np.nan

                        if not gyre_input_df.empty:
                            gyre_input_df.rename(columns={
                                'run_dir_path': 'mesa_run_directory',
                                'first_model_number': 'min_model_number',
                                'last_model_number': 'max_model_number'
                            }, inplace=True)
                            gyre_input_df['initial_Z'] = gyre_input_df['initial_Z'].apply(lambda x: f"{x:.4f}")
                            gyre_input_df['initial_Y'] = gyre_input_df['initial_Y'].apply(lambda x: f"{x:.3f}") # Format initial_Y
                            gyre_input_df.sort_values(['initial_Z', 'initial_Y', 'initial_mass'], inplace=True) # Sort by Y as well

                            gyre_input_df.to_csv(gyre_input_csv_path, index=False, na_rep='NaN')
                            logger.info(f"GYRE input CSV saved to: {gyre_input_csv_path}")
                            gyre_output_csv_path_returned = gyre_input_csv_path
                        else:
                            logger.info("No data to write to GYRE input CSV after filtering or extraction.")
                            gyre_output_csv_path_returned = ""
                    else:
                        logger.info("No source data available to generate GYRE input CSV.")
                        gyre_output_csv_path_returned = ""

                except Exception as e:
                    logger.error(f"Error generating GYRE input CSV '{gyre_input_csv_name_from_config}': {e}")
                    logger.exception("GYRE input CSV generation exception details:")
                    gyre_output_csv_path_returned = ""
    else:
        logger.info("Skipping GYRE input CSV generation: GYRE workflow is disabled in settings.")
        gyre_output_csv_path_returned = ""

    # Final sorting of combined_detail_data_for_plotting before returning
    if not combined_detail_data_for_plotting.empty:
        sort_cols = ['initial_Z', 'initial_mass', 'star_age']
        if 'initial_Y' in combined_detail_data_for_plotting.columns:
            sort_cols.insert(1, 'initial_Y') # Insert Y after Z for sorting
        combined_detail_data_for_plotting = combined_detail_data_for_plotting.sort_values(
            by=sort_cols
        ).reset_index(drop=True)

    # Re-structure full_history_data_for_plotting to be a flat list of DataFrames for plotting,
    # or keep it nested as {Z: {Y: [DFs]}} if that structure is preferred by consumers.
    # For now, it's kept nested, but consider flattening it if the plotting functions expect a single list.
    flattened_full_history_data_for_plotting = []
    for z_val in sorted(full_history_data_for_plotting.keys()):
        for y_val in sorted(full_history_data_for_plotting[z_val].keys()):
            flattened_full_history_data_for_plotting.extend(full_history_data_for_plotting[z_val][y_val])

    # The return signature dictates a dict, so ensure it matches.
    # If the consumer expects a flat list, this might need adjustment in the calling code.
    # For now, we'll return the nested dictionary structure as built.
    return summary_df, combined_detail_data_for_plotting, flattened_full_history_data_for_plotting, gyre_output_csv_path_returned, generated_rsp_inlists_paths, generated_cross_csv_paths