# mesa_tools/cli.py

import os
import argparse
import pandas as pd
import numpy as np
import re
from tqdm import tqdm
import yaml
import logging # Added logging import for better output
import sys # Required for sys.exit()

# Set up basic logging for the entire script
# Changed to DEBUG level to provide more detailed output during execution.
logging.basicConfig(level=logging.DEBUG, format='%(levelname)s: %(message)s')


from .blue_loop_analyzer import analyze_blue_loop_and_instability
from .heatmap_generator import generate_heatmaps_and_time_diff_csv
from .blue_loop_cmd_plotter import generate_blue_loop_plots_with_bc, load_and_group_data
from .config_parser import parsing_options # Correct import for parsing_options


def extract_params_from_inlist(inlist_path):
    """
    Extract initial_mass and initial_Z from a MESA inlist file.
    """
    mass = None
    z = None

    try:
        with open(inlist_path, 'r') as f:
            content = f.read()
            mass_match = re.search(r'initial_mass\s*=\s*(\d+\.?\d*)', content)
            if mass_match:
                mass = float(mass_match.group(1))
            z_match = re.search(r'initial_Z\s*=\s*(\d+\.?\d*(?:e[+\-]?\d+)?)', content)
            if z_match:
                z = float(z_match.group(1))
    except FileNotFoundError:
        logging.error(f"Inlist file not found: {inlist_path}")
    except Exception as e:
        logging.error(f"Error reading inlist file {inlist_path}: {e}")

    return mass, z


def scan_mesa_runs(input_dir, inlist_name):
    """
    Scan input directory for MESA run subdirectories ('run_*') containing inlist and history.data files.
    """
    mesa_run_infos = []
    potential_run_dirs = [d for d in os.listdir(input_dir)
                          if os.path.isdir(os.path.join(input_dir, d)) and d.startswith('run_')]

    if not potential_run_dirs:
        logging.warning(f"No 'run_*' subdirectories found directly in {input_dir}. "
                        "Ensure MESA runs are organized as 'run_*' folders.")

    for run_dir_name in potential_run_dirs:
        run_dir_path = os.path.join(input_dir, run_dir_name)
        inlist_path = os.path.join(run_dir_path, inlist_name)
        history_file_path = os.path.join(run_dir_path, 'LOGS', 'history.data')

        if os.path.exists(inlist_path) and os.path.exists(history_file_path):
            mass, z = extract_params_from_inlist(inlist_path)
            if mass is not None and z is not None:
                mesa_run_infos.append({
                    'history_file_path': history_file_path,
                    'run_dir_path': run_dir_path,
                    'mass': mass,
                    'z': z
                })
            else:
                logging.warning(f"Could not extract mass/Z from inlist '{inlist_path}'. Skipping this run.")
        else:
            if not os.path.exists(inlist_path):
                logging.warning(f"Inlist file '{inlist_name}' not found in '{run_dir_path}'. Skipping this run.")
            if not os.path.exists(history_file_path):
                logging.warning(f"history.data not found at '{history_file_path}'. Skipping this run.")

    return mesa_run_infos


def get_data_from_history_file(history_file_path):
    """
    Reads MESA history.data file into a pandas DataFrame using np.genfromtxt.
    """
    try:
        data = np.genfromtxt(history_file_path, names=True, comments="#", skip_header=5,
                             dtype=None, encoding='utf-8')

        if data.ndim == 0: # Handle case of a single row in history.data
            df = pd.DataFrame([data.tolist()], columns=data.dtype.names)
        else:
            df = pd.DataFrame(data)

        # Convert all columns to numeric, coercing errors to NaN
        for col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')

        # Ensure model_number is int if present and not NaN
        if 'model_number' in df.columns:
            df.dropna(subset=['model_number'], inplace=True) # Drop rows where model_number is NaN
            if not df['model_number'].isnull().any(): # Check if any NaNs remain after dropping
                df['model_number'] = df['model_number'].astype(int)

        return df

    except Exception as e:
        raise type(e)(f"Error loading or processing {history_file_path} using np.genfromtxt: {e}") from e


def main():
    """ MAIN function ... First, parsing the options """

    # Call the parsing_options function from config_parser.py to get all arguments
    args = parsing_options()

    # --- FINAL DEBUG: Check the final value of args.force_reanalysis after all parsing ---
    # This log is placed here to show the state of 'args' after YAML processing.
    logging.debug(f"--- CLI DEBUG: args.force_reanalysis after all parsing: {args.force_reanalysis}")
    logging.debug(f"--- CLI DEBUG: Type of args.force_reanalysis: {type(args.force_reanalysis)}")
    # -------------------------------------------------------------------------------------

    # Assign values from the args Namespace to local variables for clarity
    input_dir = args.input_dir
    output_dir = args.output_dir
    analyze_blue_loop = args.analyze_blue_loop
    inlist_name = args.inlist_name
    should_generate_heatmaps = args.generate_heatmaps
    force_reanalysis = args.force_reanalysis # This variable now holds the correctly overridden value
    blue_loop_output_type = args.blue_loop_output_type
    should_generate_plots = args.generate_plots
    should_generate_blue_loop_plots_with_bc = args.generate_blue_loop_plots_with_bc

    # Create output directories
    analysis_results_sub_dir = os.path.join(output_dir, "analysis_results")
    plots_sub_dir = os.path.join(output_dir, "plots")
    blue_loop_plots_bc_sub_dir = os.path.join(output_dir, "blue_loop_plots_bc")
    detail_files_output_dir = os.path.join(output_dir, "detail_files")

    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    if analyze_blue_loop:
        os.makedirs(detail_files_output_dir, exist_ok=True)
    if should_generate_plots:
        os.makedirs(plots_sub_dir, exist_ok=True)
    if should_generate_blue_loop_plots_with_bc:
        os.makedirs(blue_loop_plots_bc_sub_dir, exist_ok=True)

    # Define paths for summary and cross-grid CSVs
    summary_csv_path = os.path.join(analysis_results_sub_dir, "mesa_grid_analysis_summary.csv")
    cross_csv_path = os.path.join(analysis_results_sub_dir, "mesa_grid_cross.csv")

    # --- CRITICAL DEBUGGING FOR REANALYSIS LOGIC ---
    logging.debug(f"--- REANALYSIS DEBUG ---")
    logging.debug(f"  force_reanalysis (from args): {force_reanalysis}")
    logging.debug(f"  summary_csv_path: {summary_csv_path}")
    logging.debug(f"  Does summary_csv_path exist? {os.path.exists(summary_csv_path)}")
    logging.debug(f"  cross_csv_path: {cross_csv_path}")
    logging.debug(f"  Does cross_csv_path exist? {os.path.exists(cross_csv_path)}")

    # Determine if reanalysis is needed based on force_reanalysis flag or missing summary/cross-grid files
    reanalysis_needed = force_reanalysis or \
                        not os.path.exists(summary_csv_path) or \
                        not os.path.exists(cross_csv_path)

    logging.info(f"Final reanalysis_needed flag: {reanalysis_needed}")
    # -----------------------------------------------

    grouped_full_history_dfs_for_plotting = {}
    grouped_detailed_dfs_for_analysis_raw = {} # Changed name to reflect it's a list of DFs

    # This will be the single DataFrame passed to the plotter (formerly concatenated_detailed_dfs_for_plotting was a dict)
    # Now, this will be the final, fully combined DataFrame for plotting
    combined_detail_data_for_plotting = pd.DataFrame()


    mesa_run_infos = []

    # Determine if any operation requires loading history.data
    # History data is needed for: full analysis (reanalysis_needed), general HRD plots,
    # heatmap generation (if summary/cross-grid don't exist and need regeneration),
    # and if blue loop analysis is requested (which uses history data to generate detail files).
    load_history_data = reanalysis_needed or should_generate_plots or should_generate_heatmaps or analyze_blue_loop

    if load_history_data:
        logging.info(f"Scanning MESA runs and loading history data from {input_dir}...")
        mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)
        if not mesa_run_infos:
            logging.info("No MESA runs found. Exiting.")
            return

        total_runs = len(mesa_run_infos)
        logging.info(f"Found {total_runs} runs for analysis/plotting.")

        skipped_runs_log_path = os.path.join(analysis_results_sub_dir, "skipped_runs_log.txt")
        if os.path.exists(skipped_runs_log_path):
            os.remove(skipped_runs_log_path)

        # Load all history files if any operation requires them
        with tqdm(total=total_runs, desc="Loading history.data files") as pbar:
            for run_info in mesa_run_infos:
                history_file_path = run_info['history_file_path']
                current_mass = run_info['mass']
                current_z = run_info['z']
                try:
                    df_full_history = get_data_from_history_file(history_file_path)
                    df_full_history['initial_mass'] = current_mass
                    df_full_history['initial_Z'] = current_z

                    if current_z not in grouped_full_history_dfs_for_plotting:
                        grouped_full_history_dfs_for_plotting[current_z] = []
                    grouped_full_history_dfs_for_plotting[current_z].append(df_full_history)
                except Exception as err:
                    with open(skipped_runs_log_path, 'a') as log_file:
                        log_file.write(f"Skipped loading history.data for {run_info['run_dir_path']} due to error: {err}\n")
                    logging.warning(f"Skipped loading history.data for {run_info['run_dir_path']} due to error: {err}")
                pbar.update(1)
    else:
        logging.info("Skipping MESA run scan and history.data loading as no operations require it based on config.")


    # Analysis and Plotting Orchestration
    summary_df_loaded = pd.DataFrame() # Initialize empty DataFrame
    if not reanalysis_needed:
        logging.info("Summary and cross-grid CSV files already exist. Skipping full reanalysis.")
        try:
            summary_df_loaded = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])
            if should_generate_blue_loop_plots_with_bc:
                logging.info(f"Loading existing blue loop detail files from {detail_files_output_dir} for plotting...")
                # Call load_and_group_data directly to get the single combined DataFrame
                combined_detail_data_for_plotting = load_and_group_data(detail_files_output_dir)

                if combined_detail_data_for_plotting.empty:
                    logging.warning(f"No detail files found in {detail_files_output_dir}. Cannot generate blue loop BC plots without them. Please ensure 'analyze_blue_loop: True' was run previously or detail files are present.")

        except FileNotFoundError:
            logging.warning(f"Summary CSV not found at {summary_csv_path}. Full reanalysis will be forced.")
            reanalysis_needed = True # Force reanalysis if summary not found
        except Exception as e:
            logging.error(f"Error loading summary CSV for existing data check: {e}. Full reanalysis will be forced.")
            reanalysis_needed = True


    if reanalysis_needed:
        logging.info("Starting full analysis of MESA runs (reanalysis_needed is True)...")
        # Ensure mesa_run_infos is populated if it wasn't already by load_history_data
        if not mesa_run_infos:
            mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)
            if not mesa_run_infos:
                logging.info("No MESA runs found for full analysis. Exiting.")
                return

        unique_masses = sorted(set(run['mass'] for run in mesa_run_infos))
        unique_zs = sorted(set(run['z'] for run in mesa_run_infos))

        if not unique_masses or not unique_zs:
            logging.error("Error: Could not determine unique masses or metallicities from runs. Exiting.")
            return

        cross_data_matrix = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses)
        cross_data_matrix.index.name = "Z"
        cross_data_matrix.columns.name = "Mass"

        summary_data = []
        # Reinitialize for analysis if reanalysis is forced
        grouped_detailed_dfs_for_analysis_raw = {z_val: [] for z_val in unique_zs}

        # Generate YAML overview (this uses mesa_run_infos which should be populated now)
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

        # Full analysis loop (using grouped_full_history_dfs_for_plotting if loaded)
        total_runs_for_analysis = len(mesa_run_infos)
        with tqdm(total=total_runs_for_analysis, desc="Performing Blue Loop Analysis") as pbar:
            for run_info in mesa_run_infos:
                current_mass = run_info['mass']
                current_z = run_info['z']

                # Retrieve the full history DataFrame for the current run
                df_full_history_for_analysis = None
                if current_z in grouped_full_history_dfs_for_plotting:
                    for df in grouped_full_history_dfs_for_plotting[current_z]:
                        # Ensure we get the correct DataFrame for the current mass and Z
                        # by checking 'initial_mass' which was added during loading
                        if 'initial_mass' in df.columns and df['initial_mass'].iloc[0] == current_mass:
                            df_full_history_for_analysis = df
                            break

                analysis_result_summary = {
                    'initial_mass': current_mass, 'initial_Z': current_z, 'blue_loop_crossing_count': np.nan,
                    'blue_loop_duration_yr': np.nan, 'max_log_L': np.nan, 'max_log_Teff': np.nan,
                    'max_log_R': np.nan, 'first_model_number': np.nan, 'last_model_number': np.nan,
                    'first_age_yr': np.nan, 'last_age_yr': np.nan, 'blue_loop_start_age': np.nan,
                    'blue_loop_end_age': np.nan, 'instability_start_age': np.nan,
                    'instability_end_age': np.nan, 'calculated_blue_loop_duration': np.nan,
                    'calculated_instability_duration': np.nan
                }
                current_detail_df = pd.DataFrame()

                try:
                    if df_full_history_for_analysis is not None and analyze_blue_loop:
                        analyzer_output = analyze_blue_loop_and_instability(df_full_history_for_analysis, current_mass, current_z)

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
                                bl_df['initial_mass'] = current_mass
                                bl_df['initial_Z'] = current_z

                                if blue_loop_output_type == 'all':
                                    analysis_result_summary['max_log_L'] = bl_df['log_L'].max()
                                    analysis_result_summary['max_log_Teff'] = bl_df['log_Teff'].max()
                                    if 'log_R' in bl_df.columns:
                                        analysis_result_summary['max_log_R'] = bl_df['log_R'].max()
                                    elif 'log_R' in df_full_history_for_analysis.columns:
                                        analysis_result_summary['max_log_R'] = df_full_history_for_analysis['log_R'].max()
                                    analysis_result_summary['first_model_number'] = bl_df['model_number'].min()
                                    analysis_result_summary['last_model_number'] = bl_df['model_number'].max()
                                    analysis_result_summary['first_age_yr'] = bl_df['star_age'].min()
                                    analysis_result_summary['last_age_yr'] = bl_df['star_age'].max()

                                current_detail_df = bl_df # Assign the full or filtered BL df for saving

                            cross_data_matrix.at[current_z, current_mass] = analysis_result_summary['blue_loop_crossing_count']
                        else:
                            logging.warning(f"Blue loop analysis failed or returned no valid crossings for M={current_mass}, Z={current_z}. Results for this run will be NaN in summary.")
                    elif not analyze_blue_loop:
                        logging.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.")
                    else:
                        logging.warning(f"Full history data not found for M={current_mass}, Z={current_z}. Skipping blue loop analysis for this run.")

                    summary_data.append(analysis_result_summary)

                    if analyze_blue_loop and not current_detail_df.empty:
                        # Add current_detail_df to the list for the current Z value
                        if current_z not in grouped_detailed_dfs_for_analysis_raw:
                            grouped_detailed_dfs_for_analysis_raw[current_z] = []
                        grouped_detailed_dfs_for_analysis_raw[current_z].append(current_detail_df)


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

        if analyze_blue_loop:
            concise_detail_columns = [
                'initial_mass', 'initial_Z', 'star_age', 'model_number',
                'log_Teff', 'log_L', 'log_g', 'profile_number'
            ]
            # This loop is to save detail files for *each Z group*
            for z_val, dfs_list in grouped_detailed_dfs_for_analysis_raw.items():
                if dfs_list:
                    try:
                        # Concatenate all detail DataFrames for the current Z value
                        combined_df_bl = pd.concat(dfs_list, ignore_index=True)

                        # Filter columns based on blue_loop_output_type
                        if blue_loop_output_type == 'all':
                            filtered_combined_df_bl = combined_df_bl
                            output_type_label = "all columns"
                        else: # 'summary' output type
                            existing_desired_cols = [col for col in concise_detail_columns if col in combined_df_bl.columns]
                            if not existing_desired_cols:
                                logging.warning(f"No desired columns found for concise detail CSV for Z={z_val}. Skipping.")
                                continue
                            filtered_combined_df_bl = combined_df_bl[existing_desired_cols]
                            output_type_label = "selected columns"

                        detail_filename = os.path.join(detail_files_output_dir, f"detail_z{z_val:.4f}.csv")
                        filtered_combined_df_bl.to_csv(detail_filename, index=False)
                        logging.info(f"Written concatenated detail CSV for Z={z_val} with {output_type_label} to {detail_filename}")

                        # If reanalysis happens and analyze_blue_loop is true, we should populate combined_detail_data_for_plotting
                        # as a single DataFrame containing all data for eventual plotting.
                        if combined_detail_data_for_plotting.empty:
                            combined_detail_data_for_plotting = filtered_combined_df_bl.copy()
                        else:
                            combined_detail_data_for_plotting = pd.concat([combined_detail_data_for_plotting, filtered_combined_df_bl], ignore_index=True)

                    except Exception as e:
                        logging.error(f"Error writing detail CSV for Z={z_val}: {e}")
                else:
                    logging.info(f"No detailed data to write for Z={z_val}.")


    if should_generate_heatmaps:
        try:
            # If summary_df_loaded is empty, try to load it from the CSV file
            if 'summary_df_loaded' not in locals() or summary_df_loaded.empty:
                summary_df_loaded = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])

            if 'blue_loop_crossing_count' in summary_df_loaded.columns:
                cross_data_matrix_loaded = summary_df_loaded['blue_loop_crossing_count'].unstack(level='initial_mass')
                logging.info("Loaded existing summary data for heatmap generation.")

                unique_zs_for_heatmap = sorted(list(set(summary_df_loaded.index.get_level_values('initial_Z'))))
                unique_masses_for_heatmap = sorted(list(set(summary_df_loaded.index.get_level_values('initial_mass'))))

                generate_heatmaps_and_time_diff_csv(
                    cross_data_df=cross_data_matrix_loaded,
                    summary_csv_path=summary_csv_path,
                    unique_zs=unique_zs_for_heatmap,
                    unique_masses=unique_masses_for_heatmap,
                    plots_output_dir=plots_sub_dir, # This uses plots_sub_dir
                    analysis_results_output_dir=analysis_results_sub_dir,
                    model_name=os.path.basename(input_dir),
                    blue_loop_output_type=blue_loop_output_type,
                    analyze_blue_loop=analyze_blue_loop
                )
                logging.info("Heatmaps generated successfully.")
            else:
                logging.warning("Summary CSV lacks 'blue_loop_crossing_count'; skipping heatmap generation from existing data.")
        except Exception as e:
            logging.error(f"Error generating heatmaps: {e}. Use --force-reanalysis to retry analysis and generation.")

    # Plotting section for general HRD plots (Note: generate_general_hrd_plots is not imported/defined currently)
    if should_generate_plots:
        logging.warning("Attempted to generate general HRD plots, but 'generate_general_hrd_plots' is not currently imported or defined.")
        logging.warning("Please ensure 'generate_general_hrd_plots' is available if you wish to use the '--generate-plots' flag.")


    # --- Plotting section for blue loop specific plots with BCs ---
    if should_generate_blue_loop_plots_with_bc:
        try:
            # If combined_detail_data_for_plotting is still empty at this point,
            # it means either analyze_blue_loop was False AND detail files weren't loaded,
            # or there were no valid detail files to begin with.
            if combined_detail_data_for_plotting.empty:
                logging.info(f"Attempting to load detail files directly from {detail_files_output_dir} for plotting (if not already loaded during reanalysis)...")
                # load_and_group_data function now directly returns a single combined DataFrame
                combined_detail_data_for_plotting = load_and_group_data(detail_files_output_dir)

            if not combined_detail_data_for_plotting.empty:
                # Call the dedicated blue loop BC plotting function with the single combined DataFrame
                generate_blue_loop_plots_with_bc(
                    combined_df_all_data=combined_detail_data_for_plotting, # <--- This is the key change
                    output_dir=blue_loop_plots_bc_sub_dir,
                    output_type_label="all_blue_loop_data" # Added this for consistency with plotter function
                )
                logging.info("Blue loop specific plots with BCs generated successfully.")
            else:
                logging.warning("No blue loop detail data available for BC plots. Cannot generate plots.")

        except Exception as e:
            logging.error(f"Error generating blue loop specific plots with BCs: {e}")
            logging.error("Please ensure your 'blue_loop_cmd_plotter.py' is updated to expect 'combined_df_all_data' as its first parameter.")


if __name__ == "__main__":
    main()