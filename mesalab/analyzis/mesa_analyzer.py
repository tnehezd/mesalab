import os
import sys
import logging
import pandas as pd
import numpy as np
from tqdm import tqdm
from datetime import datetime

# Initialize logging for this module
logger = logging.getLogger(__name__)

# Import data_reader functions directly from the 'analyzis' package
from mesalab.analyzis.data_reader import (
    extract_params_from_inlist,
    scan_mesa_runs,
    get_data_from_history_file
)

# --- ACTUAL MODULE IMPORTS (Relative Imports) ---
# Assuming mesa_analyzer.py is in 'mesalab/analyzis/',
# this import navigates up one level to 'mesalab/' then down to the specific package.
# We import the 'analyze_blue_loop' function directly from blue_loop_analyzer module.
from mesalab.bluelooptools.blue_loop_analyzer import analyze_blue_loop
# from mesalab.plotting import plotter # Commented out to avoid plotting dependency errors
# from mesalab.gyre import gyre_modules # Commented out to avoid GYRE dependency errors
# --- END ACTUAL MODULE IMPORTS ---


def run_analysis_workflow(config):
    """
    Main entry point for the MESA analysis workflow when called as a module.
    Takes a parsed config object (Namespace) as input.

    Args:
        config (argparse.Namespace): A configuration object containing paths,
                                     flags for analysis steps, etc.
    """
    logger.info(f"MESA Grid Analysis started. Input directory: {config.input_dir}, Output directory: {config.output_dir}")
    logger.debug(f"Full config object for analysis: {config}")

    output_base_dir = config.output_dir
    analysis_results_dir = os.path.join(output_base_dir, 'analysis_results')
    detail_files_output_dir = os.path.join(output_base_dir, 'detail_files') # Define detail files output dir here
    os.makedirs(analysis_results_dir, exist_ok=True)
    os.makedirs(detail_files_output_dir, exist_ok=True) # Ensure detail_files directory exists

    summary_file_path = os.path.join(analysis_results_dir, 'mesa_grid_analysis_summary.csv')
    cross_grid_file_path = os.path.join(analysis_results_dir, 'mesa_grid_cross.csv')
    gyre_input_csv_path = os.path.join(analysis_results_dir, getattr(config, 'gyre_input_csv_name', 'sorted_mass_Z_min_max.csv'))

    reanalysis_needed = config.force_reanalysis
    if not reanalysis_needed and os.path.exists(summary_file_path):
        try:
            summary_df_raw = pd.read_csv(summary_file_path)

            mesa_dirs_from_config = scan_mesa_runs(config.input_dir, config.inlist_name)
            config_run_paths = {d['run_dir_path'] for d in mesa_dirs_from_config}

            # Check if all runs in config are present in the existing summary
            if not config_run_paths.issubset(set(summary_df_raw['run_dir_path'].values)):
                logger.info("Not all MESA run directories from config found in existing summary file. Reanalysis will be performed.")
                reanalysis_needed = True
            # Check if blue loop analysis is enabled and results are missing from summary
            # This check now looks for a key that the blue_loop_analyzer is guaranteed to provide.
            elif config.analyze_blue_loop and 'crossing_count' not in summary_df_raw.columns:
                logger.info("Blue loop analysis enabled but required columns not found in existing summary. Reanalysis will be performed.")
                reanalysis_needed = True
            # Check for GYRE related columns (first/last model_number)
            elif 'first_model_number' not in summary_df_raw.columns or summary_df_raw['first_model_number'].isnull().any():
                logger.info("GYRE model number range columns missing or incomplete in existing summary. Reanalysis will be performed.")
                reanalysis_needed = True
            else:
                logger.info("Existing summary file found and appears complete. Reanalysis not needed.")
                reanalysis_needed = False
        except Exception as e:
            logger.warning(f"Error loading existing summary file ({summary_file_path}). Reanalysis will be performed. Error: {e}")
            reanalysis_needed = True
    elif not os.path.exists(summary_file_path):
        logger.info("Summary file not found. Reanalysis will be performed.")
        reanalysis_needed = True

    logger.info(f"Analysis started. Reanalysis needed: {reanalysis_needed}")

    summary_df_raw = pd.DataFrame()
    # Initialize the dictionary for Z-grouped detailed DataFrames.
    # This will be populated during reanalysis if blue_loop_output_type is 'all'.
    grouped_detailed_dfs_for_saving = {} 

    if reanalysis_needed:
        mesa_dirs_to_analyze = scan_mesa_runs(config.input_dir, config.inlist_name)
        if not mesa_dirs_to_analyze:
            logger.error("No valid MESA runs found to analyze. Exiting analysis.")
            return

        # Perform the core MESA analysis and get back summary data and collected detail DataFrames
        summary_data, grouped_detailed_dfs_for_saving = perform_mesa_analysis(mesa_dirs_to_analyze, config)
        
        summary_df_raw = pd.DataFrame(summary_data)
        
        # --- Save Summary CSV ---
        summary_df_raw.to_csv(summary_file_path, index=False)
        logger.info(f"Summary results saved to {summary_file_path}")

        # --- Save Cross-Grid CSV (Heatmap data) ---
        if config.generate_heatmaps: # Only generate if heatmaps are requested
            if 'crossing_count' in summary_df_raw.columns:
                # Filter for runs that actually have blue loops if analyze_blue_loop is True
                if config.analyze_blue_loop:
                    cross_grid_data = summary_df_raw[summary_df_raw['crossing_count'] > 0]
                else:
                    cross_grid_data = summary_df_raw # Use all data if no blue loop filtering
                
                if not cross_grid_data.empty:
                    cross_grid_df = cross_grid_data.pivot_table(
                        index='initial_Z', columns='initial_mass', values='crossing_count'
                    )
                    # Ensure all unique Zs and masses are present in the index/columns for consistency
                    unique_zs = sorted(summary_df_raw['initial_Z'].unique())
                    unique_masses = sorted(summary_df_raw['initial_mass'].unique())
                    
                    cross_grid_df = cross_grid_df.reindex(index=unique_zs, columns=unique_masses)
                    cross_grid_df.to_csv(cross_grid_file_path, na_rep='NaN')
                    logger.info(f"Cross-grid summary saved to {cross_grid_file_path}")
                else:
                    logger.warning("No data with blue loops to build cross-grid matrix.")
            else:
                logger.warning("Cannot generate cross-grid: 'crossing_count' column not found in summary data. Make sure blue loop analysis was performed.")

        # --- Save Z-grouped Detail CSVs to disk ---
        # This block ensures all detailed blue loop data is saved together per Z, after processing all runs.
        if config.analyze_blue_loop and config.blue_loop_output_type == 'all':
            for z_val, dfs_list in grouped_detailed_dfs_for_saving.items():
                if dfs_list:
                    try:
                        combined_df_bl = pd.concat(dfs_list, ignore_index=True)
                        # Explicitly sort by initial_mass and star_age for consistent output
                        combined_df_bl = combined_df_bl.sort_values(by=['initial_mass', 'star_age']).reset_index(drop=True)
                        
                        detail_filename = os.path.join(detail_files_output_dir, f"detail_z{z_val:.4f}.csv")
                        combined_df_bl.to_csv(detail_filename, index=False, na_rep='NaN')
                        logger.info(f"Written concatenated detail CSV for Z={z_val} to {detail_filename}")
                    except Exception as e:
                        logger.error(f"Error writing detail CSV for Z={z_val}: {e}")
                else:
                    logger.info(f"No detailed data to write for Z={z_val}.")
        elif config.analyze_blue_loop and config.blue_loop_output_type == 'summary':
            logger.info("Blue loop output type is 'summary'. Detailed blue loop CSVs will not be saved.")
        else:
            logger.info("Blue loop analysis is not enabled. Detailed blue loop CSVs will not be saved.")
            
    else: # If reanalysis is NOT needed
        logger.info("Reanalysis not needed. Loading existing summary data.")
        summary_df_raw = pd.read_csv(summary_file_path)
        # Note: If reanalysis is skipped, Z-grouped detail files are NOT regenerated by this module.
        # They are assumed to exist from a previous run if needed for plotting or later stages.

    if summary_df_raw.empty:
        logger.error("MESA analysis failed or returned no data. Cannot proceed with plots or GYRE workflow.")
        return

    # --- Plotting and GYRE Workflow (Conditional execution based on config) ---
    # These blocks are structured to run only if their respective config flags are true.
    # The actual plotting/GYRE module imports are commented at the top to avoid errors
    # if those modules are not present.

    # Example of how plotting would be called if uncommented
    # if config.generate_heatmaps:
    #    # plotter.generate_heatmap(cross_grid_df, analysis_results_dir)
    #    pass
    #
    # if config.generate_hr_diagrams != 'none':
    #    # plotter.generate_hr_diagrams(config.input_dir,
    #    #                             config.output_dir,
    #    #                             summary_df_raw,
    #    #                             config.generate_hr_diagrams,
    #    #                             config.generate_blue_loop_plots_with_bc)
    #    pass # Placeholder if plotting is commented out

    # Prepare data for GYRE input (if enabled)
    # This block processes data for GYRE regardless of whether gyre_modules is imported,
    # but the actual GYRE run `gyre_modules.run_gyre_workflow` remains commented out.
    
    # Filter summary_df_raw to include only successful runs for GYRE input
    # Assuming 'mesa_status' indicates success ('OK').
    gyre_input_base_df = summary_df_raw[summary_df_raw['mesa_status'] == 'OK'].copy()

    if not gyre_input_base_df.empty:
        gyre_input_df = gyre_input_base_df[['initial_mass', 'initial_Z', 'run_dir_path', 'first_model_number', 'last_model_number']].copy()
        gyre_input_df.rename(columns={'run_dir_path': 'mesa_run_directory'}, inplace=True)

        # Ensure model numbers are integers for GYRE, handling NaNs
        gyre_input_df['first_model_number'] = gyre_input_df['first_model_number'].fillna(-1).astype(int)
        gyre_input_df['last_model_number'] = gyre_input_df['last_model_number'].fillna(-1).astype(int)
        
        # Ensure 'initial_Z' is formatted as string for consistency with GYRE expectations
        gyre_input_df['initial_Z'] = gyre_input_df['initial_Z'].apply(lambda x: f"{x:.4f}")

        gyre_input_df.to_csv(gyre_input_csv_path, index=False)
        logger.info(f"GYRE input CSV generated: {gyre_input_csv_path}")
    else:
        logger.info("No successful MESA runs found to generate GYRE input CSV.")


    # if config.run_gyre_workflow:
    #     logger.info("Starting GYRE workflow (if gyre_modules is imported and uncommented)...")
    #     try:
    #         # gyre_modules.run_gyre_workflow(
    #         #     gyre_input_csv_path,
    #         #     config.gyre_config_path,
    #         #     os.path.join(output_base_dir, 'gyre_results')
    #         # )
    #         pass # Placeholder if GYRE is commented out
    #     except Exception as e:
    #         logger.error(f"GYRE workflow failed: {e}")

    logger.info("MESA Grid Analysis finished.")


def perform_mesa_analysis(mesa_dirs_to_analyze, config):
    """
    Performs the core MESA run analysis for each specified MESA run,
    extracting history data and optionally blue loop information.
    Collects detailed blue loop DataFrames grouped by Z for later saving.

    Args:
        mesa_dirs_to_analyze (list): A list of dictionaries, each containing
                                      'run_dir_path', 'history_file_path',
                                      'mass', 'z' for a MESA run.
        config (argparse.Namespace): The configuration object.

    Returns:
        tuple: A tuple containing:
            - list: A list of dictionaries, where each dictionary summarizes the
                    analysis results for a single MESA run.
            - dict: A dictionary where keys are metallicities (Z) and values are
                    lists of blue loop detail DataFrames (pd.DataFrame) for that Z.
                    This is used for saving Z-grouped detail CSVs after the loop.
    """
    analysis_summary_data = []
    # Dictionary to collect detailed blue loop DataFrames, grouped by Z
    # E.g., {Z_value: [df_for_mass1, df_for_mass2], ...}
    grouped_detailed_dfs = {} 

    for run_info in tqdm(mesa_dirs_to_analyze, desc="Performing MESA Run Analysis"):
        run_path = run_info['run_dir_path']
        current_mass = run_info.get('mass')
        current_z = run_info.get('z')

        history_file_path = run_info['history_file_path']
        df_full_history = get_data_from_history_file(history_file_path)

        analysis_result_summary = {
            'initial_mass': current_mass,
            'initial_Z': current_z,
            'run_dir_path': run_path,
            'mesa_status': 'OK'
        }

        if df_full_history.empty:
            logger.warning(f"No history data found for M={current_mass}, Z={current_z} at {run_path}. Skipping analysis for this run.")
            analysis_result_summary['mesa_status'] = 'NO_HISTORY_DATA'
            analysis_summary_data.append(analysis_result_summary)
            continue

        # Extract first and last model numbers for GYRE
        if not df_full_history.empty and 'model_number' in df_full_history.columns:
            analysis_result_summary['first_model_number'] = df_full_history['model_number'].min()
            analysis_result_summary['last_model_number'] = df_full_history['model_number'].max()
        else:
            logger.warning(f"Could not determine first/last model_number for M={current_mass}, Z={current_z}. Setting to NaN.")
            analysis_result_summary['first_model_number'] = np.nan
            analysis_result_summary['last_model_number'] = np.nan

        # Perform blue loop analysis if enabled
        if config.analyze_blue_loop:
            # logger.debug(f"Analyzing blue loop for M={current_mass}, Z={current_z}") # Suppressed
            try:
                # Call analyze_blue_loop which returns a single dictionary.
                # All results, including 'blue_loop_detail_df', are inside this dictionary.
                analysis_output = analyze_blue_loop(df_full_history, current_mass, current_z)

                # Update the summary with the results from the blue loop analysis.
                # We use .update() directly as analyze_blue_loop returns a dict with all necessary keys.
                analysis_result_summary.update(analysis_output)

                # Retrieve the detail DataFrame from the analysis_output dictionary
                df_loop_details = analysis_output.get('blue_loop_detail_df', pd.DataFrame())

                # Collect detailed blue loop data grouped by Z if 'all' output type is requested
                if config.blue_loop_output_type == 'all' and not df_loop_details.empty:
                    # Add initial_mass and initial_Z to the detail DataFrame before storing
                    df_loop_details['initial_mass'] = current_mass
                    df_loop_details['initial_Z'] = current_z
                    
                    if current_z not in grouped_detailed_dfs:
                        grouped_detailed_dfs[current_z] = []
                    grouped_detailed_dfs[current_z].append(df_loop_details)
                    # logger.debug(f"Collected blue loop details for M={current_mass}, Z={current_z} for Z-grouped saving.") # Suppressed
                # elif config.blue_loop_output_type == 'all' and df_loop_details.empty: # Suppressed
                #     logger.info(f"No detailed blue loop data to collect for M={current_mass}, Z={current_z} (DataFrame was empty).") # Suppressed

            except Exception as e:
                logger.error(f"Error during blue loop analysis for M={current_mass}, Z={current_z}: {e}")
                analysis_result_summary['mesa_status'] = 'BLUE_LOOP_ANALYSIS_FAILED'
                # Ensure all blue loop related keys are set to NaN if analysis fails.
                # These keys must match exactly what analyze_blue_loop returns (or aims to return as NaN).
                for key in ['crossing_count', 'blue_loop_duration_yr', 'max_log_L',
                            'max_log_Teff', 'max_log_R', 
                            'first_age_yr', 'last_age_yr',
                            'blue_loop_start_age', 'blue_loop_end_age',
                            'instability_start_age', 'instability_end_age',
                            'calculated_blue_loop_duration', 'calculated_instability_duration']:
                    analysis_result_summary[key] = np.nan
                # Also ensure 'state_times' sub-dictionary keys are NaN if the main analysis failed
                if 'state_times' not in analysis_result_summary:
                    analysis_result_summary['state_times'] = {}
                for skey in ['ms_end_age', 'min_teff_post_ms_age', 'first_is_entry_age',
                             'first_is_exit_age', 'last_is_entry_age', 'last_is_exit_age',
                             'instability_start_age', 'instability_end_age']:
                    analysis_result_summary['state_times'][skey] = np.nan

        else: # if config.analyze_blue_loop is False
            # logger.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.") # Suppressed
            # Ensure blue loop related keys are set to NaN if analysis is skipped (for consistency in summary DF)
            for key in ['crossing_count', 'blue_loop_duration_yr', 'max_log_L',
                        'max_log_Teff', 'max_log_R', 
                        'first_age_yr', 'last_age_yr',
                        'blue_loop_start_age', 'blue_loop_end_age',
                        'instability_start_age', 'instability_end_age',
                        'calculated_blue_loop_duration', 'calculated_instability_duration']:
                analysis_result_summary[key] = np.nan
            # Ensure 'state_times' is present but with NaN values if skipped
            analysis_result_summary['state_times'] = {
                'ms_end_age': np.nan, 'min_teff_post_ms_age': np.nan,
                'first_is_entry_age': np.nan, 'first_is_exit_age': np.nan,
                'last_is_entry_age': np.nan, 'last_is_exit_age': np.nan,
                'instability_start_age': np.nan, 'instability_end_age': np.nan
            }

        analysis_summary_data.append(analysis_result_summary)

    return analysis_summary_data, grouped_detailed_dfs
