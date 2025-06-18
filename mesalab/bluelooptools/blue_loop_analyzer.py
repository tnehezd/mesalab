import os
import sys
import logging
import pandas as pd
import numpy as np
# import matplotlib.pyplot as plt # Commented out as per your request
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
# WE ARE NOW IMPORTING THE CORRECT FUNCTION NAME: analyze_blue_loop
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
    os.makedirs(analysis_results_dir, exist_ok=True)

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
    if reanalysis_needed:
        mesa_dirs_to_analyze = scan_mesa_runs(config.input_dir, config.inlist_name)
        if not mesa_dirs_to_analyze:
            logger.error("No valid MESA runs found to analyze. Exiting analysis.")
            return

        summary_data = perform_mesa_analysis(mesa_dirs_to_analyze, config)
        summary_df_raw = pd.DataFrame(summary_data)
        summary_df_raw.to_csv(summary_file_path, index=False)
        logger.info(f"Summary results saved to {summary_file_path}")
    else:
        logger.info("Reanalysis not needed. Loading existing summary data.")
        summary_df_raw = pd.read_csv(summary_file_path)

    if summary_df_raw.empty:
        logger.error("MESA analysis failed or returned no data. Cannot proceed with plots or GYRE workflow.")
        return

    # --- Plotting and GYRE Workflow (Conditional execution based on config) ---
    # These blocks are structured to run only if their respective config flags are true.
    # The actual plotting/GYRE module imports are commented at the top to avoid errors
    # if those modules are not present.

    # Example of how plotting would be called if uncommented
    # if config.generate_heatmaps:
    #     if 'crossing_count' in summary_df_raw.columns:
    #         cross_grid_df = summary_df_raw.pivot_table(
    #             index='initial_mass', columns='initial_Z', values='crossing_count'
    #         )
    #         cross_grid_df.to_csv(cross_grid_file_path)
    #         logger.info(f"Cross-grid summary saved to {cross_grid_file_path}")
    #         # plotter.generate_heatmap(cross_grid_df, analysis_results_dir)
    #     else:
    #         logger.warning("Cannot generate heatmaps: 'crossing_count' column not found in summary data. Make sure blue loop analysis was performed.")

    # if config.generate_hr_diagrams != 'none':
    #     # plotter.generate_hr_diagrams(config.input_dir,
    #     #                              config.output_dir,
    #     #                              summary_df_raw,
    #     #                              config.generate_hr_diagrams,
    #     #                              config.generate_blue_loop_plots_with_bc)
    #     pass # Placeholder if plotting is commented out

    # Prepare data for GYRE input (if enabled)
    # This block processes data for GYRE regardless of whether gyre_modules is imported,
    # but the actual GYRE run `gyre_modules.run_gyre_workflow` remains commented out.
    gyre_input_df = summary_df_raw[['initial_mass', 'initial_Z', 'run_dir_path', 'first_model_number', 'last_model_number']].copy()
    gyre_input_df.rename(columns={'run_dir_path': 'mesa_run_directory'}, inplace=True)

    # Ensure model numbers are integers for GYRE, handling NaNs
    gyre_input_df['first_model_number'] = gyre_input_df['first_model_number'].fillna(-1).astype(int)
    gyre_input_df['last_model_number'] = gyre_input_df['last_model_number'].fillna(-1).astype(int)

    gyre_input_df.to_csv(gyre_input_csv_path, index=False)
    logger.info(f"GYRE input CSV generated: {gyre_input_csv_path}")

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

    Args:
        mesa_dirs_to_analyze (list): A list of dictionaries, each containing
                                      'run_dir_path', 'history_file_path',
                                      'mass', 'z' for a MESA run.
        config (argparse.Namespace): The configuration object.

    Returns:
        list: A list of dictionaries, where each dictionary summarizes the
              analysis results for a single MESA run.
    """
    analysis_summary_data = []

    detail_files_output_dir = os.path.join(config.output_dir, 'detail_files')
    if config.analyze_blue_loop:
        os.makedirs(detail_files_output_dir, exist_ok=True)

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
            logger.debug(f"Analyzing blue loop for M={current_mass}, Z={current_z}")
            try:
                # Add 'log_R' to required columns check in mesa_analyzer too
                # This makes the check more robust before passing to blue_loop_analyzer
                required_cols_for_blue_loop = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g', 'center_he4']
                if 'log_R' in df_full_history.columns: # Check if log_R exists in the history_df, then add to required_cols for blue loop analysis if it might be used.
                    required_cols_for_blue_loop.append('log_R')

                missing_cols_before_blue_loop = [col for col in required_cols_for_blue_loop if col not in df_full_history.columns]
                if missing_cols_before_blue_loop:
                    logger.error(f"ERROR: Pre-check failed for M={current_mass}, Z={current_z}. Missing: {missing_cols_before_blue_loop}. Skipping blue loop analysis.")
                    analysis_result_summary['mesa_status'] = 'BLUE_LOOP_PREREQ_FAILED'
                    # Ensure blue loop related keys are set to NaN if analysis is skipped.
                    for key in ['crossing_count', 'blue_loop_duration_yr', 'max_log_L',
                                'max_log_Teff', 'max_log_R', 'first_model_number',
                                'last_model_number', 'first_age_yr', 'last_age_yr',
                                'blue_loop_start_age', 'blue_loop_end_age',
                                'instability_start_age', 'instability_end_age',
                                'calculated_blue_loop_duration', 'calculated_instability_duration']:
                        analysis_result_summary[key] = np.nan
                    if 'state_times' not in analysis_result_summary:
                        analysis_result_summary['state_times'] = {}
                    for skey in ['ms_end_age', 'min_teff_post_ms_age', 'first_is_entry_age',
                                 'first_is_exit_age', 'last_is_entry_age', 'last_is_exit_age',
                                 'instability_start_age', 'instability_end_age']:
                        analysis_result_summary['state_times'][skey] = np.nan
                    analysis_summary_data.append(analysis_result_summary)
                    continue


                # Call analyze_blue_loop which returns a single dictionary.
                # All results, including 'blue_loop_detail_df', are inside this dictionary.
                analysis_output = analyze_blue_loop(df_full_history, current_mass, current_z) # <-- THIS IS THE CORRECTED CALL

                # Update the summary with the results from the blue loop analysis.
                # We use .update() directly as analyze_blue_loop returns a dict with all necessary keys.
                analysis_result_summary.update(analysis_output)

                # Retrieve the detail DataFrame from the analysis_output dictionary
                # Use .get() with a default empty DataFrame to avoid KeyError if 'blue_loop_detail_df' is not present
                df_loop_details = analysis_output.get('blue_loop_detail_df', pd.DataFrame())

                # Save detailed blue loop data if output type is 'all' and the detail DF is not empty
                if config.blue_loop_output_type == 'all' and not df_loop_details.empty:
                    z_formatted = f"{current_z:.4f}"
                    detail_output_path = os.path.join(detail_files_output_dir, f"detail_M{current_mass:.1f}_Z{z_formatted}.csv") # More specific filename

                    # Append if file exists, create new if not
                    # Use header=False when appending to avoid duplicating header row
                    if os.path.exists(detail_output_path):
                        df_loop_details.to_csv(detail_output_path, mode='a', header=False, index=False)
                    else:
                        df_loop_details.to_csv(detail_output_path, index=False)
                    logger.debug(f"Saved blue loop details for M={current_mass}, Z={current_z} to {detail_output_path}")
                elif config.blue_loop_output_type == 'all' and df_loop_details.empty:
                    logger.info(f"No detailed blue loop data to save for M={current_mass}, Z={current_z} (DataFrame was empty).")


            except Exception as e:
                logger.error(f"Error during blue loop analysis for M={current_mass}, Z={current_z}: {e}")
                analysis_result_summary['mesa_status'] = 'BLUE_LOOP_ANALYSIS_FAILED'
                # Ensure all blue loop related keys are set to NaN if analysis fails.
                # These keys must match exactly what analyze_blue_loop returns (or aims to return as NaN).
                for key in ['crossing_count', 'blue_loop_duration_yr', 'max_log_L',
                            'max_log_Teff', 'max_log_R', 'first_model_number',
                            'last_model_number', 'first_age_yr', 'last_age_yr',
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

        else:
            logger.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.")
            # Ensure blue loop related keys are set to NaN if analysis is skipped (for consistency in summary DF)
            for key in ['crossing_count', 'blue_loop_duration_yr', 'max_log_L',
                        'max_log_Teff', 'max_log_R', 'first_model_number',
                        'last_model_number', 'first_age_yr', 'last_age_yr',
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

    return analysis_summary_data
