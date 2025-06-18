import os
import sys
import logging
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt # Still needed if plotter uses it directly
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

# --- ACTUAL MODULE IMPORTS ---
# These imports assume the following structure within your 'mesalab' package:
# mesalab/bluelooptools/blue_loop_analyzer.py
# mesalab/plotting/plotter.py
# mesalab/gyre/gyre_modules.py
# And that each of these subdirectories contains an __init__.py file.
from mesalab.bluelooptools import blue_loop_analyzer
from mesalab.plotting import plotter
from mesalab.gyre import gyre_modules
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
            elif config.analyze_blue_loop and 'blue_loop_duration_yr' not in summary_df_raw.columns:
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

    # --- Plotting and GYRE Workflow based on config and analysis results ---
    if config.generate_heatmaps:
        if 'blue_loop_crossing_count' in summary_df_raw.columns:
            # Create a pivot table for heatmap generation
            cross_grid_df = summary_df_raw.pivot_table(
                index='initial_mass', columns='initial_Z', values='blue_loop_crossing_count'
            )
            cross_grid_df.to_csv(cross_grid_file_path)
            logger.info(f"Cross-grid summary saved to {cross_grid_file_path}")
            # Call the actual plotter module's heatmap function
            plotter.generate_heatmap(cross_grid_df, analysis_results_dir)
        else:
            logger.warning("Cannot generate heatmaps: 'blue_loop_crossing_count' column not found in summary data.")

    if config.generate_hr_diagrams != 'none':
        # Call the actual plotter module's HR diagram function
        plotter.generate_hr_diagrams(config.input_dir,
                                     config.output_dir,
                                     summary_df_raw,
                                     config.generate_hr_diagrams,
                                     config.generate_blue_loop_plots_with_bc)

    # Prepare data for GYRE input (if enabled)
    gyre_input_df = summary_df_raw[['initial_mass', 'initial_Z', 'run_dir_path', 'first_model_number', 'last_model_number']].copy()
    gyre_input_df.rename(columns={'run_dir_path': 'mesa_run_directory'}, inplace=True)

    # Ensure model numbers are integers for GYRE, handling NaNs
    gyre_input_df['first_model_number'] = gyre_input_df['first_model_number'].fillna(-1).astype(int)
    gyre_input_df['last_model_number'] = gyre_input_df['last_model_number'].fillna(-1).astype(int)

    gyre_input_df.to_csv(gyre_input_csv_path, index=False)
    logger.info(f"GYRE input CSV generated: {gyre_input_csv_path}")

    if config.run_gyre_workflow:
        logger.info("Starting GYRE workflow...")
        try:
            # Call the actual gyre_modules' run_gyre_workflow function
            gyre_modules.run_gyre_workflow(
                gyre_input_csv_path,
                config.gyre_config_path,
                os.path.join(output_base_dir, 'gyre_results')
            )
        except Exception as e:
            logger.error(f"GYRE workflow failed: {e}")

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
                # IMPORTANT: Calling analyze_blue_loop with 3 arguments as defined in blue_loop_analyzer.py
                loop_summary, df_loop_details = blue_loop_analyzer.analyze_blue_loop(df_full_history, current_mass, current_z)
                analysis_result_summary.update(loop_summary)

                # Save detailed blue loop data if output type is 'all'
                if config.blue_loop_output_type == 'all':
                    # Removed .replace('.', 'p') as requested
                    z_formatted = f"{current_z:.4f}"
                    detail_output_path = os.path.join(detail_files_output_dir, f"detail_z{z_formatted}.csv")
                    
                    # Append if file exists, create new if not
                    if os.path.exists(detail_output_path):
                        df_loop_details.to_csv(detail_output_path, mode='a', header=False, index=False)
                    else:
                        df_loop_details.to_csv(detail_output_path, index=False)
                    logger.debug(f"Saved blue loop details for Z={current_z} to {detail_output_path}")

            except Exception as e:
                logger.error(f"Error during blue loop analysis for M={current_mass}, Z={current_z}: {e}")
                analysis_result_summary['mesa_status'] = 'BLUE_LOOP_ANALYSIS_FAILED'
                # Ensure all blue loop related keys are set to NaN if analysis fails
                for key in ['blue_loop_crossing_count', 'blue_loop_duration_yr',
                             'blue_loop_start_age_yr', 'blue_loop_end_age_yr',
                             'min_log_Teff_bl', 'max_log_Teff_bl',
                             'min_log_L_bl', 'max_log_L_bl']: # Ensure these match keys in loop_summary
                    analysis_result_summary[key] = np.nan
        else:
            logger.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.")
            # Ensure blue loop related keys are set to NaN if analysis is skipped
            for key in ['blue_loop_crossing_count', 'blue_loop_duration_yr',
                         'blue_loop_start_age_yr', 'blue_loop_end_age_yr',
                         'min_log_Teff_bl', 'max_log_Teff_bl',
                         'min_log_L_bl', 'max_log_L_bl']:
                analysis_result_summary[key] = np.nan

        analysis_summary_data.append(analysis_result_summary)

    return analysis_summary_data
