# mesalab/mesa_analyzer.py (Module Version)

import os
import sys
import logging
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt # Still needed if plotter uses it directly
from tqdm import tqdm
from datetime import datetime

# Import data_reader functions directly
from mesalab.analyzis.data_reader import (
    extract_params_from_inlist,
    scan_mesa_runs,
    get_data_from_history_file
)

# Placeholder imports for other modules.
# These will be imported or their functions embedded/mocked by your main script.
# For now, I'll keep the try-except blocks with Dummy classes for self-containment
# if you don't have these files yet, but ideally, your main script sets them up.

logger = logging.getLogger(__name__) # Get a logger for this module


# --- Dummy Modules (Replace with your actual implementations) ---
# These are here so the code runs without errors if your other modules are missing.
# In a real setup, your main script would import and pass these, or they exist.

try:
    from mesalab.analyzis import blue_loop_analyzer # e.g., mesalab/analysis/blue_loop_analyzer.py
except ImportError:
    logger.warning("mesalab.analysis.blue_loop_analyzer not found. Using a dummy placeholder.")
    class DummyBlueLoopAnalyzer:
        def analyze_blue_loop(self, df_history, mass, z, config):
            logger.warning("DUMMY blue_loop_analyzer.analyze_blue_loop called.")
            return { 'blue_loop_crossing_count': 0, 'blue_loop_duration_yr': np.nan,
                     'blue_loop_start_age_yr': np.nan, 'blue_loop_end_age_yr': np.nan,
                     'min_log_Teff_bl': np.nan, 'max_log_Teff_bl': np.nan,
                     'min_log_L_bl': np.nan, 'max_log_L_bl': np.nan }, pd.DataFrame()
    blue_loop_analyzer = DummyBlueLoopAnalyzer()

try:
    from mesalab.plotting import plotter # e.g., mesalab/plotting/plotter.py
except ImportError:
    logger.warning("mesalab.plotting.plotter not found. Using a dummy placeholder.")
    class DummyPlotter:
        def generate_heatmap(self, df, output_dir):
            logger.warning("DUMMY plotter.generate_heatmap called.")
        def generate_hr_diagrams(self, input_dir, output_dir, summary_df, hr_option, bc_flag):
            logger.warning("DUMMY plotter.generate_hr_diagrams called.")
    plotter = DummyPlotter()

try:
    from mesalab.gyre import gyre_modules # e.g., mesalab/gyre/gyre_modules.py
except ImportError:
    logger.warning("mesalab.gyre.gyre_modules not found. Using a dummy placeholder.")
    class DummyGyreModules:
        def run_gyre_workflow(self, gyre_input_csv_path, gyre_config_path, gyre_output_dir):
            logger.warning("DUMMY gyre_modules.run_gyre_workflow called.")
            logger.info(f"DUMMY: Would run GYRE with input: {gyre_input_csv_path}, config: {gyre_config_path}, output to: {gyre_output_dir}")
    gyre_modules = DummyGyreModules()
# --- End Dummy Modules ---


def run_analysis_workflow(config):
    """
    Main entry point for the MESA analysis workflow when called as a module.
    Takes a parsed config object (Namespace) as input.
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

            if not config_run_paths.issubset(set(summary_df_raw['run_dir_path'].values)):
                logger.info("Not all MESA run directories from config found in existing summary file. Reanalysis will be performed.")
                reanalysis_needed = True
            elif config.analyze_blue_loop and 'blue_loop_duration_yr' not in summary_df_raw.columns:
                logger.info("Blue loop analysis enabled but required columns not found in existing summary. Reanalysis will be performed.")
                reanalysis_needed = True
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

    if config.generate_heatmaps:
        if 'blue_loop_crossing_count' in summary_df_raw.columns:
            cross_grid_df = summary_df_raw.pivot_table(
                index='initial_mass', columns='initial_Z', values='blue_loop_crossing_count'
            )
            cross_grid_df.to_csv(cross_grid_file_path)
            logger.info(f"Cross-grid summary saved to {cross_grid_file_path}")
            plotter.generate_heatmap(cross_grid_df, analysis_results_dir)
        else:
            logger.warning("Cannot generate heatmaps: 'blue_loop_crossing_count' column not found in summary data.")

    if config.generate_hr_diagrams != 'none':
        plotter.generate_hr_diagrams(config.input_dir,
                                    config.output_dir,
                                    summary_df_raw,
                                    config.generate_hr_diagrams,
                                    config.generate_blue_loop_plots_with_bc)

    gyre_input_df = summary_df_raw[['initial_mass', 'initial_Z', 'run_dir_path', 'first_model_number', 'last_model_number']].copy()
    gyre_input_df.rename(columns={'run_dir_path': 'mesa_run_directory'}, inplace=True)

    gyre_input_df['first_model_number'] = gyre_input_df['first_model_number'].fillna(-1).astype(int)
    gyre_input_df['last_model_number'] = gyre_input_df['last_model_number'].fillna(-1).astype(int)

    gyre_input_df.to_csv(gyre_input_csv_path, index=False)
    logger.info(f"GYRE input CSV generated: {gyre_input_csv_path}")

    if config.run_gyre_workflow:
        logger.info("Starting GYRE workflow...")
        try:
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
    (This function was part of the main workflow, kept separate for clarity)
    Performs the core MESA run analysis, extracting data and optionally blue loop info.
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

        if not df_full_history.empty and 'model_number' in df_full_history.columns:
            analysis_result_summary['first_model_number'] = df_full_history['model_number'].min()
            analysis_result_summary['last_model_number'] = df_full_history['model_number'].max()
        else:
            logger.warning(f"Could not determine first/last model_number for M={current_mass}, Z={current_z}. Setting to NaN.")
            analysis_result_summary['first_model_number'] = np.nan
            analysis_result_summary['last_model_number'] = np.nan

        if config.analyze_blue_loop:
            logger.debug(f"Analyzing blue loop for M={current_mass}, Z={current_z}")
            try:
                loop_summary, df_loop_details = blue_loop_analyzer.analyze_blue_loop(df_full_history, current_mass, current_z, config)
                analysis_result_summary.update(loop_summary)

                if config.blue_loop_output_type == 'all':
                    z_formatted = f"{current_z:.4f}"
                    detail_output_path = os.path.join(detail_files_output_dir, f"detail_z{z_formatted}.csv")
                    if os.path.exists(detail_output_path):
                        df_loop_details.to_csv(detail_output_path, mode='a', header=False, index=False)
                    else:
                        df_loop_details.to_csv(detail_output_path, index=False)
                    logger.debug(f"Saved blue loop details for Z={current_z} to {detail_output_path}")

            except Exception as e:
                logger.error(f"Error during blue loop analysis for M={current_mass}, Z={current_z}: {e}")
                analysis_result_summary['mesa_status'] = 'BLUE_LOOP_ANALYSIS_FAILED'
                for key in ['blue_loop_crossing_count', 'blue_loop_duration_yr',
                             'blue_loop_start_age_yr', 'blue_loop_end_age_yr',
                             'min_log_Teff_bl', 'max_log_Teff_bl',
                             'min_log_L_bl', 'max_log_L_bl']:
                    analysis_result_summary[key] = np.nan
        else:
            logger.info(f"Skipping blue loop analysis for M={current_mass}, Z={current_z} as analyze_blue_loop is False.")
            for key in ['blue_loop_crossing_count', 'blue_loop_duration_yr',
                         'blue_loop_start_age_yr', 'blue_loop_end_age_yr',
                         'min_log_Teff_bl', 'max_log_Teff_bl',
                         'min_log_L_bl', 'max_log_L_bl']:
                analysis_result_summary[key] = np.nan

        analysis_summary_data.append(analysis_result_summary)

    return analysis_summary_data

# Removed if __name__ == "__main__" block
