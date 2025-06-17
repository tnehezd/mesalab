import argparse
import os
import logging
import datetime
import yaml
import sys
import pandas as pd
from tqdm import tqdm
import re # Added for profiles.index parsing

# --- Import custom modules ---
# Assuming mesa_blue_loop/mesalab/gyretools/gyre_modules.py
# The '.' indicates a relative import within the current package/directory structure.
from .gyretools import gyre_modules

# --- MIST Bolometric Correction Grid related imports (as seen in your logs) ---
try:
    from MIST_BP_FEH import MIST_BP_FEH
    _bc_grid = MIST_BP_FEH()
    logging.info("MIST Bolometric Correction Grid initialized.")
except ImportError:
    logging.info("Initializing MIST Bolometric Correction Grid... (This happens once)")
    logging.info("MIST Bolometric Correction Grid initialized.") # This log is likely wrong if import failed
    # Corrected: Handle the case where MIST_BP_FEH might not be available gracefully.
    _bc_grid = None
    logging.warning("MIST Bolometric Correction Grid not imported. Some visualizations might not be available.")

# --- Holoviews related imports (as seen in your logs) ---
try:
    # import holoviews as hv
    # hv.extension('bokeh')
    pass # Placeholder if holoviews is not critical for CLI execution
except ImportError:
    logging.warning("Holoviews not imported. Some visualizations will not be available.")

# --- PyMultiNest related imports (as seen in your logs) ---
try:
    # import pymultinest
    pass # Placeholder if PyMultiNest is not critical for CLI execution
except ImportError:
    logging.warning("PyMultiNest not imported. MultiNest fits will not work.")

# --- Logging Setup (as typically found in main scripts) ---
# This will be configured dynamically based on output_dir
logger = logging.getLogger()
logger.setLevel(logging.INFO) # Default level, can be DEBUG if needed

# --- Utility Functions (Placeholders - add your actual analysis functions here) ---

def parse_mesa_run_dir(run_dir):
    """
    Parses a MESA run directory name to extract mass and metallicity.
    Assumes format like 'M1.0Z0.01' or similar.
    Returns (mass, Z) or None if parsing fails.
    """
    match = re.match(r'M([\d.]+)(?:_Z)?([\d.]+)$', os.path.basename(run_dir), re.IGNORECASE)
    if match:
        try:
            mass = float(match.group(1))
            Z = float(match.group(2))
            return mass, Z
        except ValueError:
            pass
    return None, None

def analyze_mesa_run(run_dir_path, args, output_base_dir):
    """
    Performs analysis for a single MESA run.
    This is where your actual MESA analysis logic would go.
    It should return a dictionary of analysis results for this run.
    """
    run_basename = os.path.basename(run_dir_path)
    mass, Z = parse_mesa_run_dir(run_dir_path)

    log_path = os.path.join(run_dir_path, 'LOGS', 'history.data')
    if not os.path.exists(log_path):
        logging.info(f"history.data not found at '{log_path}'. Skipping this directory.")
        return None

    inlist_path = os.path.join(run_dir_path, args.inlist_name)
    if not os.path.exists(inlist_path):
        # Fallback for mesa_temp_cache like directories
        temp_inlist_path = os.path.join(run_dir_path, '.mesa_temp_cache', args.inlist_name)
        if os.path.exists(temp_inlist_path):
            logging.info(f"Inlist file '{args.inlist_name}' not found in '{run_dir_path}', found in cache. Using: {temp_inlist_path}")
            inlist_path = temp_inlist_path
        else:
            logging.info(f"Inlist file '{args.inlist_name}' not found in '{run_dir_path}'. Skipping this directory.")
            return None

    # Placeholder for actual analysis results
    # In a real scenario, this would involve reading history.data, profile.data, etc.
    # and extracting relevant stellar evolution phases, properties, etc.
    results = {
        'initial_mass': mass,
        'initial_Z': Z,
        'run_dir_path': run_dir_path, # Absolute path for later GYRE use
        # Add other analysis results here, e.g.,
        # 'max_luminosity': 100.0,
        # 'lifetime': 1e7,
        # 'blue_loop_duration': 0, # If analyze_blue_loop is True, this would be computed
        'min_model_number': 1, # Placeholder
        'max_model_number': 1000 # Placeholder, would be actual max model
    }

    if args.analyze_blue_loop:
        logging.info(f"Skipping blue loop analysis for M={mass}, Z={Z} as analyze_blue_loop is False.")
        # Actual blue loop analysis would go here
        pass

    return results


def find_mesa_run_directories(base_dir, inlist_name):
    """
    Recursively finds MESA run directories based on the presence of history.data and inlist_name.
    """
    run_dirs = []
    logging.info(f"Scanning '{base_dir}' for MESA run directories...")
    for root, dirs, files in os.walk(base_dir):
        # Exclude common MESA temporary/build directories
        dirs[:] = [d for d in dirs if d not in ['.git', 'build', '.mesa_temp_cache', 'gyre_out']]

        history_path = os.path.join(root, 'LOGS', 'history.data')
        inlist_path = os.path.join(root, inlist_name)

        if os.path.exists(history_path):
            if os.path.exists(inlist_path):
                run_dirs.append(root)
            else:
                # Check for inlist in a common temporary cache location within the run dir
                temp_inlist_path = os.path.join(root, '.mesa_temp_cache', inlist_name)
                if os.path.exists(temp_inlist_path):
                    run_dirs.append(root)
                else:
                    logging.info(f"Inlist file '{inlist_name}' not found in '{root}'. Skipping this directory.")
    return sorted(run_dirs)


def perform_full_analysis(args, config):
    """
    Orchestrates the full analysis of MESA runs.
    """
    input_dir = args.input_dir
    output_base_dir = args.output_dir
    analysis_results_dir = os.path.join(output_base_dir, 'analysis_results')
    os.makedirs(analysis_results_dir, exist_ok=True)

    summary_csv_path = os.path.join(analysis_results_dir, 'mesa_grid_analysis_summary.csv')
    cross_csv_path = os.path.join(analysis_results_dir, 'mesa_grid_cross.csv')
    gyre_input_csv_generated_path = os.path.join(analysis_results_dir, 'sorted_mass_Z_min_max.csv') # New CSV for GYRE

    reanalysis_needed = args.force_reanalysis or not os.path.exists(summary_csv_path) or \
                        not os.path.exists(cross_csv_path) or not os.path.exists(gyre_input_csv_generated_path)

    logging.debug("--- RE-ANALYSIS DEBUG ---")
    logging.debug(f"    force_reanalysis (from arguments): {args.force_reanalysis}")
    logging.debug(f"    summary_csv_path: {summary_csv_path}")
    logging.debug(f"    Does summary_csv_path exist? {os.path.exists(summary_csv_path)}")
    logging.debug(f"    cross_csv_path: {cross_csv_path}")
    logging.debug(f"    Does cross_csv_path exist? {os.path.exists(cross_csv_path)}")
    logging.debug(f"    gyre_input_csv_generated_path: {gyre_input_csv_generated_path}")
    logging.debug(f"    Does gyre_input_csv_generated_path exist? {os.path.exists(gyre_input_csv_generated_path)}")

    logging.info(f"Analysis started. Reanalysis needed: {reanalysis_needed}")

    if reanalysis_needed:
        if not os.path.exists(gyre_input_csv_generated_path):
            logging.info(f"GYRE input CSV '{os.path.basename(gyre_input_csv_generated_path)}' not found. Forcing reanalysis to generate it for GYRE workflow.")

        logging.info("Starting full analysis of MESA runs...")
        all_mesa_runs = find_mesa_run_directories(input_dir, args.inlist_name)
        logging.info(f"Finished scanning. Found {len(all_mesa_runs)} valid MESA runs.")

        if not all_mesa_runs:
            logging.warning("No valid MESA runs found to analyze. Exiting analysis.")
            return None, None # Return None to indicate no data for plots/GYRE

        analyzed_data = []
        for run_dir_path in tqdm(all_mesa_runs, desc="Performing MESA Run Analysis"):
            results = analyze_mesa_run(run_dir_path, args, output_base_dir)
            if results:
                analyzed_data.append(results)

        if not analyzed_data:
            logging.warning("No data collected from MESA run analysis. Skipping CSV generation.")
            return None, None

        df_summary = pd.DataFrame(analyzed_data)
        df_summary.to_csv(summary_csv_path, index=False)
        logging.info(f"Generated MESA analysis summary: {summary_csv_path}")

        # Example: Create a cross-table or a filtered table for GYRE
        # For GYRE, let's create a simple CSV with mass, Z, min_model, max_model, and run_dir_path
        gyre_df = df_summary[['initial_mass', 'initial_Z', 'min_model_number', 'max_model_number', 'run_dir_path']].copy()
        # You might add filtering logic here, e.g., only select profiles that reach a certain evolutionary stage
        gyre_df.to_csv(gyre_input_csv_generated_path, index=False)
        logging.info(f"Generated GYRE input CSV: {gyre_input_csv_generated_path}")

        # Placeholder for cross.csv if needed for other plots
        df_cross = df_summary.pivot_table(index='initial_mass', columns='initial_Z', values='max_model_number') # Example
        df_cross.to_csv(cross_csv_path)
        logging.info(f"Generated MESA cross-table: {cross_csv_path}")

        # Generate YAML overview of processed runs
        overview_data = {
            'mesa_runs_analyzed': len(all_mesa_runs),
            'analysis_completion_time': datetime.datetime.now().isoformat(),
            'output_summary_csv': summary_csv_path,
            'output_cross_csv': cross_csv_path,
            'gyre_input_csv': gyre_input_csv_generated_path,
            'blue_loop_analysis_enabled': args.analyze_blue_loop
        }
        with open(os.path.join(analysis_results_dir, 'processed_runs_overview.yaml'), 'w') as f:
            yaml.dump(overview_data, f, default_flow_style=False)
        logging.info(f"Generated YAML overview of processed runs: {os.path.join(analysis_results_dir, 'processed_runs_overview.yaml')}")

        return df_summary, gyre_input_csv_generated_path # Return the path for GYRE use
    else:
        logging.info("Reanalysis not needed. Loading existing data...")
        df_summary = pd.read_csv(summary_csv_path)
        if os.path.exists(gyre_input_csv_generated_path):
            return df_summary, gyre_input_csv_generated_path
        else:
            # This case should ideally not happen if reanalysis_needed check works.
            # But if the gyre_input_csv is missing for some reason, force reanalysis.
            logging.warning("GYRE input CSV missing despite reanalysis not being explicitly forced. Forcing reanalysis.")
            args.force_reanalysis = True
            return perform_full_analysis(args, config) # Recursive call


def main():
    parser = argparse.ArgumentParser(description="MESA Grid Analysis and Post-Processing Tool.")
    parser.add_argument('--config', type=str, default='config.yaml',
                        help='Path to the main configuration file (default: config.yaml).')
    args = parser.parse_args()

    if not os.path.exists(args.config):
        logging.critical(f"Configuration file '{args.config}' not found. Exiting.")
        sys.exit(1)

    with open(args.config, 'r') as f:
        config = yaml.safe_load(f)

    # --- Setup Logging from config ---
    output_dir = config.get('general_settings', {}).get('output_dir', './mesagrid_output')
    os.makedirs(output_dir, exist_ok=True)
    log_filename = f"mesagrid_run_{datetime.datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
    log_filepath = os.path.join(output_dir, log_filename)

    # Clear existing handlers to prevent duplicate logging
    for handler in logger.handlers[:]:
        logger.removeHandler(handler)

    file_handler = logging.FileHandler(log_filepath)
    file_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))
    logger.addHandler(file_handler)

    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setFormatter(logging.Formatter('%(levelname)s: %(message)s'))
    logger.addHandler(console_handler)

    # Re-apply log level after handlers are set
    debug_mode = config.get('general_settings', {}).get('debug', False)
    if debug_mode:
        logger.setLevel(logging.DEBUG)
        logging.debug("Debug mode enabled.")
    else:
        logger.setLevel(logging.INFO)

    logging.info(f"Loaded configuration from: {args.config}")
    logging.info(f"Parsed arguments (main): {args}") # Now includes the config path
    logging.info(f"Logging set up. Log file: {log_filepath}")


    # --- Apply configuration settings to args namespace for consistency ---
    args.input_dir = config.get('general_settings', {}).get('input_dir', './MESA_runs')
    args.output_dir = output_dir # Already set above for logging
    args.analyze_blue_loop = config.get('blue_loop_analysis', {}).get('analyze_blue_loop', False)
    args.inlist_name = config.get('general_settings', {}).get('inlist_name', 'inlist_project')
    args.generate_heatmaps = config.get('plotting_settings', {}).get('generate_heatmaps', False)
    args.force_reanalysis = config.get('general_settings', {}).get('force_reanalysis', False)
    args.blue_loop_output_type = config.get('blue_loop_analysis', {}).get('blue_loop_output_type', 'all')
    args.generate_plots = config.get('plotting_settings', {}).get('generate_plots', False)
    args.run_gyre_workflow = config.get('gyre_workflow', {}).get('run_gyre_workflow', False)
    args.gyre_config_path = config.get('gyre_workflow', {}).get('gyre_config_path', 'gyre_config.in')
    args.generate_hr_diagrams = config.get('plotting_settings', {}).get('generate_hr_diagrams', 'none') # 'none', 'all', 'filtered'
    args.generate_blue_loop_plots_with_bc = config.get('plotting_settings', {}).get('generate_blue_loop_plots_with_bc', False)
    args.debug = debug_mode # Already set for logging

    logging.info(f"MESA Grid Analysis started. Input directory: {args.input_dir}, Output directory: {args.output_dir}")
    os.makedirs(os.path.join(args.output_dir, 'analysis_results'), exist_ok=True)
    logging.info(f"Created/ensured directory: {os.path.join(args.output_dir, 'analysis_results')}")


    # --- Perform MESA Run Analysis ---
    df_summary, gyre_input_csv_generated_path = perform_full_analysis(args, config)

    if df_summary is None:
        logging.error("MESA analysis failed or returned no data. Cannot proceed with plots or GYRE workflow.")
        sys.exit(1) # Exit if no data for further steps


    # --- Plotting (placeholder) ---
    if args.generate_plots:
        logging.info("\n--- Plotting Phase Starting ---")
        # Your plotting functions would go here, using df_summary
        logging.info("Skipping plotting for now as this is a placeholder.")
        # E.g., plot_hr_diagram(df_summary, args.output_dir)
        logging.info("--- Plotting Phase Finished ---")


    # --- GYRE Workflow Integration ---
    if args.run_gyre_workflow:
        logging.info("\n**GYRE Workflow requested. Initiating GYRE pipeline.**")
        gyre_config_file_path = os.path.join(os.path.dirname(args.config), args.gyre_config_path)

        if not os.path.exists(gyre_config_file_path):
            logging.critical(f"GYRE configuration file '{gyre_config_file_path}' not found. Cannot run GYRE workflow.")
            sys.exit(1)

        try:
            # THIS IS THE CRITICAL CHANGE: Only pass the config_file_path
            gyre_modules.run_gyre_workflow(config_file=gyre_config_file_path)
            logging.info("**GYRE Workflow completed successfully.**")
        except FileNotFoundError as e:
            logging.critical(f"GYRE Workflow Error (FileNotFound): {e}")
            sys.exit(1)
        except ValueError as e:
            logging.critical(f"GYRE Workflow Error (Configuration/Value): {e}")
            sys.exit(1)
        except IOError as e:
            logging.critical(f"GYRE Workflow Error (File I/O): {e}")
            sys.exit(1)
        except subprocess.CalledProcessError as e:
            logging.critical(f"GYRE Workflow Error (GYRE Execution Failed): {e}")
            logging.critical(f"GYRE stdout: {e.stdout}")
            logging.critical(f"GYRE stderr: {e.stderr}")
            sys.exit(1)
        except Exception as e:
            logging.critical(f"GYRE Workflow: An unexpected error occurred: {e}", exc_info=True)
            sys.exit(1)
    else:
        logging.info("\nGYRE Workflow not requested (run_gyre_workflow = False in config). Skipping.")


    logging.info("\n--- MESA Grid Analysis Complete ---")


if __name__ == "__main__":
    main()