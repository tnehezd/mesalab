# cli.py

import argparse
import os
import logging
import datetime
import yaml
import sys
import pandas as pd
from tqdm import tqdm
import re # Often useful for parsing run names, keeping it in case you add such logic.

# --- Import core logic module for MESA analysis ---
from mesalab.analysis import mesa_analyzer

# --- Import GYRE specific module ---
# This import assumes gyre_modules.py is in mesalab/gyretools/
from mesalab.gyretools import gyre_modules

# --- MIST Bolometric Correction Grid related imports (as seen in your logs) ---
# It's generally better to place this where it's used (e.g., in plotter.py)
# but if it's a global dependency for your project's environment setup, it can stay.
try:
    from MIST_BP_FEH import MIST_BP_FEH
    _bc_grid = MIST_BP_FEH()
    logging.info("MIST Bolometric Correction Grid initialized.")
except ImportError:
    _bc_grid = None
    logging.warning("MIST Bolometric Correction Grid not imported. Some visualizations might not be available.")

# --- Other optional imports (Holoviews, PyMultiNest) ---
try:
    # import holoviews as hv
    # hv.extension('bokeh')
    pass
except ImportError:
    logging.warning("Holoviews not imported. Some visualizations will not be available.")

try:
    # import pymultinest
    pass
except ImportError:
    logging.warning("PyMultiNest not imported. MultiNest fits will not work.")

# Configure the root logger. This will be adjusted based on config.
logger = logging.getLogger()
logger.setLevel(logging.INFO) # Default level, will be overridden by config


def main():
    parser = argparse.ArgumentParser(description="MESA Grid Analysis and Post-Processing Tool.")
    parser.add_argument('--config', type=str, default='config.yaml',
                        help='Path to the main configuration file (default: config.yaml).')
    args = parser.parse_args()

    if not os.path.exists(args.config):
        logging.critical(f"Configuration file '{args.config}' not found. Exiting.")
        sys.exit(1)

    with open(args.config, 'r') as f:
        full_config = yaml.safe_load(f)

    # --- Setup Logging from config ---
    # Extract output_dir from config for logging setup
    output_dir = full_config.get('general_settings', {}).get('output_dir', './mesagrid_output')
    os.makedirs(output_dir, exist_ok=True) # Ensure output directory exists before logging
    log_filename = f"mesagrid_run_{datetime.datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
    log_filepath = os.path.join(output_dir, log_filename)

    # Clear existing handlers to prevent duplicate logging if run multiple times in same session
    for handler in logger.handlers[:]:
        logger.removeHandler(handler)

    file_handler = logging.FileHandler(log_filepath)
    file_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))
    logger.addHandler(file_handler)

    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setFormatter(logging.Formatter('%(levelname)s: %(message)s'))
    logger.addHandler(console_handler)

    # Apply log level based on config's debug setting
    debug_mode = full_config.get('general_settings', {}).get('debug', False)
    if debug_mode:
        logger.setLevel(logging.DEBUG)
        logging.debug("Debug mode enabled.")
    else:
        logger.setLevel(logging.INFO)

    logging.info(f"Loaded configuration from: {args.config}")
    logging.info(f"Logging set up. Log file: {log_filepath}")
    logging.debug(f"Full configuration dictionary: {full_config}")


    # --- Call the main MESA analysis orchestrator from mesa_analyzer ---
    # mesa_analyzer will now perform the MESA data parsing, analysis, and plotting.
    logging.info("\n--- Starting MESA Grid Analysis ---")
    # The orchestrate_analysis in mesa_analyzer now returns the summary DataFrame
    # and the path to the generated GYRE input CSV.
    df_summary, gyre_input_csv_path = mesa_analyzer.orchestrate_analysis(full_config)

    if df_summary is None:
        logging.error("MESA analysis failed or returned no data. Cannot proceed with plots or GYRE workflow.")
        sys.exit(1) # Exit if no data for further steps


    # --- GYRE Workflow Integration ---
    # This section is now solely handled by cli.py, calling gyre_modules directly.
    if full_config.get('gyre_workflow', {}).get('run_gyre_workflow', False):
        logging.info("\n**GYRE Workflow requested. Initiating GYRE pipeline.**")
        # The gyre_config_path in config.yaml is relative to the config.yaml file's location
        gyre_config_file_path = os.path.join(os.path.dirname(args.config),
                                             full_config['gyre_workflow'].get('gyre_config_path', 'gyre_config.in'))

        if not os.path.exists(gyre_config_file_path):
            logging.critical(f"GYRE configuration file '{gyre_config_file_path}' not found. Cannot run GYRE workflow.")
            sys.exit(1)

        try:
            # Call gyre_modules directly with the full config and the generated CSV path.
            gyre_modules.run_gyre_workflow(full_config, gyre_input_csv_path)
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
        except Exception as e: # Catch all other exceptions, including subprocess.CalledProcessError
            logging.critical(f"GYRE Workflow: An unexpected error occurred: {e}", exc_info=True)
            if hasattr(e, 'stdout'): # For subprocess.CalledProcessError
                logging.critical(f"GYRE stdout: {e.stdout}")
            if hasattr(e, 'stderr'): # For subprocess.CalledProcessError
                logging.critical(f"GYRE stderr: {e.stderr}")
            sys.exit(1)
    else:
        logging.info("\nGYRE Workflow not requested (run_gyre_workflow = False in config). Skipping.")

    logging.info("\n--- MESA Grid Analysis Complete ---")


if __name__ == "__main__":
    main()
