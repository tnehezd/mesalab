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

# --- Import core logic module from mesa_analyzer ---
# This is the corrected import. It assumes mesa_analyzer.py is directly in mesalab/
# If it's in mesalab/analysis/, change this to: from mesalab.analysis import mesa_analyzer
from mesalab import mesa_analyzer

# --- Adjust GYRE specific module import to match mesa_analyzer.py's dummy import path ---
# This import assumes gyre_modules.py is in mesalab/gyre/
# If your actual gyre_modules.py is in mesalab/gyretools/, you'll need to change this
# back to 'from mesalab.gyretools import gyre_modules' and update mesa_analyzer.py's dummy too.
try:
    from mesalab.gyre import gyre_modules # Matched with the path in mesa_analyzer.py
except ImportError:
    logging.warning("mesalab.gyre.gyre_modules not found in cli.py. Using a dummy placeholder.")
    # This dummy is for cli.py's direct use. mesa_analyzer.py has its own.
    class DummyGyreModulesCLI: # Renamed to avoid conflict if both were active
        def run_gyre_workflow(self, gyre_input_csv_path, gyre_config_path, gyre_output_dir):
            logging.warning("DUMMY gyre_modules.run_gyre_workflow called from cli.py.")
            logging.info(f"DUMMY CLI: Would run GYRE with input: {gyre_input_csv_path}, config: {gyre_config_path}, output to: {gyre_output_dir}")
    gyre_modules = DummyGyreModulesCLI()


# --- MIST Bolometric Correction Grid related imports (as seen in your logs) ---
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
    # Default config file name set to mysett.yaml as per your usage
    parser.add_argument('--config', type=str, default='mysett.yaml',
                        help='Path to the main configuration file (default: mysett.yaml).')
    args = parser.parse_args()

    if not os.path.exists(args.config):
        logging.critical(f"Configuration file '{args.config}' not found. Exiting.")
        sys.exit(1)

    with open(args.config, 'r') as f:
        full_config_dict = yaml.safe_load(f)

    # --- Setup Logging from config ---
    output_dir = full_config_dict.get('general_settings', {}).get('output_dir', './mesagrid_output')
    os.makedirs(output_dir, exist_ok=True)
    log_filename = f"mesagrid_run_{datetime.datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
    log_filepath = os.path.join(output_dir, log_filename)

    for handler in logger.handlers[:]:
        logger.removeHandler(handler)

    file_handler = logging.FileHandler(log_filepath)
    file_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))
    logger.addHandler(file_handler)

    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setFormatter(logging.Formatter('%(levelname)s: %(message)s'))
    logger.addHandler(console_handler)

    debug_mode = full_config_dict.get('general_settings', {}).get('debug', False)
    if debug_mode:
        logger.setLevel(logging.DEBUG)
        logging.debug("Debug mode enabled.")
    else:
        logger.setLevel(logging.INFO)

    logging.info(f"Loaded configuration from: {args.config}")
    logging.info(f"Logging set up. Log file: {log_filepath}")
    logging.debug(f"Full configuration dictionary: {full_config_dict}")


    # --- Convert dictionary to a Namespace object for consistency with mesa_analyzer.py's expected 'config' ---
    # The mesa_analyzer.py you provided expects 'config.input_dir' etc. so we convert.
    class ConfigNamespace:
        def __init__(self, dictionary):
            for k, v in dictionary.items():
                if isinstance(v, dict): # For nested dictionaries like general_settings
                    setattr(self, k, ConfigNamespace(v))
                else:
                    setattr(self, k, v)

    # Flatten nested dicts for direct access in mesa_analyzer.py, as your mesa_analyzer.py expects direct attributes
    # e.g., config.input_dir, not config.general_settings.input_dir
    flat_config = {}
    for section, settings in full_config_dict.items():
        if isinstance(settings, dict):
            for key, value in settings.items():
                flat_config[key] = value
        else:
            flat_config[section] = settings # For top-level keys like 'debug' if they existed

    # Also add the individual blue_loop_analysis, plotting_settings, gyre_workflow flags directly
    # for simpler access in run_analysis_workflow as it expects config.analyze_blue_loop etc.
    if 'blue_loop_analysis' in full_config_dict:
        for k, v in full_config_dict['blue_loop_analysis'].items():
            flat_config[k] = v
    if 'plotting_settings' in full_config_dict:
        for k, v in full_config_dict['plotting_settings'].items():
            flat_config[k] = v
    if 'gyre_workflow' in full_config_dict:
        for k, v in full_config_dict['gyre_workflow'].items():
            flat_config[k] = v

    config_for_analyzer = ConfigNamespace(flat_config)
    logging.debug(f"Config object for analyzer: {config_for_analyzer.__dict__}")


    # --- Call the main MESA analysis workflow from mesa_analyzer ---
    logging.info("\n--- Starting MESA Grid Analysis ---")
    # THE CRITICAL CHANGE: Call the correct function
    mesa_analyzer.run_analysis_workflow(config_for_analyzer) # Pass the Namespace object


    # The gyre workflow is now handled *within* run_analysis_workflow,
    # so we remove the redundant call here.
    # The summary_df and gyre_input_csv_path are now managed and saved by mesa_analyzer.py
    # and plotter functions are called from there.

    logging.info("\n--- MESA Grid Analysis Complete ---")


if __name__ == "__main__":
    main()
