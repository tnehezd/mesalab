import os
import argparse
import pandas as pd
import numpy as np
import re
from tqdm import tqdm
import yaml
import logging
import sys

# --- GLOBAL LOGGING CONFIGURATION ---
logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s') # Changed to INFO as default
logging.getLogger('matplotlib').setLevel(logging.ERROR) # Suppress Matplotlib's non-error messages
# -----------------------------------------------------

# Import functions from your specialized modules
from .config_parser import parsing_options
from .data_reader import extract_params_from_inlist, scan_mesa_runs, get_data_from_history_file
from .blue_loop_analyzer import analyze_blue_loop_and_instability
from .output_manager import create_output_directories, get_analysis_file_paths
from .mesa_analyzer import perform_mesa_analysis
from .plot_handlers import handle_heatmap_generation, handle_blue_loop_bc_plotting, handle_hr_diagram_generation

# Import the GYRE input generator module
from .gyre_in_generator import find_nearest_profiles, generate_gyre_in_file, run_gyre_input_generation

def main():
    """ Main function: Orchestrates the analysis and plotting workflow. """

    args = parsing_options() # Parse all arguments from CLI and config file

    # Set the global logging level based on the --debug argument.
    # This must be done here after args are parsed and before any significant logging.
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
        logging.debug("Debug logging enabled.")
    else:
        # Default logging level is INFO, already set by basicConfig
        pass

    input_dir = args.input_dir
    output_dir = args.output_dir
    analyze_blue_loop = args.analyze_blue_loop
    force_reanalysis = args.force_reanalysis
    should_generate_plots = args.generate_plots
    
    logging.info(f"MESA Grid Analysis started with input_dir: {input_dir}, output_dir: {output_dir}")

    # Create necessary output directories
    analysis_results_sub_dir, plots_sub_dir, blue_loop_plots_bc_sub_dir, detail_files_output_dir = \
        create_output_directories(output_dir, analyze_blue_loop, should_generate_plots, args.generate_blue_loop_plots_with_bc)

    # Get paths for analysis summary files
    summary_csv_path, cross_csv_path = get_analysis_file_paths(analysis_results_sub_dir)

    logging.debug(f"--- REANALYSIS DEBUG ---")
    logging.debug(f"    force_reanalysis (from args): {force_reanalysis}")
    logging.debug(f"    summary_csv_path: {summary_csv_path}")
    logging.debug(f"    Does summary_csv_path exist? {os.path.exists(summary_csv_path)}")
    logging.debug(f"    cross_csv_path: {cross_csv_path}")
    logging.debug(f"    Does cross_csv_path exist? {os.path.exists(cross_csv_path)}")

    # Call the main analysis function which returns three DataFrames
    summary_df_for_plotting, combined_detail_data_for_plotting, full_history_data_for_plotting = \
        perform_mesa_analysis(args, analysis_results_sub_dir, detail_files_output_dir)

    # Determine if analysis was actually performed or data loaded.
    # If summary_df_for_plotting is empty, it means no runs were found or processed.
    analysis_performed_or_data_loaded = not summary_df_for_plotting.empty
    
    if not analysis_performed_or_data_loaded:
        logging.info("No analysis performed or data loaded. Exiting.")
        return # Exit if no data to work with

    # --- GYRE Input File Generation ---
    if args.generate_gyre_in:
        logging.info("Starting GYRE input file generation process.")
        
        # Create a new Namespace object to pass to run_gyre_input_generation
        # This allows us to pass specific arguments, including an in-memory DataFrame
        gyre_args = argparse.Namespace()
        
        # Populate GYRE-specific arguments from the main args object
        gyre_args.input_dir = args.gyre_mesa_base_dir # This will default to args.input_dir if not specified
        gyre_args.output_dir = args.gyre_output_dir   # This will default based on args.output_dir
        gyre_args.model_name = args.gyre_model_name   # This will have a default value
        gyre_args.blue_loop_csv = args.gyre_blue_loop_csv # This will be None if not provided by user

        # Conditional logic to pass in-memory DataFrame if analysis was performed
        if args.analyze_blue_loop and analysis_performed_or_data_loaded:
            # Check for required columns and rename them for consistency with gyre_in_generator
            # mesa_analyzer returns 'first_model_number' and 'last_model_number'
            # gyre_in_generator expects 'min_model_number' and 'max_model_number'
            if 'first_model_number' in summary_df_for_plotting.columns and \
               'last_model_number' in summary_df_for_plotting.columns:
                
                df_to_pass_to_gyre = summary_df_for_plotting[['initial_mass', 'initial_Z', 'first_model_number', 'last_model_number']].copy()
                df_to_pass_to_gyre.rename(columns={
                    'initial_mass': 'mass', # Rename for consistency with gyre_in_generator's 'mass' column
                    'initial_Z': 'Z',       # Rename for consistency with gyre_in_generator's 'Z' column
                    'first_model_number': 'min_model_number',
                    'last_model_number': 'max_model_number'
                }, inplace=True)
                
                gyre_args.df_model_ranges = df_to_pass_to_gy # Pass the in-memory DataFrame
                logging.info("Passing in-memory analysis results to GYRE input generator.")
            else:
                logging.warning("In-memory summary DataFrame does not contain 'first_model_number' or 'last_model_number'. GYRE generator will attempt to load from CSV.")
                gyre_args.df_model_ranges = None # Ensure it's None to trigger CSV loading in gyre_in_generator
        else:
            logging.info("Analysis not performed or no data available. GYRE input generator will attempt to load from CSV if --gyre-blue-loop-csv was provided.")
            gyre_args.df_model_ranges = None # Ensure it's None to trigger CSV loading in gyre_in_generator

        try:
            run_gyre_input_generation(gyre_args)
        except Exception as e:
            logging.error(f"Error during GYRE input file generation: {e}")
            logging.exception("Exception details for GYRE input generation:") # Log full traceback
            
    # --- Heatmap generation ---
    handle_heatmap_generation(
        args,
        summary_df_for_plotting,
        plots_sub_dir,
        analysis_results_sub_dir,
        input_dir
    )

    # --- Plotting section for HR diagrams (now uses full_history_data_for_plotting) ---
    if args.generate_hr_diagrams != 'none': # Check if HR diagrams should be generated at all
        logging.info("Attempting to generate HR diagrams.")
        
        # Determine the boolean value for dropping ZAMS based on the argument
        should_drop_zams_for_hrd = (args.generate_hr_diagrams == 'drop_zams')
        logging.debug(f"    --> HR diagram generation: should_drop_zams_for_hrd set to: {should_drop_zams_for_hrd}")

        handle_hr_diagram_generation(
            args,
            plots_sub_dir, # This is the base 'plots' directory
            full_history_data_for_plotting, # Pass the full history data
            drop_zams=should_drop_zams_for_hrd # Pass the boolean flag
        )
    else:
        logging.info("Skipping HR diagram generation as per --generate-hr-diagrams setting.")

    # --- Plotting section for blue loop specific plots with BCs ---
    handle_blue_loop_bc_plotting(
        args,
        combined_detail_data_for_plotting,
        blue_loop_plots_bc_sub_dir,
        detail_files_output_dir
    )

    logging.info("MESA Grid Analysis and workflow completed.")

if __name__ == "__main__":
    main()
