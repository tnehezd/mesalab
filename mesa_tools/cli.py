import argparse
import os
import pandas as pd
import numpy as np
import re
from tqdm import tqdm
import json

# --- Import your analyzer functions ---
from mesa_tools.blue_loop_analyzer import analyze_blue_loop_and_instability
from mesa_tools.profile_indexer import get_profile_numbers_for_models
from mesa_tools.plotting_routines import generate_blue_loop_heatmap_revised

def extract_params_from_path(path):
    """
    Extracts initial mass and metallicity from a MESA run path.
    Assumes a format like 'run_nad_convos_mid_15.0MSUN_z0.0015'
    """
    mass = None
    z = None
    
    # Try to find mass (e.g., 15.0MSUN)
    mass_match = re.search(r'(\d+\.?\d*)MSUN', path)
    if mass_match:
        mass = float(mass_match.group(1))

    # Try to find metallicity (e.g., z0.0015)
    z_match = re.search(r'z(\d+\.?\d*)', path)
    if z_match:
        z = float(z_match.group(1))
        
    return mass, z

def scan_mesa_runs(input_dir, inlist_name):
    """
    Scans the input directory for MESA run subdirectories based on the presence of an inlist file.
    """
    mesa_run_dirs = []
    for root, dirs, files in os.walk(input_dir):
        if inlist_name in files:
            mass, z = extract_params_from_path(root)
            if mass is not None and z is not None:
                mesa_run_dirs.append({
                    'path': root,
                    'mass': mass,
                    'z': z
                })
            else:
                print(f"Warning: Could not extract mass/Z from path: {root}. Skipping.")
    return mesa_run_dirs

def main():
    """
    Main function to parse arguments and orchestrate the grid analysis.
    """
    parser = argparse.ArgumentParser(description="Analyze MESA stellar evolution grids.")
    parser.add_argument("-i", "--input-dir", required=True,
                        help="Path to the root directory containing MESA run subdirectories.")
    parser.add_argument("-o", "--output-dir", required=True,
                        help="Path to the directory where analysis results will be saved.")
    parser.add_argument("--inlist-name", default="inlist_project",
                        help="Name of the inlist file to identify MESA run directories (default: inlist_project).")
    parser.add_argument("--analyze-blue-loop", action="store_true",
                        help="Enable blue loop analysis and generate summary/detail files.")
    parser.add_argument("--generate-plots", action="store_true",
                        help="Generate plots for each run's analysis (e.g., HR diagrams). Requires existing detail files or --analyze-blue-loop.")
    parser.add_argument("--get-profile-numbers", action="store_true",
                        help="Determine min/max profile numbers for the blue loop models. Requires existing summary or --analyze-blue-loop.")
    parser.add_argument("--regenerate-summary", action="store_true",
                        help="Force re-running the full blue loop analysis, even if a summary file already exists. Implies --analyze-blue-loop.")
    parser.add_argument("--generate-heatmap", action="store_true",
                        help="Generate heatmap of blue loop crossing counts.")
    # The --legacy-summary-format argument is removed as this behavior is now default.


    args = parser.parse_args()

    input_dir = args.input_dir
    output_dir = args.output_dir
    inlist_name = args.inlist_name
    analyze_blue_loop = args.analyze_blue_loop or args.regenerate_summary
    generate_plots = args.generate_plots
    get_profile_numbers = args.get_profile_numbers
    regenerate_summary = args.regenerate_summary

    # Create the top-level output directory
    os.makedirs(output_dir, exist_ok=True)

    # Define paths for subdirectories for organized output
    analysis_results_output_dir = os.path.join(output_dir, "analysis_results") 
    detail_files_output_dir = os.path.join(output_dir, "detail_files") 
    plots_output_dir = os.path.join(output_dir, "plots") 

    # Create subdirectories conditionally
    os.makedirs(analysis_results_output_dir, exist_ok=True)
    if analyze_blue_loop:
        os.makedirs(detail_files_output_dir, exist_ok=True) 
    if generate_plots or args.generate_heatmap:
        os.makedirs(plots_output_dir, exist_ok=True) 
    
    summary_csv_path = os.path.join(analysis_results_output_dir, "mesa_grid_analysis_summary.csv")
    run_paths_json_path = os.path.join(analysis_results_output_dir, "run_paths.json") 
    profile_numbers_csv_path = os.path.join(analysis_results_output_dir, "mesa_grid_profile_numbers.csv")
    combined_detail_output_path = os.path.join(analysis_results_output_dir, "mesa_grid_all_blue_loop_detail.csv")
    
    # final_results_df will now directly come from the loaded legacy CSV for internal use (e.g., heatmap)
    final_results_df = None
    run_paths_data = {}
    all_blue_loop_detail_dfs = [] 

    # --- Step 1: Perform or Load Blue Loop Analysis ---
    if regenerate_summary or not os.path.exists(summary_csv_path) or analyze_blue_loop:
        print("Performing full analysis from MESA run directories...")
        mesa_run_dirs = scan_mesa_runs(input_dir, inlist_name)

        if not mesa_run_dirs:
            print("No MESA run directories found to analyze.")
            return

        unique_zs = sorted(list(set(run['z'] for run in mesa_run_dirs)))
        unique_masses = sorted(list(set(run['mass'] for run in mesa_run_dirs)))

        # Initialize a NumPy array to store the stringified lists for the summary CSV
        result_table_for_summary = np.full((len(unique_zs), len(unique_masses)), 'N/A', dtype=object)
        z_to_idx = {z: i for i, z in enumerate(unique_zs)}
        mass_to_idx = {m: i for i, m in enumerate(unique_masses)}
        
        total_runs = len(mesa_run_dirs)
        print(f"Starting analysis of {total_runs} MESA runs...")

        with tqdm(total=total_runs, desc="Analyzing MESA runs") as pbar:
            for i, run_info in enumerate(mesa_run_dirs):
                run_path = run_info['path']
                current_mass = run_info['mass']
                current_z = run_info['z']
                
                # Store path mapping for JSON
                z_key = f"Z{current_z:.4f}"
                mass_key = f"M{current_mass:.1f}"
                if z_key not in run_paths_data:
                    run_paths_data[z_key] = {}
                run_paths_data[z_key][mass_key] = run_path

                analysis_result = None

                if analyze_blue_loop: 
                    analysis_result = analyze_blue_loop_and_instability(run_path)

                if analysis_result:
                    crossing_count = analysis_result.get('crossing_count', np.nan)
                    state_times_list = analysis_result.get('state_times', [np.nan]*6)
                    
                    # Ensure all 6 elements are present and are floats or NaN for consistency
                    state_times_list = [float(x) if x is not np.nan else np.nan for x in state_times_list]
                    
                    # Combine count and state_times into a single list
                    # Format as string for CSV cell, mimicking the original cross_model.csv
                    formatted_list_elements = [str(int(crossing_count)) if not np.isnan(crossing_count) else '0'] + \
                                              [f"{t:.6f}" if not np.isnan(t) else '0' for t in state_times_list]
                    result_table_for_summary[z_to_idx[current_z], mass_to_idx[current_mass]] = f"[{', '.join(formatted_list_elements)}]"

                    # If the detail DataFrame was created, add it to the list for combined saving
                    if analysis_result['blue_loop_detail_df'] is not None and not analysis_result['blue_loop_detail_df'].empty:
                        detailed_df_for_saving = analysis_result['blue_loop_detail_df'].copy()
                        detailed_df_for_saving.insert(0, 'initial_mass', current_mass)
                        detailed_df_for_saving.insert(0, 'initial_Z', current_z)
                        all_blue_loop_detail_dfs.append(detailed_df_for_saving)
                else: # analysis_result is None (e.g., file not found, incomplete data)
                    result_table_for_summary[z_to_idx[current_z], mass_to_idx[current_mass]] = 'N/A'

                pbar.update(1)

        print("\nFull analysis complete. Compiling summary results...")
        
        # Always save in the legacy format now
        with open(summary_csv_path, mode='w', newline='') as cross_file:
            df_to_save = pd.DataFrame(result_table_for_summary, index=unique_zs, columns=unique_masses)
            df_to_save.index.name = "Z \\ M"
            df_to_save.columns = [f'{m:.1f}' for m in unique_masses]
            df_to_save.to_csv(cross_file, na_rep='N/A')
        print(f"Main summary results (legacy format) saved to {summary_csv_path}")
        
        # Load the saved legacy CSV to create 'final_results_df' for internal use (e.g., heatmap)
        try:
            temp_df_for_heatmap_conv = pd.read_csv(summary_csv_path, index_col=0)
            temp_df_for_heatmap_conv.columns = temp_df_for_heatmap_conv.columns.astype(float)
            temp_df_for_heatmap_conv.index = temp_df_for_heatmap_conv.index.astype(float)
            
            data_for_heatmap_conversion = {}
            for z_val in temp_df_for_heatmap_conv.index:
                for m_val in temp_df_for_heatmap_conv.columns:
                    val_str = temp_df_for_heatmap_conv.loc[z_val, m_val]
                    if isinstance(val_str, str) and val_str.startswith('['):
                        try:
                            val_list = json.loads(val_str.replace("'", '"')) 
                            data_for_heatmap_conversion[(z_val, m_val)] = val_list[0] # First element is the count
                        except Exception as e:
                            data_for_heatmap_conversion[(z_val, m_val)] = np.nan
                    else:
                        data_for_heatmap_conversion[(z_val, m_val)] = np.nan

            processed_heatmap_data = pd.DataFrame.from_dict(data_for_heatmap_conversion, orient='index', columns=['blue_loop_crossing_count'])
            processed_heatmap_data.index.names = ['initial_Z', 'initial_mass']
            final_results_df = processed_heatmap_data.sort_index()

        except Exception as e:
            print(f"Error converting legacy summary for internal use: {e}. Some operations might be incomplete.")
            final_results_df = pd.DataFrame(columns=['initial_Z', 'initial_mass', 'blue_loop_crossing_count']) # Empty DataFrame


        # Save run_paths_data to JSON file
        try:
            sorted_run_paths_data = {}
            for z_key in sorted(run_paths_data.keys()):
                sorted_mass_data = {}
                mass_keys_sorted = sorted(run_paths_data[z_key].keys(), 
                                          key=lambda x: float(x.replace('M', '')))
                for mass_key in mass_keys_sorted:
                    sorted_mass_data[mass_key] = run_paths_data[z_key][mass_key]
                sorted_run_paths_data[z_key] = sorted_mass_data

            with open(run_paths_json_path, 'w') as f:
                json.dump(sorted_run_paths_data, f, indent=4)
            print(f"Run paths saved to {run_paths_json_path}")
        except Exception as e:
            print(f"Error saving run paths to JSON: {e}")

        # Save the aggregated detailed dataframes for each Z
        if all_blue_loop_detail_dfs:
            print(f"Saving aggregated detailed files to '{combined_detail_output_path}'...")
            combined_detail_df = pd.concat(all_blue_loop_detail_dfs, ignore_index=True)
            combined_detail_df = combined_detail_df.sort_values(by=['initial_Z', 'initial_mass', 'star_age'])
            
            combined_detail_df.to_csv(combined_detail_output_path, index=False)
            print(f"  Saved: {combined_detail_output_path}")

        else:
            print("No detailed blue loop data found to save.")

    else: # If not regenerating and summary exists, load it
        print(f"Loading existing summary data from {summary_csv_path} (assuming legacy format)...")
        try:
            # Always assume legacy format for loading now
            temp_df_for_heatmap_conv = pd.read_csv(summary_csv_path, index_col=0)
            temp_df_for_heatmap_conv.columns = temp_df_for_heatmap_conv.columns.astype(float)
            temp_df_for_heatmap_conv.index = temp_df_for_heatmap_conv.index.astype(float)
            
            data_for_heatmap_conversion = {}
            for z_val in temp_df_for_heatmap_conv.index:
                for m_val in temp_df_for_heatmap_conv.columns:
                    val_str = temp_df_for_heatmap_conv.loc[z_val, m_val]
                    if isinstance(val_str, str) and val_str.startswith('['):
                        try:
                            val_list = json.loads(val_str.replace("'", '"'))
                            data_for_heatmap_conversion[(z_val, m_val)] = val_list[0]
                        except Exception as e:
                            data_for_heatmap_conversion[(z_val, m_val)] = np.nan
                    else:
                        data_for_heatmap_conversion[(z_val, m_val)] = np.nan

            processed_heatmap_data = pd.DataFrame.from_dict(data_for_heatmap_conversion, orient='index', columns=['blue_loop_crossing_count'])
            processed_heatmap_data.index.names = ['initial_Z', 'initial_mass']
            final_results_df = processed_heatmap_data.sort_index()
            print("Successfully loaded existing summary.")

        except Exception as e:
            print(f"Error loading summary CSV: {e}. Please use --regenerate-summary or check file. Exiting.")
            return
        
        # Load run_paths_data from JSON file
        print(f"Loading run paths from {run_paths_json_path}...")
        try:
            with open(run_paths_json_path, 'r') as f:
                run_paths_data = json.load(f)
            print("Successfully loaded run paths.")
        except FileNotFoundError:
            print(f"Warning: {run_paths_json_path} not found. Some operations may fail. Please --regenerate-summary if you need full functionality.")
        except Exception as e:
            print(f"Error loading run paths JSON: {e}. Please use --regenerate-summary.")
            run_paths_data = {}

    # --- Step 2: Perform Profile Number Extraction (if requested and data available) ---
    # This section is commented out or produces a warning because the 'min_model_number_bl'
    # and 'max_model_number_bl' columns are not available in the legacy summary CSV.
    # If profile numbers are still needed, you would need to implement a separate
    # mechanism to store and retrieve these model numbers (e.g., a dedicated CSV file).
    if get_profile_numbers:
        print("Skipping profile number determination: Model number details are not available in the legacy summary format.")
        print("If you need profile numbers, consider adapting the analysis logic to store them separately.")

    # --- Step 3: Generate Plots (if requested and data available) ---
    if generate_plots:
        print("Generating plots...")
        if not run_paths_data:
            print("Skipping plot generation: Run path information is missing. Please --regenerate-summary if you need to generate detailed data.")
        else:
            if not os.path.exists(combined_detail_output_path):
                print(f"Warning: Combined detail file not found at {combined_detail_output_path}. Skipping plotting.")
                return 

            try:
                full_detail_df = pd.read_csv(combined_detail_output_path)
            except Exception as e:
                print(f"Error reading combined detail file {combined_detail_output_path}: {e}. Skipping plotting.")
                return 

            unique_zs_to_plot = sorted(list(full_detail_df['initial_Z'].unique()))

            for z in tqdm(unique_zs_to_plot, desc="Processing Z for plots"):
                masses_for_this_z = full_detail_df[full_detail_df['initial_Z'] == z]['initial_mass'].unique()

                for mass in masses_for_this_z:
                    run_detail_df = full_detail_df[(full_detail_df['initial_Z'] == z) & (full_detail_df['initial_mass'] == mass)]
                    
                    if not run_detail_df.empty:
                        # Placeholder for actual plotting code
                        pass 
                    else:
                        print(f"Warning: No detailed data for mass {mass} in Z={z}. Skipping plot.")
    elif generate_plots:
        print("Skipping plot generation: Detailed data files (e.g., mesa_grid_all_blue_loop_detail.csv) were not generated. Please run with --analyze-blue-loop first.")

    # --- Step 4: Generate Heatmap (if requested and data available) ---
    if args.generate_heatmap:
        print("\nGenerating heatmap...")
        # The heatmap function no longer needs the legacy flag, as it's now the default behavior
        generate_blue_loop_heatmap_revised(summary_csv_path, plots_output_dir)


if __name__ == "__main__":
    main()