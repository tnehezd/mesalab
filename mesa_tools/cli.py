import argparse
import os
import pandas as pd
import numpy as np
import re
from tqdm import tqdm
# import matplotlib.pyplot as plt # Uncomment if you plan to add plotting functions here

# --- Import your analyzer functions ---
from mesa_tools.blue_loop_analyzer import analyze_blue_loop_and_instability

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
                        help="Enable blue loop analysis.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'], default='all',
                        help="Type of blue loop output: 'summary' (count only) or 'all' (detailed ages).")
    parser.add_argument("--generate-plots", action="store_true",
                        help="Generate plots for each run's analysis (e.g., HR diagrams).")

    args = parser.parse_args()

    input_dir = args.input_dir
    output_dir = args.output_dir
    inlist_name = args.inlist_name
    analyze_blue_loop = args.analyze_blue_loop
    blue_loop_output_type = args.blue_loop_output_type
    generate_plots = args.generate_plots

    # Create the top-level output directory
    os.makedirs(output_dir, exist_ok=True)

    # Define paths for subdirectories for organized output
    # Fixed folder name: 'analysis_results' (typo corrected and consistent with general usage)
    analysis_results_output_dir = os.path.join(output_dir, "analysis_results") 
    # Changed folder name: 'detail_files'
    detail_files_output_dir = os.path.join(output_dir, "detail_files") 
    plots_output_dir = os.path.join(output_dir, "plots") 

    # Create subdirectories conditionally
    os.makedirs(analysis_results_output_dir, exist_ok=True) # Always create for the main summary CSV
    if analyze_blue_loop:
        os.makedirs(detail_files_output_dir, exist_ok=True) # Create detail_files directory if blue loop analysis is on
    if generate_plots:
        os.makedirs(plots_output_dir, exist_ok=True) # Create plots directory only if plotting is requested
    
    print(f"Scanning input directory: {input_dir} for MESA runs...")
    mesa_run_dirs = scan_mesa_runs(input_dir, inlist_name)

    if not mesa_run_dirs:
        print("No MESA run directories found.")
        return

    # Extract unique Z values to group detailed output by metallicity
    unique_zs = sorted(list(set(run['z'] for run in mesa_run_dirs)))
    
    # Dictionary to hold combined detailed dataframes for each Z
    # Key: Z value, Value: list of DataFrames (one for each mass at that Z)
    grouped_detailed_dfs = {z_val: [] for z_val in unique_zs}

    initial_masses = sorted(list(set(run['mass'] for run in mesa_run_dirs)))
    initial_zs = sorted(list(set(run['z'] for run in mesa_run_dirs)))

    print(f"Found {len(mesa_run_dirs)} MESA runs for Z={initial_zs} and Mass={initial_masses}")

    all_results_data = [] # For the main summary CSV

    total_runs = len(mesa_run_dirs)
    print(f"Starting analysis of {total_runs} MESA runs...")

    with tqdm(total=total_runs, desc="Analyzing MESA runs") as pbar:
        for i, run_info in enumerate(mesa_run_dirs):
            run_path = run_info['path']
            current_mass = run_info['mass']
            current_z = run_info['z']
            
            run_result = {
                'initial_mass': current_mass,
                'initial_Z': current_z
            }

            analysis_result = None
            if analyze_blue_loop:
                analysis_result = analyze_blue_loop_and_instability(run_path)

            if analysis_result:
                if blue_loop_output_type == 'summary':
                    run_result['blue_loop_crossing_count'] = analysis_result['crossing_count']
                elif blue_loop_output_type == 'all':
                    run_result['blue_loop_crossing_count'] = analysis_result['crossing_count']
                    run_result['ms_end_age'] = analysis_result['state_times'].get('ms_end_age', np.nan)
                    run_result['min_teff_post_ms_age'] = analysis_result['state_times'].get('min_teff_post_ms_age', np.nan)
                    run_result['first_is_entry_age'] = analysis_result['state_times'].get('first_is_entry_age', np.nan)
                    run_result['first_is_exit_age'] = analysis_result['state_times'].get('first_is_exit_age', np.nan)
                    run_result['last_is_entry_age'] = analysis_result['state_times'].get('last_is_entry_age', np.nan)
                    run_result['last_is_exit_age'] = analysis_result['state_times'].get('last_is_exit_age', np.nan)
                
                # Prepare detailed DataFrame for saving (add 'initial_mass' column)
                if 'blue_loop_detail_df' in analysis_result and analysis_result['blue_loop_detail_df'] is not None:
                    detailed_df_for_saving = analysis_result['blue_loop_detail_df'].copy()
                    detailed_df_for_saving.insert(0, 'initial_mass', current_mass) # Add mass column at the beginning
                    grouped_detailed_dfs[current_z].append(detailed_df_for_saving)
                    
                    # Note: 'detailed_data_file' in the summary will now point to the aggregated file
                    # This column will be populated after all runs are processed and aggregated files are saved.
                    # Temporarily, we can set it to a placeholder or determine it later.
                    # For now, we won't set this on a per-run basis, as the file will contain *all* models for a Z.
                else:
                    # If no detailed_df, still ensure detailed_data_file is NaN in the summary
                    pass # We will set 'detailed_data_file' after the loop

                # Conditional Plot Generation for the current run
                if generate_plots:
                    # Implement your plotting logic here for individual runs.
                    # Example:
                    # if analysis_result['blue_loop_detail_df'] is not None:
                    #     fig, ax = plt.subplots(figsize=(8, 6))
                    #     ax.plot(analysis_result['blue_loop_detail_df']['log_Teff'],
                    #             analysis_result['blue_loop_detail_df']['log_L'],
                    #             label=f'M={current_mass}')
                    #     ax.set_xlabel('log(Teff)')
                    #     ax.set_ylabel('log(L/Lsun)')
                    #     ax.set_title(f'HR Diagram for Z={current_z}, M={current_mass}')
                    #     ax.invert_xaxis()
                    #     ax.legend()
                    #     plot_filename = f"HR_Z{current_z:.4f}_M{current_mass:.1f}MSUN.png"
                    #     plot_path = os.path.join(plots_output_dir, plot_filename)
                    #     plt.savefig(plot_path)
                    #     plt.close(fig)
                    pass
            else:
                run_result['blue_loop_crossing_count'] = np.nan
                if blue_loop_output_type == 'all':
                    run_result['ms_end_age'] = np.nan
                    run_result['min_teff_post_ms_age'] = np.nan
                    run_result['first_is_entry_age'] = np.nan
                    run_result['first_is_exit_age'] = np.nan
                    run_result['last_is_entry_age'] = np.nan
                    run_result['last_is_exit_age'] = np.nan
                # No 'detailed_data_file' entry for summary, will be added if needed below
            
            all_results_data.append(run_result)
            pbar.update(1)

    print("\nAnalysis complete. Compiling summary results...")
    
    # Save the main summary CSV file
    final_results_df = pd.DataFrame(all_results_data)
    final_results_df = final_results_df.set_index(['initial_Z', 'initial_mass']).sort_index()
    
    # Now, save the aggregated detailed dataframes for each Z
    if analyze_blue_loop:
        print(f"Saving detailed per-metallicity files to '{detail_files_output_dir}'...")
        for z_val, list_of_dfs in grouped_detailed_dfs.items():
            if list_of_dfs: # Only save if there's data for this Z
                # Concatenate all DataFrames for this Z into one
                combined_df_for_z = pd.concat(list_of_dfs, ignore_index=True)
                
                # Define columns order explicitly as requested:
                # mass,star_age,model_number,log_Teff,log_L,log_g
                desired_order = ['initial_mass', 'star_age', 'model_number', 'log_Teff', 'log_L', 'log_g']
                
                # Reorder columns and ensure all desired are present (fill with NaN if not for some reason)
                final_combined_df = pd.DataFrame(columns=desired_order) # Create an empty DF with desired columns
                for col in desired_order:
                    if col in combined_df_for_z.columns:
                        final_combined_df[col] = combined_df_for_z[col]
                    else:
                        final_combined_df[col] = np.nan # Or handle missing columns as an error

                # Sort the combined DataFrame for consistency (optional, but good practice)
                final_combined_df = final_combined_df.sort_values(by=['initial_mass', 'star_age'])

                detail_filename_z = f"detail_Z{z_val:.4f}.csv" # Filename for each Z
                detail_file_path_z = os.path.join(detail_files_output_dir, detail_filename_z)
                final_combined_df.to_csv(detail_file_path_z, index=False)
                print(f"  Saved: {detail_file_path_z}")
                
                # Add the relative path to the summary DataFrame for this Z's rows
                # This needs careful handling as it's a per-Z file, not per-run
                # For simplicity, let's add a new column for this in the summary if we desire.
                # Since the summary is already indexed by Z and Mass, we'll mark all rows for this Z.
                # A more precise way would be to add a new 'detail_file_Z' column.
                
                # Let's add a new column 'aggregated_detail_file' to the summary
                # It will have the same path for all masses within a given Z.
                final_results_df.loc[final_results_df.index.get_level_values('initial_Z') == z_val, 
                                     'aggregated_detail_file'] = os.path.relpath(detail_file_path_z, output_dir)
            
    # Final save of the summary CSV (after potentially adding 'aggregated_detail_file')
    summary_csv_path = os.path.join(analysis_results_output_dir, "mesa_grid_analysis_summary.csv") # Renamed for clarity
    final_results_df.to_csv(summary_csv_path)
    print(f"Summary results saved to {summary_csv_path}")

if __name__ == "__main__":
    main()