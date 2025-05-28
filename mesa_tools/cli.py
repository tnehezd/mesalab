import argparse
import os
import pandas as pd
import numpy as np
import re
import json
from tqdm import tqdm
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, BoundaryNorm
import zipfile

# --- Import your analyzer and config functions ---
from mesa_tools.blue_loop_analyzer import analyze_blue_loop_and_instability, INSTABILITY_STRIP_VERTICES
from mesa_tools.config_manager import load_config
from mesa_tools.mesa_reader import read_history_data
from mesa_tools.grid_analyzer import analyze_mesa_grid_directory

def main():
    """
    Main function to parse arguments and orchestrate the grid analysis.
    """
    parser = argparse.ArgumentParser(description="Analyze MESA stellar evolution grids.")
    parser.add_argument("-i", "--input-dir", required=True,
                        help="Path to the root directory containing MESA run subdirectories.")
    parser.add_argument("-o", "--output-dir", required=True,
                        help="Path to the directory where analysis results will be saved.")
    parser.add_argument("--model-name", default="mesa_grid",
                        help="A name for the current model grid, used in output filenames (default: mesa_grid).")
    parser.add_argument("--analyze-blue-loop", action="store_true",
                        help="Enable blue loop analysis.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'], default='all',
                        help="Type of blue loop output: 'summary' (count only) or 'all' (detailed ages).")
    parser.add_argument("--generate-plots", action="store_true",
                        help="Generate heatmap plots from the analysis results.")
    parser.add_argument("--zip-details", action="store_true",
                        help="Zip detail files after generation.")


    # Command line arguments for config overrides
    parser.add_argument("--mesa-output-subdir", default=None,
                        help="Name of the MESA output subdirectory (e.g., LOGS) (default: from config.toml or LOGS).")
    parser.add_argument("--inlist-name", default=None,
                        help="Primary inlist filename (default: from config.toml or inlist).")
    parser.add_argument("--inlist-alternatives", nargs='*', default=None,
                        help="List of alternative inlist filenames (default: from config.toml or pre-defined).")

    args = parser.parse_args()

    # Load configuration from config.toml first
    config = load_config()

    # Override config values with command-line arguments if provided
    mesa_output_subdir = args.mesa_output_subdir if args.mesa_output_subdir is not None \
                         else config.get('paths', {}).get('mesa_output_subdir', 'LOGS')
    
    inlist_filename = args.inlist_name if args.inlist_name is not None \
                      else config.get('filenames', {}).get('inlist_filename', 'inlist')
    
    inlist_alternatives = args.inlist_alternatives if args.inlist_alternatives is not None \
                          else config.get('filenames', {}).get('inlist_alternatives', ["inlist_project", "inlist_1.0"])


    input_dir = args.input_dir
    output_dir = args.output_dir
    model_name = args.model_name
    analyze_blue_loop = args.analyze_blue_loop
    blue_loop_output_type = args.blue_loop_output_type
    generate_plots = args.generate_plots
    zip_details = args.zip_details

    # Create the top-level output directory
    os.makedirs(output_dir, exist_ok=True)

    # Define paths for subdirectories for organized output
    analysis_results_output_dir = os.path.join(output_dir, "analysis_results")
    detail_files_output_dir = os.path.join(output_dir, "detail_files")
    plots_output_dir = os.path.join(output_dir, "plots")

    # Create subdirectories
    os.makedirs(analysis_results_output_dir, exist_ok=True)
    os.makedirs(detail_files_output_dir, exist_ok=True)
    if generate_plots:
        os.makedirs(plots_output_dir, exist_ok=True)

    # Define output CSV and JSON paths
    summary_csv_path = os.path.join(analysis_results_output_dir, f"mesa_grid_analysis_summary_{model_name}.csv")
    cross_csv_path = os.path.join(analysis_results_output_dir, f"cross_{model_name}.csv")
    run_paths_json_path = os.path.join(analysis_results_output_dir, f"run_paths_{model_name}.json")

    print(f"Scanning input directory: {input_dir} for MESA runs using inlists: '{inlist_filename}', {inlist_alternatives} and output sub-directory: '{mesa_output_subdir}'...")
    
    # Use the analyze_mesa_grid_directory from grid_analyzer.py
    mesa_run_dirs_info = analyze_mesa_grid_directory(
        input_dir,
        mesa_output_subdir=mesa_output_subdir,
        inlist_filename=inlist_filename,
        inlist_alternatives=inlist_alternatives
    )

    if not mesa_run_dirs_info:
        print("No MESA run directories found based on the provided criteria. Exiting.")
        return

    # Store run paths for JSON export in a structured, sorted way
    # Create a dictionary of dictionaries: {Z_val: {Mass_val: path}}
    run_paths_structured = {}
    for run_info in mesa_run_dirs_info:
        z_val = run_info['z']
        mass_val = run_info['mass']
        path_val = run_info['path']
        
        if z_val not in run_paths_structured:
            run_paths_structured[z_val] = {}
        run_paths_structured[z_val][mass_val] = path_val
    
    # Sort the outer dictionary by Z keys
    sorted_zs_keys = sorted(run_paths_structured.keys())
    # Prepare the final JSON output with formatted keys
    json_output_dict = {}
    for z_key in sorted_zs_keys:
        # Sort the inner dictionary by Mass keys
        sorted_masses_for_z = sorted(run_paths_structured[z_key].keys())
        
        # Format the Z key
        formatted_z_key = f"Z{z_key:.4f}" # e.g., Z0.0015
        json_output_dict[formatted_z_key] = {}
        
        for mass_key in sorted_masses_for_z:
            # Format the Mass key
            formatted_mass_key = f"{mass_key:.1f}MSUN" # e.g., 15.0MSUN
            json_output_dict[formatted_z_key][formatted_mass_key] = run_paths_structured[z_key][mass_key]

    # Save the run paths to a JSON file
    with open(run_paths_json_path, 'w') as f:
        json.dump(json_output_dict, f, indent=4)
    print(f"Run paths saved to {run_paths_json_path}")


    # Extract unique Z values and Masses for structured output
    unique_zs = sorted(list(set(run['z'] for run in mesa_run_dirs_info)))
    unique_masses = sorted(list(set(run['mass'] for run in mesa_run_dirs_info)))
    
    print(f"Found {len(mesa_run_dirs_info)} MESA runs covering Z={unique_zs} and Mass={unique_masses}")

    all_summary_results_data = [] # For the main summary CSV
    
    # List to hold paths of detail files for zipping later
    detail_file_paths_for_zipping = []

    total_runs = len(mesa_run_dirs_info)
    print(f"Starting analysis of {total_runs} MESA runs...")

    # Initialize a DataFrame for the cross-section data for the heatmap
    # Use unique_zs as index and unique_masses as columns
    cross_data_df = pd.DataFrame(index=unique_zs, columns=unique_masses, dtype=float)


    with tqdm(total=total_runs, desc="Analyzing MESA runs") as pbar:
        for i, run_info in enumerate(mesa_run_dirs_info):
            run_path = run_info['path']
            current_mass = run_info['mass']
            current_z = run_info['z']
            
            run_result = {
                'initial_mass': current_mass,
                'initial_Z': current_z
            }

            analysis_result = None
            history_data = read_history_data(run_path, mesa_output_subdir) 

            if history_data is not None:
                if analyze_blue_loop:
                    analysis_result = analyze_blue_loop_and_instability(history_data) 

                if analysis_result:
                    # Update summary results
                    if blue_loop_output_type == 'summary':
                        run_result['blue_loop_crossing_count'] = analysis_result['crossing_count']
                    elif blue_loop_output_type == 'all':
                        run_result['blue_loop_crossing_count'] = analysis_result['crossing_count']
                        # Use .get() with a default of np.nan for state_times
                        run_result['ms_end_age'] = analysis_result['state_times'].get('ms_end_age', np.nan)
                        run_result['min_teff_post_ms_age'] = analysis_result['state_times'].get('min_teff_post_ms_age', np.nan)
                        run_result['first_is_entry_age'] = analysis_result['state_times'].get('first_is_entry_age', np.nan)
                        run_result['first_is_exit_age'] = analysis_result['state_times'].get('first_is_exit_age', np.nan)
                        run_result['last_is_entry_age'] = analysis_result['state_times'].get('last_is_entry_age', np.nan)
                        run_result['last_is_exit_age'] = analysis_result['state_times'].get('last_is_exit_age', np.nan)
                    
                    # Store crossing count for cross_data_df (heatmap source)
                    # Use .loc with specific Z and Mass values to ensure correct placement
                    cross_data_df.loc[current_z, current_mass] = analysis_result['crossing_count']

                    # Prepare detailed DataFrame for saving - SAVE INDIVIDUALLY HERE
                    if 'blue_loop_detail_df' in analysis_result and analysis_result['blue_loop_detail_df'] is not None:
                        detailed_df_for_saving = analysis_result['blue_loop_detail_df'].copy()
                        detailed_df_for_saving.insert(0, 'initial_mass', current_mass)
                        detailed_df_for_saving.insert(1, 'initial_Z', current_z)
                        
                        # --- Apply the specific filter for detail files: log_Teff > blue_edge_teff + 0.01 ---
                        blue_edge_teff_at_L = lambda log_L: np.interp(log_L, 
                                                                    [INSTABILITY_STRIP_VERTICES[0][1], INSTABILITY_STRIP_VERTICES[1][1]], 
                                                                    [INSTABILITY_STRIP_VERTICES[0][0], INSTABILITY_STRIP_VERTICES[1][0]])
                        
                        is_blue_edge_relevant = (detailed_df_for_saving['log_L'] >= min(INSTABILITY_STRIP_VERTICES[:,1])) & \
                                                (detailed_df_for_saving['log_L'] <= max(INSTABILITY_STRIP_VERTICES[:,1]))
                        
                        temp_blue_edge_teff_series = pd.Series(np.nan, index=detailed_df_for_saving.index)
                        temp_blue_edge_teff_series.loc[is_blue_edge_relevant] = detailed_df_for_saving.loc[is_blue_edge_relevant, 'log_L'].apply(blue_edge_teff_at_L)
                        
                        blueward_of_blue_edge_filter = (detailed_df_for_saving['log_Teff'] > temp_blue_edge_teff_series + 0.01) & is_blue_edge_relevant
                        
                        filtered_detail_df = detailed_df_for_saving[blueward_of_blue_edge_filter].copy()

                        if not filtered_detail_df.empty:
                            # Format Z and Mass for filename
                            formatted_z = f"{current_z:.4f}".replace('.', '') # e.g., 0.001 -> 0001
                            formatted_mass = f"{current_mass:.1f}".replace('.', '') # e.g., 15.0 -> 150

                            detail_filename = f"detail_Z{formatted_z}_M{formatted_mass}_{model_name}.csv"
                            detail_file_path = os.path.join(detail_files_output_dir, detail_filename)
                            
                            # Sort columns and save individually
                            desired_order = ['initial_mass', 'initial_Z', 'star_age', 'model_number', 'log_Teff', 'log_L', 'log_g']
                            # Add nu_radial_X and eta_radial_X columns if they exist in the filtered_detail_df
                            for col in filtered_detail_df.columns:
                                if re.match(r'nu_radial_\d+', col) or re.match(r'eta_radial_\d+', col):
                                    if col not in desired_order:
                                        desired_order.append(col)
                            
                            # CORRECTED LINE: Removed 'axis=1'
                            filtered_detail_df = filtered_detail_df.reindex(columns=desired_order) 
                            filtered_detail_df.to_csv(detail_file_path, index=False)
                            detail_file_paths_for_zipping.append(detail_file_path) # Add to list for zipping
                            # print(f"Saved individual detail file: {detail_file_path}") # Optional: enable for verbose output
                    
                else: # Analysis result is None (e.g., missing data from analyzer)
                    run_result['blue_loop_crossing_count'] = np.nan
                    if blue_loop_output_type == 'all':
                        run_result['ms_end_age'] = np.nan
                        run_result['min_teff_post_ms_age'] = np.nan
                        run_result['first_is_entry_age'] = np.nan
                        run_result['first_is_exit_age'] = np.nan
                        run_result['last_is_entry_age'] = np.nan
                        run_result['last_is_exit_age'] = np.nan
                    cross_data_df.loc[current_z, current_mass] = np.nan # Ensure NaN for heatmap source
            else: # History data could not be read
                print(f"Warning: Could not read history data for run at {run_path}. Skipping analysis for this run.")
                run_result['blue_loop_crossing_count'] = np.nan
                if blue_loop_output_type == 'all':
                    run_result['ms_end_age'] = np.nan
                    run_result['min_teff_post_ms_age'] = np.nan
                    run_result['first_is_entry_age'] = np.nan
                    run_result['first_is_exit_age'] = np.nan
                    run_result['last_is_entry_age'] = np.nan
                    run_result['last_is_exit_age'] = np.nan
                cross_data_df.loc[current_z, current_mass] = np.nan # Ensure NaN for heatmap source

            all_summary_results_data.append(run_result)
            pbar.update(1)

    print("\nAnalysis complete. Compiling summary results...")
    
    # Save the main summary CSV file
    final_results_df = pd.DataFrame(all_summary_results_data)
    final_results_df = final_results_df.set_index(['initial_Z', 'initial_mass']).sort_index()
    final_results_df.to_csv(summary_csv_path)
    print(f"Summary results saved to {summary_csv_path}")

    # Save the cross-section data for the heatmap
    cross_data_df = cross_data_df.reindex(index=unique_zs, columns=unique_masses)
    cross_data_df.index.name = "Z \ M" 
    cross_data_df.fillna('N/A', inplace=True) 

    cross_data_df.to_csv(cross_csv_path)
    print(f"Cross-section data for heatmap saved to {cross_csv_path}")

    # Zip the individual detailed dataframes if requested
    if analyze_blue_loop and detail_file_paths_for_zipping and zip_details:
        print(f"Zipping individual detail files into {detail_files_output_dir}/detail_files_{model_name}.zip ...")
        zip_filename = os.path.join(detail_files_output_dir, f"detail_files_{model_name}.zip")
        # Create a ZipFile object with write mode
        with zipfile.ZipFile(zip_filename, 'w', zipfile.ZIP_DEFLATED) as zipf:
            for file_path in detail_file_paths_for_zipping:
                # Calculate relative path inside the zip to avoid including full system paths
                arcname = os.path.basename(file_path)
                zipf.write(file_path, arcname) 
                os.remove(file_path) # Optionally remove individual files after zipping
        print(f"Individual detail files zipped to {zip_filename}. Original individual files removed.")
    elif analyze_blue_loop and not detail_file_paths_for_zipping:
        print("No individual detailed blue loop data files were generated to zip.")
    elif analyze_blue_loop and not zip_details:
        print(f"Individual detail files saved to '{detail_files_output_dir}'. Zipping was not requested.")


    # Generate heatmap if requested (logic directly in CLI for now)
    if generate_plots:
        print("\nGenerating heatmap...")
        try:
            # Reload cross_csv_path, ensuring N/A values are handled correctly if they are strings
            heatmap_df = pd.read_csv(cross_csv_path, index_col=0, na_values='N/A')
            
            # Convert columns and index to float for proper numeric operations
            heatmap_df.columns = heatmap_df.columns.astype(float)
            heatmap_df.index = heatmap_df.index.astype(float)

            # Reindex to ensure consistency, filling any missing Z/Mass combinations with NaN
            heatmap_data_matrix = heatmap_df.reindex(index=unique_zs, columns=unique_masses).values

            # Define colors and normalization for the heatmap
            colors = ["#440154", "#3b528b", "#21918c", "#5ec962", "#fde725", "#ffb14e"]
            cmap = ListedColormap(colors)
            cmap.set_bad(color="lightgray") # Color for NaN values

            bounds = [-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5] # Bounds for color bins
            norm = BoundaryNorm(bounds, cmap.N)

            plt.figure(figsize=(20, 12))
            plt.imshow(heatmap_data_matrix, aspect='equal', origin='lower', cmap=cmap, norm=norm)

            cbar = plt.colorbar(ticks=[0, 1, 2, 3, 4, 5])
            cbar.set_label("Number of IS Crossing Segments")
            cbar.ax.set_yticklabels(["0", "1", "2", "3", "4", "5+"])

            plt.xticks(np.arange(len(unique_masses)), [f'{m:.1f}' for m in unique_masses], rotation=90)
            # Adjust y-axis ticks for better readability if unique_zs is very large
            num_yticks = min(10, len(unique_zs)) # Max 10 ticks, or less if fewer Zs
            tick_positions = np.linspace(0, len(unique_zs) - 1, num_yticks, dtype=int)
            tick_labels = [f'{unique_zs[i]:.4f}' for i in tick_positions]
            plt.yticks(tick_positions, tick_labels)
            plt.xlabel("Mass [M$_\odot$]")
            plt.ylabel("Metallicity (Z)")
            plt.title(f"Heatmap: Number of Instability Strip Crossings ({model_name})")

            # Add text labels to heatmap cells
            for i in range(heatmap_data_matrix.shape[0]):
                for j in range(heatmap_data_matrix.shape[1]):
                    val = heatmap_data_matrix[i, j]
                    if not np.isnan(val):
                        # Ensure value is an integer for display
                        plt.text(j, i, int(val), ha='center', va='center', color='black', fontsize=8)
                    else:
                        plt.text(j, i, 'N/A', ha='center', va='center', color='gray', fontsize=8)


            image_file_name = f"heatmap_mass_metallicity_{model_name}.png"
            image_file_path = os.path.join(plots_output_dir, image_file_name)
            plt.savefig(image_file_path, bbox_inches='tight', dpi=300)
            print(f"Heatmap saved to {image_file_path}")
            plt.close()
        except Exception as e:
            print(f"Error generating heatmap: {e}")
            print("Please ensure the 'matplotlib' library is installed and verify data integrity.")


if __name__ == "__main__":
    main()