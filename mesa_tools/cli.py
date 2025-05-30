import os
import argparse
import pandas as pd
import numpy as np
import re
from tqdm import tqdm

# Import your analyzer functions
# Use relative imports for modules within the same package.
# The '.' signifies "from the current package".
from .blue_loop_analyzer import analyze_blue_loop_and_instability
from .heatmap_generator import generate_heatmaps_and_time_diff_csv


def extract_params_from_inlist(inlist_path):
    """
    Extract initial_mass and initial_Z from a MESA inlist file.
    """
    mass = None
    z = None

    try:
        with open(inlist_path, 'r') as f:
            content = f.read()
            mass_match = re.search(r'initial_mass\s*=\s*(\d+\.?\d*)', content)
            if mass_match:
                mass = float(mass_match.group(1))
            z_match = re.search(r'initial_Z\s*=\s*(\d+\.?\d*(?:e[+\-]?\d+)?)', content)
            if z_match:
                z = float(z_match.group(1))
    except FileNotFoundError:
        print(f"ERROR: Inlist file not found: {inlist_path}")
    except Exception as e:
        print(f"ERROR: Error reading inlist file {inlist_path}: {e}")

    return mass, z


def scan_mesa_runs(input_dir, inlist_name):
    """
    Scan input directory for MESA run subdirectories ('run_*') containing inlist and history.data files.
    """
    mesa_run_infos = []
    potential_run_dirs = [d for d in os.listdir(input_dir)
                          if os.path.isdir(os.path.join(input_dir, d)) and d.startswith('run_')]

    if not potential_run_dirs:
        print(f"Warning: No 'run_*' subdirectories found directly in {input_dir}. "
              "Ensure MESA runs are organized as 'run_*' folders.")

    for run_dir_name in potential_run_dirs:
        run_dir_path = os.path.join(input_dir, run_dir_name)
        inlist_path = os.path.join(run_dir_path, inlist_name)
        history_file_path = os.path.join(run_dir_path, 'LOGS', 'history.data')

        if os.path.exists(inlist_path) and os.path.exists(history_file_path):
            mass, z = extract_params_from_inlist(inlist_path)
            if mass is not None and z is not None:
                mesa_run_infos.append({
                    'history_file_path': history_file_path,
                    'run_dir_path': run_dir_path,
                    'mass': mass,
                    'z': z
                })
            else:
                print(f"Warning: Could not extract mass/Z from inlist '{inlist_path}'. Skipping this run.")
        else:
            if not os.path.exists(inlist_path):
                print(f"Warning: Inlist file '{inlist_name}' not found in '{run_dir_path}'. Skipping this run.")
            if not os.path.exists(history_file_path):
                print(f"Warning: history.data not found at '{history_file_path}'. Skipping this run.")

    return mesa_run_infos


def get_data_from_history_file(history_file_path):
    """
    Reads MESA history.data file into a pandas DataFrame using np.genfromtxt.
    """
    try:
        # np.genfromtxt is good for fixed-width/delimiter-separated files.
        # skip_header=5 for MESA history.data format usually skips comments and column description.
        # names=True auto-detects column names from the first meaningful row.
        data = np.genfromtxt(history_file_path, names=True, comments="#", skip_header=5,
                             dtype=None, encoding='utf-8')

        # Handle cases where genfromtxt reads a single row as a 0-dimensional array
        if data.ndim == 0:
            # Convert single row to a list of tuples, then to DataFrame
            df = pd.DataFrame([data.tolist()], columns=data.dtype.names)
        else:
            # For multiple rows, directly convert to DataFrame
            df = pd.DataFrame(data)

        # Convert all columns to numeric, coercing errors to NaN
        for col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')

        # Drop rows where 'model_number' is NaN and convert to int
        if 'model_number' in df.columns:
            df.dropna(subset=['model_number'], inplace=True)
            if not df['model_number'].isnull().any(): # Check again after dropping NaNs
                df['model_number'] = df['model_number'].astype(int)

        return df

    except Exception as e:
        # Re-raise with more context to help debugging
        raise type(e)(f"Error loading or processing {history_file_path} using np.genfromtxt: {e}") from e


def main():
    parser = argparse.ArgumentParser(description="Analyze MESA stellar evolution grid runs.")
    parser.add_argument("-i", "--input-dir", required=True,
                        help="Directory containing MESA run subdirectories (e.g., 'run_M2.0_Z0.01').")
    parser.add_argument("-o", "--output-dir", required=True,
                        help="Output directory for results.")
    parser.add_argument("--analyze-blue-loop", action="store_true",
                        help="Perform blue loop analysis.")
    parser.add_argument("--inlist-name", default="inlist_project",
                        help="Inlist filename to identify runs (default: inlist_project).")
    parser.add_argument("--generate-heatmaps", action="store_true",
                        help="Generate heatmaps from cross-grid data.")
    parser.add_argument("--force-reanalysis", action="store_true",
                        help="Force reanalysis even if summary files exist.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'], default='all',
                        help="Blue loop output type: 'summary' or 'all'. 'summary' provides basic counts, 'all' includes detailed Blue Loop DataFrame.")
    parser.add_argument("--generate-plots", action="store_true",
                        help="Generate plots for analysis (beyond heatmaps).")

    args = parser.parse_args()

    input_dir = args.input_dir
    output_dir = args.output_dir
    analyze_blue_loop = args.analyze_blue_loop
    inlist_name = args.inlist_name
    generate_heatmaps = args.generate_heatmaps
    force_reanalysis = args.force_reanalysis
    blue_loop_output_type = args.blue_loop_output_type
    generate_plots = args.generate_plots

    # Define output subdirectories
    analysis_results_sub_dir = os.path.join(output_dir, "analysis_results")
    plots_sub_dir = os.path.join(output_dir, "plots")
    detail_files_output_dir = os.path.join(output_dir, "detail_files")

    # Create output directories
    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    if analyze_blue_loop and blue_loop_output_type == 'all':
        os.makedirs(detail_files_output_dir, exist_ok=True)
    if generate_plots or generate_heatmaps:
        os.makedirs(plots_sub_dir, exist_ok=True)

    summary_csv_path = os.path.join(analysis_results_sub_dir, "mesa_grid_analysis_summary.csv")
    cross_csv_path = os.path.join(analysis_results_sub_dir, "mesa_grid_cross.csv") # Renamed for consistency

    # Determine if reanalysis is needed
    reanalysis_needed = force_reanalysis or not os.path.exists(summary_csv_path) or not os.path.exists(cross_csv_path)

    if not reanalysis_needed:
        print("Summary and cross-grid CSV files already exist. Use --force-reanalysis to re-run analysis.")
        if generate_heatmaps:
            try:
                # Attempt to load existing summary data for heatmap generation
                summary_df_loaded = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])
                # Ensure the column used for heatmaps exists
                if 'blue_loop_crossing_count' in summary_df_loaded.columns:
                    # Unstack to get the matrix format for heatmap (Z rows, Mass columns)
                    cross_data_matrix_loaded = summary_df_loaded['blue_loop_crossing_count'].unstack(level='initial_mass')
                    print("Loaded existing summary data for heatmap generation.")

                    # Call the heatmap generator with loaded data
                    generate_heatmaps_and_time_diff_csv(
                        cross_data_df=cross_data_matrix_loaded,
                        summary_csv_path=summary_csv_path, # This is the full summary CSV
                        unique_zs=sorted(list(set(summary_df_loaded.index.get_level_values('initial_Z')))),
                        unique_masses=sorted(list(set(summary_df_loaded.index.get_level_values('initial_mass')))),
                        plots_output_dir=plots_sub_dir,
                        analysis_results_output_dir=analysis_results_sub_dir,
                        model_name=os.path.basename(input_dir), # Keep model_name for other heatmap titles, but time_diff file name is fixed
                        blue_loop_output_type=blue_loop_output_type,
                        analyze_blue_loop=analyze_blue_loop
                    )
                    print("Heatmaps generated from existing data.")
                else:
                    print("Summary CSV lacks 'blue_loop_crossing_count'; skipping heatmap generation from existing data.")
            except Exception as e:
                print(f"Error loading data for heatmaps: {e}. Use --force-reanalysis to retry analysis and generation.")
        return # Exit if reanalysis is not needed and heatmaps are generated/attempted

    print("Starting analysis of MESA runs...")
    mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)

    if not mesa_run_infos:
        print("No MESA runs found. Exiting.")
        return

    # Extract unique masses and metallicities to define the grid
    unique_masses = sorted(set(run['mass'] for run in mesa_run_infos))
    unique_zs = sorted(set(run['z'] for run in mesa_run_infos))

    if not unique_masses or not unique_zs:
        print("Error: Could not determine unique masses or metallicities from runs. Exiting.")
        return

    # Initialize DataFrame for cross-grid summary (e.g., crossing counts)
    cross_data_matrix = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses)
    cross_data_matrix.index.name = "Z"
    cross_data_matrix.columns.name = "Mass"

    # Lists to collect data for final summary and detailed DataFrames
    summary_data = []
    # Dictionary to group detailed DataFrames by Z for later concatenation
    grouped_detailed_dfs = {z_val: [] for z_val in unique_zs}

    total_runs = len(mesa_run_infos)
    print(f"Found {total_runs} runs for Z={unique_zs} and Mass={unique_masses}")

    # Prepare log file for skipped runs
    skipped_runs_log_path = os.path.join(analysis_results_sub_dir, "skipped_runs_log.txt")
    if os.path.exists(skipped_runs_log_path):
        os.remove(skipped_runs_log_path) # Clear previous log

    # Iterate through MESA runs with a progress bar
    with tqdm(total=total_runs, desc="Analyzing MESA runs") as pbar:
        for run_info in mesa_run_infos:
            history_file_path = run_info['history_file_path']
            current_mass = run_info['mass']
            current_z = run_info['z']

            # Initialize a dictionary for the current run's summary results
            # Default to NaN or appropriate empty values.
            # IMPORTANT: Added columns for time differences and instability
            analysis_result_summary = {
                'initial_mass': current_mass,
                'initial_Z': current_z,
                'blue_loop_crossing_count': np.nan,
                'blue_loop_duration_yr': np.nan,
                'max_log_L': np.nan,
                'max_log_Teff': np.nan,
                'max_log_R': np.nan,
                'first_model_number': np.nan,
                'last_model_number': np.nan,
                'first_age_yr': np.nan,
                'last_age_yr': np.nan,
                'blue_loop_start_age': np.nan, # New column
                'blue_loop_end_age': np.nan,   # New column
                'instability_start_age': np.nan, # New column
                'instability_end_age': np.nan,   # New column
                'calculated_blue_loop_duration': np.nan, # New column, will be derived
                'calculated_instability_duration': np.nan # New column, will be derived
            }
            current_detail_df = pd.DataFrame() # Initialize empty DataFrame for detailed data

            try:
                # Get history data for the current run
                df = get_data_from_history_file(history_file_path)

                if analyze_blue_loop:
                    # Capture the output of the analyzer!
                    analyzer_output = analyze_blue_loop_and_instability(df, current_mass, current_z)

                    # Check if analysis was successful (e.g., crossing_count is not NaN)
                    if not pd.isna(analyzer_output['crossing_count']):
                        # Populate the summary dictionary directly from the analyzer's output
                        analysis_result_summary['blue_loop_crossing_count'] = int(analyzer_output['crossing_count'])

                        state_times = analyzer_output['state_times']

                        # Populate blue loop start/end ages
                        analysis_result_summary['blue_loop_start_age'] = state_times.get('first_is_entry_age', np.nan)
                        analysis_result_summary['blue_loop_end_age'] = state_times.get('last_is_exit_age', np.nan)
                        
                        # Calculate blue loop duration if both start and end ages are available
                        if pd.notna(analysis_result_summary['blue_loop_start_age']) and pd.notna(analysis_result_summary['blue_loop_end_age']):
                            analysis_result_summary['calculated_blue_loop_duration'] = analysis_result_summary['blue_loop_end_age'] - analysis_result_summary['blue_loop_start_age']
                            # Also update the original 'blue_loop_duration_yr' for consistency with older code logic
                            analysis_result_summary['blue_loop_duration_yr'] = analysis_result_summary['calculated_blue_loop_duration']


                        # Populate instability start/end ages (assuming blue_loop_analyzer provides them)
                        analysis_result_summary['instability_start_age'] = state_times.get('instability_start_age', np.nan)
                        analysis_result_summary['instability_end_age'] = state_times.get('instability_end_age', np.nan)
                        
                        # Calculate instability duration if both start and end ages are available
                        if pd.notna(analysis_result_summary['instability_start_age']) and pd.notna(analysis_result_summary['instability_end_age']):
                            analysis_result_summary['calculated_instability_duration'] = analysis_result_summary['instability_end_age'] - analysis_result_summary['instability_start_age']

                        # If detailed output ('all') is requested, extract more metrics and the detailed DataFrame
                        if blue_loop_output_type == 'all' and not analyzer_output['blue_loop_detail_df'].empty:
                            bl_df = analyzer_output['blue_loop_detail_df']
                            analysis_result_summary['max_log_L'] = bl_df['log_L'].max()
                            analysis_result_summary['max_log_Teff'] = bl_df['log_Teff'].max()

                            # Max log_R needs to be handled.
                            # If 'log_R' is available in the detailed blue loop DF:
                            if 'log_R' in bl_df.columns:
                                analysis_result_summary['max_log_R'] = bl_df['log_R'].max()
                            # Otherwise, if it's not part of the detailed bl_df but exists in the full history_df:
                            elif 'log_R' in df.columns:
                                # This takes the max log_R from the *entire* history.
                                # If you need it *only* for the blue loop, ensure it's in bl_df.
                                analysis_result_summary['max_log_R'] = df['log_R'].max()

                            analysis_result_summary['first_model_number'] = bl_df['model_number'].min()
                            analysis_result_summary['last_model_number'] = bl_df['model_number'].max()
                            analysis_result_summary['first_age_yr'] = bl_df['star_age'].min()
                            analysis_result_summary['last_age_yr'] = bl_df['star_age'].max()

                            current_detail_df = bl_df # Store the detailed DF returned by the analyzer

                        # Update the cross-grid matrix with the crossing count for this run
                        cross_data_matrix.at[current_z, current_mass] = analysis_result_summary['blue_loop_crossing_count']
                    else:
                        print(f"Warning: Blue loop analysis failed or returned no valid crossings for M={current_mass}, Z={current_z}. Results for this run will be NaN in summary.")

                # Add the summary dictionary for the current run to the list
                summary_data.append(analysis_result_summary)

                # Append the detailed DataFrame (if it's not empty and 'all' output type is chosen)
                if blue_loop_output_type == 'all' and not current_detail_df.empty:
                    grouped_detailed_dfs[current_z].append(current_detail_df)

            except Exception as err:
                # Log errors for skipped runs
                with open(skipped_runs_log_path, 'a') as log_file:
                    log_file.write(f"Skipped run {run_info['run_dir_path']} due to error: {err}\n")
                print(f"Skipped run {run_info['run_dir_path']} due to error: {err}")

            pbar.update(1) # Update progress bar

    # Post-processing: Writing aggregated results

    # Convert collected summary data to DataFrame and save to CSV
    summary_df = pd.DataFrame(summary_data)
    summary_df.sort_values(['initial_Z', 'initial_mass'], inplace=True)
    summary_df.set_index(['initial_Z', 'initial_mass'], inplace=True) # Set a multi-index for easy unstacking later
    summary_df.to_csv(summary_csv_path)
    print(f"Summary CSV written to {summary_csv_path}")

    # Save the cross-grid matrix to CSV
    cross_data_matrix.to_csv(cross_csv_path)
    print(f"Cross-grid CSV written to {cross_csv_path}")

    # Write concatenated detail files per Z, if requested
    if blue_loop_output_type == 'all': # Only write detail files if 'all' output type is chosen
        for z_val, dfs_list in grouped_detailed_dfs.items():
            if dfs_list:
                try:
                    # Concatenate all detailed DataFrames for the current Z value
                    # The analyzer already adds 'initial_mass' and 'initial_Z' to each detail_df
                    combined_df = pd.concat(dfs_list, ignore_index=True)
                    # The detail filename should reflect the Z value.
                    detail_filename = os.path.join(detail_files_output_dir, f"detail_z{z_val:.4f}.csv")
                    combined_df.to_csv(detail_filename, index=False)
                    print(f"Written concatenated detail CSV for Z={z_val} to {detail_filename}")
                except Exception as e:
                    print(f"Error writing detail CSV for Z={z_val}: {e}")

    # Generate heatmaps if requested (using the newly created cross_data_matrix or the saved summary_csv)
    if generate_heatmaps:
        try:
            # Use the computed `cross_data_matrix` directly for heatmap generation as it's already in the correct format.
            generate_heatmaps_and_time_diff_csv(
                cross_data_df=cross_data_matrix,
                summary_csv_path=summary_csv_path, # Path to the generated summary (might be used for other heatmap functions)
                unique_zs=unique_zs,
                unique_masses=unique_masses,
                plots_output_dir=plots_sub_dir,
                analysis_results_output_dir=analysis_results_sub_dir,
                model_name=os.path.basename(input_dir), # Pass input_dir base name for potential other heatmap titles
                blue_loop_output_type=blue_loop_output_type,
                analyze_blue_loop=analyze_blue_loop
            )
            print("Heatmaps generated successfully.")
        except Exception as e:
            print(f"Error generating heatmaps: {e}")

if __name__ == "__main__":
    main()