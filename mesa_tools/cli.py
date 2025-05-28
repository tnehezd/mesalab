import os
import argparse
import pandas as pd
import numpy as np
import glob
import re
import csv
from tqdm import tqdm
import math

# --- Import your analyzer functions ---
# Make sure these modules are accessible in your Python environment
from mesa_tools.blue_loop_analyzer import analyze_blue_loop_and_instability
from mesa_tools.heatmap_generator import generate_heatmaps_and_time_diff_csv


def extract_params_from_inlist(inlist_path):
    """
    Extracts initial_mass and initial_Z from a MESA inlist file.
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
    Scans the input directory for MESA run subdirectories (named 'run_*')
    based on the presence of an inlist file within them.
    """
    mesa_run_infos = []
    potential_run_dirs = [d for d in os.listdir(input_dir) if os.path.isdir(os.path.join(input_dir, d)) and d.startswith('run_')]

    if not potential_run_dirs:
        print(f"Warning: No 'run_*' subdirectories found directly in {input_dir}. "
              "Please ensure your MESA runs are organized as subdirectories starting with 'run_'.")

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
                print(f"Warning: Could not extract mass/Z from inlist file '{inlist_path}'. Skipping this run from scan.")
        else:
            if not os.path.exists(inlist_path):
                print(f"Warning: Inlist file '{inlist_name}' not found in '{run_dir_path}'. Skipping this run from scan.")
            if not os.path.exists(history_file_path):
                print(f"Warning: history.data not found at '{history_file_path}'. Skipping this run from scan.")

    return mesa_run_infos


def get_data_from_history_file(history_file_path):
    """
    Reads data from a MESA history.data file using np.genfromtxt
    with known header properties, matching the successful debug_hr_plot.py method.
    """
    # Removed: print(f"DEBUG: Attempting to read history.data from: {history_file_path} using np.genfromtxt (skip_header=5).")

    try:
        data = np.genfromtxt(history_file_path, names=True, comments="#", skip_header=5, dtype=None, encoding='utf-8')

        if data.ndim == 0:
            df = pd.DataFrame([data.tolist()], columns=data.dtype.names)
        else:
            df = pd.DataFrame(data)

        for col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')

        if 'model_number' in df.columns:
            df.dropna(subset=['model_number'], inplace=True)
            if not df['model_number'].isnull().any():
                df['model_number'] = df['model_number'].astype(int)

        # Removed: print(f"DEBUG: Successfully read {len(df)} rows from {history_file_path} using np.genfromtxt.")
        return df

    except Exception as e:
        # Re-raise the exception with more context for better debugging
        raise type(e)(f"Error loading or processing data from {history_file_path} using np.genfromtxt: {e}") from e


def main():
    """
    Main function to parse arguments and orchestrate the grid analysis.
    """
    parser = argparse.ArgumentParser(description="Analyze MESA stellar evolution grid runs.")
    parser.add_argument("-i", "--input-dir", required=True,
                        help="Path to the directory containing MESA run subdirectories (e.g., 'run_M2.0_Z0.01').")
    parser.add_argument("-o", "--output-dir", required=True,
                        help="Path to the output directory where results will be saved.")
    parser.add_argument("--analyze-blue-loop", action="store_true",
                        help="Enable blue loop analysis for each run.")
    parser.add_argument("--inlist-name", default="inlist_project",
                        help="Name of the inlist file to identify MESA run directories (default: inlist_project).")
    parser.add_argument("--generate-heatmaps", action="store_true",
                        help="Generate heatmaps from the cross-grid data.")
    parser.add_argument("--force-reanalysis", action="store_true",
                        help="Force reanalysis of all runs, even if summary files exist.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'], default='all',
                        help="Specify blue loop output type: 'summary' (count only) or 'all' (detailed ages).")
    parser.add_argument("--generate-plots", action="store_true",
                        help="Generate plots for each run's analysis (e.g., HR diagrams).")


    args = parser.parse_args()

    input_dir = args.input_dir
    output_dir = args.output_dir
    analyze_blue_loop = args.analyze_blue_loop
    inlist_name = args.inlist_name
    generate_heatmaps = args.generate_heatmaps
    force_reanalysis = args.force_reanalysis
    blue_loop_output_type = args.blue_loop_output_type
    generate_plots = args.generate_plots

    analysis_results_sub_dir = os.path.join(output_dir, "analysis_results")
    plots_sub_dir = os.path.join(output_dir, "plots")
    detail_files_output_dir = os.path.join(output_dir, "detail_files")

    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    if analyze_blue_loop and blue_loop_output_type == 'all':
        os.makedirs(detail_files_output_dir, exist_ok=True)
    if generate_plots or generate_heatmaps:
        os.makedirs(plots_sub_dir, exist_ok=True)


    summary_csv_path = os.path.join(analysis_results_sub_dir, "mesa_grid_analysis_summary.csv")
    cross_csv_path = os.path.join(analysis_results_sub_dir, "cross_mesa_grid.csv")

    reanalysis_needed = force_reanalysis or \
                        not os.path.exists(summary_csv_path) or \
                        not os.path.exists(cross_csv_path)


    if not reanalysis_needed:
        print("Summary and/or cross-grid CSV files already exist. Use --force-reanalysis to re-run analysis.")
        if generate_heatmaps:
            try:
                summary_df_loaded = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])
                if 'blue_loop_crossing_count' in summary_df_loaded.columns:
                    cross_data_matrix_loaded = summary_df_loaded['blue_loop_crossing_count'].unstack(level='initial_mass')
                    print("Loaded existing summary data for heatmap generation.")
                    # --- MÓDOSÍTÁS ITT: cross_data_matrix_loaded -> cross_data_df, és hiányzó paraméterek hozzáadása ---
                    generate_heatmaps_and_time_diff_csv(
                        cross_data_df=cross_data_matrix_loaded,
                        summary_csv_path=summary_csv_path, # HOZZÁADVA
                        unique_zs=sorted(list(set(summary_df_loaded.index.get_level_values('initial_Z')))), # HOZZÁADVA
                        unique_masses=sorted(list(set(summary_df_loaded.index.get_level_values('initial_mass')))), # HOZZÁADVA
                        plots_output_dir=plots_sub_dir,
                        analysis_results_output_dir=analysis_results_sub_dir,
                        project_name=os.path.basename(input_dir),
                        blue_loop_output_type=blue_loop_output_type, # HOZZÁADVA
                        analyze_blue_loop=analyze_blue_loop # HOZZÁADVA
                    )
                    print("Heatmaps generated from existing data.")
                else:
                    print("Existing summary CSV does not contain 'blue_loop_crossing_count' for heatmaps. Skipping heatmap generation.")
            except Exception as e:
                print(f"Error loading existing data for heatmaps: {e}. Please consider using --force-reanalysis.")
        return

    print("Starting analysis of MESA runs...")
    mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)

    if not mesa_run_infos:
        print("No MESA runs found in the input directory. Exiting.")
        return

    unique_masses = sorted(list(set([run['mass'] for run in mesa_run_infos])))
    unique_zs = sorted(list(set([run['z'] for run in mesa_run_infos])))

    if not unique_masses:
        print("Error: No unique masses could be determined from runs. Exiting.")
        return
    if not unique_zs:
        print("Error: No unique metallicities (Z) could be determined from runs. Exiting.")
        return

    cross_data_matrix = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses)
    cross_data_matrix.index.name = "Z"
    cross_data_matrix.columns.name = "Mass"

    summary_data = []
    grouped_detailed_dfs = {z_val: [] for z_val in unique_zs}

    total_runs = len(mesa_run_infos)
    print(f"Found {total_runs} MESA runs for Z={unique_zs} and Mass={unique_masses}")
    print(f"Starting analysis of {total_runs} MESA runs...")

    skipped_runs_log_path = os.path.join(analysis_results_sub_dir, "skipped_runs_log.txt")
    if os.path.exists(skipped_runs_log_path):
        os.remove(skipped_runs_log_path)
    
    with tqdm(total=total_runs, desc="Analyzing MESA runs") as pbar:
        for run_info in mesa_run_infos:
            history_file_path = run_info['history_file_path']
            run_dir_path = run_info['run_dir_path']
            current_mass = run_info['mass']
            current_z = run_info['z']

            analysis_result = {
                'crossing_count': np.nan,
                'state_times': {},
                'blue_loop_detail_df': pd.DataFrame()
            }

            try:
                if not os.path.exists(history_file_path):
                    print(f"Warning: history.data not found at {history_file_path}. Skipping analysis for this run. (Path issue suspected.)")
                    if current_z in cross_data_matrix.index and current_mass in cross_data_matrix.columns:
                        cross_data_matrix.loc[current_z, current_mass] = np.nan
                    with open(skipped_runs_log_path, "a") as log_file:
                        log_file.write(f"Z={current_z}, M={current_mass}, Path: {history_file_path}, Error: history.data not found.\n")
                    pbar.update(1)
                    continue

                # Get the DataFrame from history.data using the proven np.genfromtxt method
                history_df = get_data_from_history_file(history_file_path)

                if history_df.empty:
                    print(f"Warning: DataFrame from {history_file_path} is empty. Skipping analysis for this run.")
                    if current_z in cross_data_matrix.index and current_mass in cross_data_matrix.columns:
                        cross_data_matrix.loc[current_z, current_mass] = np.nan
                    with open(skipped_runs_log_path, "a") as log_file:
                        log_file.write(f"Z={current_z}, M={current_mass}, Path: {history_file_path}, Error: Empty DataFrame.\n")
                    pbar.update(1)
                    continue

                if analyze_blue_loop:
                    # PASS THE DATAFRAME and current_mass, current_z to the analyzer
                    analysis_result = analyze_blue_loop_and_instability(history_df, current_mass, current_z)

                summary_data.append({
                    'initial_mass': current_mass,
                    'initial_Z': current_z,
                    'blue_loop_crossing_count': analysis_result.get('crossing_count', np.nan),
                    'blue_loop_start_age': analysis_result['state_times'].get('blue_loop_start_age', np.nan),
                    'blue_loop_end_age': analysis_result['state_times'].get('blue_loop_end_age', np.nan),
                    'blue_loop_duration': analysis_result['state_times'].get('blue_loop_duration', np.nan),
                    'instability_start_age': analysis_result['state_times'].get('instability_start_age', np.nan),
                    'instability_end_age': analysis_result['state_times'].get('instability_end_age', np.nan),
                    'instability_duration': analysis_result['state_times'].get('instability_duration', np.nan),
                })

                if current_z in cross_data_matrix.index and current_mass in cross_data_matrix.columns:
                    cross_data_matrix.loc[current_z, current_mass] = analysis_result.get('crossing_count', np.nan)
                else:
                    print(f"Warning: Run Z={current_z:.4f}, M={current_mass:.1f} is outside the determined grid. It will not be included in the cross-grid summary.")

                if analyze_blue_loop and blue_loop_output_type == 'all':
                    blue_loop_detail_df = analysis_result.get('blue_loop_detail_df', pd.DataFrame())
                    if not blue_loop_detail_df.empty:
                        desired_detail_cols = ["star_age", "model_number", "log_Teff", "log_L", "log_g",
                                               "center_h1", "r_mix_core", "r_mix_env"]
                                            
                        available_cols = [col for col in desired_detail_cols if col in blue_loop_detail_df.columns]
                        filtered_df = blue_loop_detail_df[available_cols].copy()

                        for col in desired_detail_cols:
                            if col not in filtered_df.columns:
                                filtered_df[col] = np.nan

                        filtered_df = filtered_df[desired_detail_cols]
                        filtered_df.insert(0, 'mass', current_mass)

                        grouped_detailed_dfs[current_z].append(filtered_df)

                if generate_plots:
                    pass # Plotting logic would go here, or in a separate function call

            except Exception as load_e:
                print(f"⚠️ Error loading or processing data for run Z={current_z:.4f}, M={current_mass:.1f} from {history_file_path}: {load_e}. Skipping this run.")
                with open(skipped_runs_log_path, "a") as log_file:
                    log_file.write(f"Z={current_z}, M={current_mass}, Path: {history_file_path}, Error: {load_e}\n")

                if current_z in cross_data_matrix.index and current_mass in cross_data_matrix.columns:
                    cross_data_matrix.loc[current_z, current_mass] = np.nan
            pbar.update(1)

    print("\nAnalysis complete. Saving results...")

    summary_df = pd.DataFrame(summary_data)
    full_grid_index = pd.MultiIndex.from_product([unique_zs, unique_masses],
                                                  names=['initial_Z', 'initial_mass'])
    final_summary_df = summary_df.set_index(['initial_Z', 'initial_mass']).reindex(full_grid_index).sort_index()
    final_summary_df.to_csv(summary_csv_path)
    print(f"Summary results saved to {summary_csv_path}")


    cross_data_matrix = cross_data_matrix.loc[unique_zs, unique_masses]
    cross_data_matrix.to_csv(cross_csv_path, index=True, index_label="Z / Mass")
    print(f"Cross-grid results saved to {cross_csv_path}")


    if analyze_blue_loop and blue_loop_output_type == 'all':
        print(f"Saving detailed per-metallicity files to '{detail_files_output_dir}'...")
        for z_val, list_of_dfs in grouped_detailed_dfs.items():
            if list_of_dfs:
                combined_df_for_z = pd.concat(list_of_dfs, ignore_index=True)

                if 'initial_mass' in combined_df_for_z.columns and 'mass' not in combined_df_for_z.columns:
                    combined_df_for_z.rename(columns={'initial_mass': 'mass'}, inplace=True)
                
                final_detail_header = ["mass", "star_age", "model_number", "log_Teff", "log_L", "log_g",
                                       "center_h1"]
                # Add nu_radial and eta_radial columns back to the header if they are expected in detail files
                # Based on your previous code, these were in the header.
#                final_detail_header.extend([f"nu_radial_{i}" for i in range(40)])
#                final_detail_header.extend([f"eta_radial_{i}" for i in range(40)])


                for col in final_detail_header:
                    if col not in combined_df_for_z.columns:
                        combined_df_for_z[col] = np.nan

                final_combined_df_sorted = combined_df_for_z[final_detail_header].sort_values(by=['mass', 'star_age'])

                detail_filename_z = f"detail_Z{z_val:.4f}.csv"
                detail_file_path_z = os.path.join(detail_files_output_dir, detail_filename_z)
                final_combined_df_sorted.to_csv(detail_file_path_z, index=False)
                print(f"    Saved: {detail_file_path_z}")

                final_summary_df.loc[final_summary_df.index.get_level_values('initial_Z') == z_val,
                                     'aggregated_detail_file'] = os.path.relpath(detail_file_path_z, output_dir)
            else:
                print(f"    No detailed data to save for Z={z_val}.")
                final_summary_df.loc[final_summary_df.index.get_level_values('initial_Z') == z_val,
                                     'aggregated_detail_file'] = np.nan


    if generate_heatmaps:
        print("Generating heatmaps...")
        # --- MÓDOSÍTÁS ITT: cross_data_matrix -> cross_data_df, és hiányzó paraméterek hozzáadása ---
        generate_heatmaps_and_time_diff_csv(
            cross_data_df=cross_data_matrix,
            summary_csv_path=summary_csv_path, 
            unique_zs=unique_zs,               
            unique_masses=unique_masses,       
            plots_output_dir=plots_sub_dir,
            analysis_results_output_dir=analysis_results_sub_dir,
            model_name=os.path.basename(input_dir),
            blue_loop_output_type=blue_loop_output_type, # HOZZÁADVA
            analyze_blue_loop=analyze_blue_loop # HOZZÁADVA
        )
        print("Heatmaps generated.")
    else:
        print("Heatmap generation skipped (use --generate-heatmaps).")


if __name__ == "__main__":
    main()