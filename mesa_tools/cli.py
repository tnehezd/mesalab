import os
import argparse
import pandas as pd
import numpy as np
import re
from tqdm import tqdm

# --- Import your analyzer functions ---
from mesa_tools.blue_loop_analyzer import analyze_blue_loop_and_instability
from mesa_tools.heatmap_generator import generate_heatmaps_and_time_diff_csv


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
        data = np.genfromtxt(history_file_path, names=True, comments="#", skip_header=5,
                             dtype=None, encoding='utf-8')
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

        return df

    except Exception as e:
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
                        help="Blue loop output type: 'summary' or 'all'.")
    parser.add_argument("--generate-plots", action="store_true",
                        help="Generate plots for analysis.")

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

    reanalysis_needed = force_reanalysis or not os.path.exists(summary_csv_path) or not os.path.exists(cross_csv_path)

    if not reanalysis_needed:
        print("Summary and cross-grid CSV files already exist. Use --force-reanalysis to re-run analysis.")
        if generate_heatmaps:
            try:
                summary_df_loaded = pd.read_csv(summary_csv_path, index_col=['initial_Z', 'initial_mass'])
                if 'blue_loop_crossing_count' in summary_df_loaded.columns:
                    cross_data_matrix_loaded = summary_df_loaded['blue_loop_crossing_count'].unstack(level='initial_mass')
                    print("Loaded existing summary data for heatmap generation.")

                    generate_heatmaps_and_time_diff_csv(
                        cross_data_df=cross_data_matrix_loaded,
                        summary_csv_path=summary_csv_path,
                        unique_zs=sorted(list(set(summary_df_loaded.index.get_level_values('initial_Z')))),
                        unique_masses=sorted(list(set(summary_df_loaded.index.get_level_values('initial_mass')))),
                        plots_output_dir=plots_sub_dir,
                        analysis_results_output_dir=analysis_results_sub_dir,
                        model_name=os.path.basename(input_dir),  # javított: 'model_name'
                        blue_loop_output_type=blue_loop_output_type,
                        analyze_blue_loop=analyze_blue_loop
                    )
                    print("Heatmaps generated from existing data.")
                else:
                    print("Summary CSV lacks 'blue_loop_crossing_count'; skipping heatmap generation.")
            except Exception as e:
                print(f"Error loading data for heatmaps: {e}. Use --force-reanalysis to retry.")
        return

    print("Starting analysis of MESA runs...")
    mesa_run_infos = scan_mesa_runs(input_dir, inlist_name)

    if not mesa_run_infos:
        print("No MESA runs found. Exiting.")
        return

    unique_masses = sorted(set(run['mass'] for run in mesa_run_infos))
    unique_zs = sorted(set(run['z'] for run in mesa_run_infos))

    if not unique_masses or not unique_zs:
        print("Error: Could not determine unique masses or metallicities from runs. Exiting.")
        return

    cross_data_matrix = pd.DataFrame(np.nan, index=unique_zs, columns=unique_masses)
    cross_data_matrix.index.name = "Z"
    cross_data_matrix.columns.name = "Mass"

    summary_data = []
    grouped_detailed_dfs = {z_val: [] for z_val in unique_zs}

    total_runs = len(mesa_run_infos)
    print(f"Found {total_runs} runs for Z={unique_zs} and Mass={unique_masses}")

    skipped_runs_log_path = os.path.join(analysis_results_sub_dir, "skipped_runs_log.txt")
    if os.path.exists(skipped_runs_log_path):
        os.remove(skipped_runs_log_path)

    with tqdm(total=total_runs, desc="Analyzing MESA runs") as pbar:
        for run_info in mesa_run_infos:
            history_file_path = run_info['history_file_path']
            current_mass = run_info['mass']
            current_z = run_info['z']

            analysis_result = {
                'crossing_count': 0,
                'duration': np.nan,
                'max_luminosity': np.nan,
                'max_teff': np.nan,
                'max_radius': np.nan,
                'first_model_number': None,
                'last_model_number': None,
                'first_age': np.nan,
                'last_age': np.nan,
            }

            try:
                df = get_data_from_history_file(history_file_path)

                if analyze_blue_loop:
                    analyze_blue_loop_and_instability(df, current_mass, current_z)

                    # Read summary CSV generated by analyze_blue_loop (assumed path)
                    run_name = os.path.basename(run_info['run_dir_path'])
                    summary_file_path = os.path.join(analysis_results_sub_dir, f"{run_name}_blue_loop_summary.csv")

                    if os.path.exists(summary_file_path):
                        summary_df = pd.read_csv(summary_file_path)
                        if not summary_df.empty:
                            # Assuming summary_df has relevant columns, take first row:
                            row = summary_df.iloc[0]
                            analysis_result['crossing_count'] = int(row.get('blue_loop_crossing_count', 0))
                            analysis_result['duration'] = float(row.get('blue_loop_duration', np.nan))
                            analysis_result['max_luminosity'] = float(row.get('max_log_L', np.nan))
                            analysis_result['max_teff'] = float(row.get('max_log_Teff', np.nan))
                            analysis_result['max_radius'] = float(row.get('max_log_R', np.nan))
                            analysis_result['first_model_number'] = int(row.get('first_model_number', 0))
                            analysis_result['last_model_number'] = int(row.get('last_model_number', 0))
                            analysis_result['first_age'] = float(row.get('first_age_yr', np.nan))
                            analysis_result['last_age'] = float(row.get('last_age_yr', np.nan))

                            cross_data_matrix.at[current_z, current_mass] = analysis_result['crossing_count']

                    else:
                        print(f"Warning: Summary file not found for run {run_info['run_dir_path']}. Skipping summary extraction.")

                else:
                    # If no blue loop analysis, you can fill summary info from df if needed
                    pass

                # Store summary data for output CSV
                summary_data.append({
                    'initial_mass': current_mass,
                    'initial_Z': current_z,
                    **analysis_result
                })

                # Store detailed df per Z for potential concatenation later
                grouped_detailed_dfs[current_z].append(df)

            except Exception as err:
                with open(skipped_runs_log_path, 'a') as log_file:
                    log_file.write(f"Skipped run {run_info['run_dir_path']} due to error: {err}\n")
                print(f"Skipped run {run_info['run_dir_path']} due to error: {err}")

            pbar.update(1)

    # Write summary CSV
    summary_df = pd.DataFrame(summary_data)
    summary_df.sort_values(['initial_Z', 'initial_mass'], inplace=True)
    summary_df.set_index(['initial_Z', 'initial_mass'], inplace=True)
    summary_df.to_csv(summary_csv_path)
    print(f"Summary CSV written to {summary_csv_path}")

    # Write cross-grid CSV
    cross_data_matrix.to_csv(cross_csv_path)
    print(f"Cross-grid CSV written to {cross_csv_path}")

    # Write concatenated detail files per Z
    for z_val, dfs_list in grouped_detailed_dfs.items():
        if dfs_list:
            try:
                combined_df = pd.concat(dfs_list, ignore_index=True)
                detail_filename = os.path.join(detail_files_output_dir, f"detail_z{z_val:.4f}_{blue_loop_output_type}.csv")
                combined_df.to_csv(detail_filename, index=False)
                print(f"Written concatenated detail CSV for Z={z_val} to {detail_filename}")
            except Exception as e:
                print(f"Error writing detail CSV for Z={z_val}: {e}")

    # Generate heatmaps if requested
    if generate_heatmaps:
        try:
            generate_heatmaps_and_time_diff_csv(
                cross_data_df=cross_data_matrix,
                summary_csv_path=summary_csv_path,
                unique_zs=unique_zs,
                unique_masses=unique_masses,
                plots_output_dir=plots_sub_dir,
                analysis_results_output_dir=analysis_results_sub_dir,
                model_name=os.path.basename(input_dir),
                blue_loop_output_type=blue_loop_output_type,
                analyze_blue_loop=analyze_blue_loop
            )
            print("Heatmaps generated successfully.")
        except Exception as e:
            print(f"Error generating heatmaps: {e}")

if __name__ == "__main__":
    main()
