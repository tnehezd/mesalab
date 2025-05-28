import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.colors import ListedColormap, BoundaryNorm
from tqdm import tqdm
import os
import csv
import json

def generate_blue_loop_heatmap_revised(summary_df_path, plots_output_dir):
    """
    Generates a heatmap of blue loop crossing counts and a CSV of time differences.
    This version assumes the summary CSV always uses the legacy list-in-string format.

    Args:
        summary_df_path (str): Path to the main summary CSV (mesa_grid_analysis_summary.csv).
        plots_output_dir (str): Path to the directory where plots will be saved.
    """
    try:
        print(f"Loading summary data from {summary_df_path} (assuming legacy format)...")
        # Special loading for the legacy format (this is now the default and only behavior)
        temp_df = pd.read_csv(summary_df_path, index_col=0)
        # Convert Mass columns to float, Z index to float
        temp_df.columns = temp_df.columns.astype(float)
        temp_df.index = temp_df.index.astype(float)

        crossing_counts_data = {}
        time_differences = {}

        print("Processing legacy summary format for heatmap...")
        for z_val in tqdm(temp_df.index, desc="Processing metallicities"):
            for m_val in temp_df.columns:
                value_str = temp_df.loc[z_val, m_val]
                if isinstance(value_str, str) and value_str.startswith('['):
                    try:
                        # Use json.loads for robust parsing of stringified lists
                        value_list = json.loads(value_str.replace("'", '"')) 
                        
                        # Ensure all elements are numbers; replace 'N/A' or empty with NaN
                        value_list = [v if (isinstance(v, (int, float)) and not np.isnan(v)) else np.nan for v in value_list]

                        crossing_counts_data[(z_val, m_val)] = value_list[0] # First element is the crossing count

                        # Calculate time differences as per your original code
                        diff_2_1 = value_list[2] - value_list[1] if len(value_list) >= 3 and not np.isnan(value_list[1]) and not np.isnan(value_list[2]) else np.nan
                        diff_3_2 = value_list[3] - value_list[2] if len(value_list) >= 4 and not np.isnan(value_list[2]) and not np.isnan(value_list[3]) else np.nan
                        diff_4_3 = value_list[4] - value_list[3] if len(value_list) >= 5 and not np.isnan(value_list[3]) and not np.isnan(value_list[4]) else np.nan

                        # Only save if at least one difference is calculable
                        if not np.isnan(diff_2_1) or not np.isnan(diff_3_2) or not np.isnan(diff_4_3):
                            time_differences[(z_val, m_val)] = {'2-1': diff_2_1, '3-2': diff_3_2, '4-3': diff_4_3}

                    except Exception as e:
                        # print(f"Warning: Could not parse legacy list '{value_str}' at Z={z_val}, M={m_val}: {e}")
                        crossing_counts_data[(z_val, m_val)] = np.nan
                else:
                    crossing_counts_data[(z_val, m_val)] = np.nan
        
        # Convert the collected data into a DataFrame suitable for heatmap generation
        masses = sorted(list(set(m for z,m in crossing_counts_data.keys())))
        metallicities = sorted(list(set(z for z,m in crossing_counts_data.keys())))
        
        data = np.full((len(metallicities), len(masses)), np.nan)
        mass_to_col_idx = {m: i for i, m in enumerate(masses)}
        z_to_row_idx = {z: i for i, z in enumerate(metallicities)}

        for (z, m), count in crossing_counts_data.items():
            if z in z_to_row_idx and m in mass_to_col_idx:
                data[z_to_row_idx[z], mass_to_col_idx[m]] = count


        # Determine max crossing count for colormap scaling
        max_crossing_count = int(np.nanmax(data)) if not np.all(np.isnan(data)) else 0
        if max_crossing_count < 5: 
            max_crossing_count = 5

        # Custom colormap (using your original heatmap code's colors)
        colors_original = ["#440154", "#3b528b", "#21918c", "#5ec962", "#fde725", "#ffb14e"]
        if max_crossing_count >= len(colors_original): 
            colors_viridis_like = plt.cm.viridis(np.linspace(0, 1, max_crossing_count + 1))
            cmap = ListedColormap(colors_viridis_like)
        else:
            cmap = ListedColormap(colors_original[:max_crossing_count + 1]) 
        
        cmap.set_bad(color="white")

        # Color scale bounds: 0, 1, 2, ..., max_crossing_count
        bounds = np.arange(-0.5, max_crossing_count + 0.5, 1)
        norm = BoundaryNorm(bounds, cmap.N)

        # Create the heatmap
        plt.figure(figsize=(15, 10))
        plt.imshow(data, aspect='auto', origin='lower', cmap=cmap, norm=norm)

        # Colorbar settings
        cbar_ticks = np.arange(0, max_crossing_count + 1)
        cbar = plt.colorbar(ticks=cbar_ticks)
        cbar.set_label("Number of Blue Loop Crossings")
        cbar.ax.set_yticklabels([str(int(t)) for t in cbar_ticks]) 

        # Axis settings
        plt.xticks(np.arange(len(masses)), [f'{m:.1f}' for m in masses], rotation=90)
        y_tick_step = max(1, len(metallicities) // 10) 
        plt.yticks(np.arange(0, len(metallicities), y_tick_step), [f'{metallicities[i]:.4f}' for i in np.arange(0, len(metallicities), y_tick_step)])
        
        plt.xlabel("Mass [M$_\odot$]")
        plt.ylabel("Metallicity (Z)")
        plt.title("Heatmap: Blue Loop Crossing Counts")

        # Save the image
        image_file_name = "heatmap_blue_loop_crossings.png"
        image_file_path = os.path.join(plots_output_dir, image_file_name)
        plt.savefig(image_file_path, bbox_inches='tight', dpi=300)
        print(f"Heatmap saved to {image_file_path}")
        plt.close()

        # Save the time differences to a CSV file (this will always be saved if data is available)
        if time_differences:
            analysis_results_dir = os.path.dirname(summary_df_path) 
            output_file_name = "time_spent_blue_loop.csv"
            output_file_path = os.path.join(analysis_results_dir, output_file_name)

            with open(output_file_path, mode='w', newline='') as file:
                writer = csv.writer(file)
                writer.writerow(['Z (Metallicity)', 'M (Mass)', '2-1', '3-2', '4-3'])
                
                for (z, m), diffs in time_differences.items():
                    writer.writerow([z, m, diffs.get('2-1', np.nan), diffs.get('3-2', np.nan), diffs.get('4-3', np.nan)])

            print(f"Time differences have been saved to {output_file_path}")
        else:
            print("No specific time differences for blue loop crossings were calculated or saved.")


    except FileNotFoundError as e:
        print(f"Error: Required file not found for heatmap generation: {e}. Please ensure analysis has been run.")
    except Exception as e:
        print(f"An unexpected error occurred during heatmap generation: {e}")