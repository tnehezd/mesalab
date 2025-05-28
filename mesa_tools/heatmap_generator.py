import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns # Visszaadva a seaborn import
from matplotlib.colors import ListedColormap, BoundaryNorm
import csv

def generate_heatmaps_and_time_diff_csv(cross_data_df, summary_csv_path, unique_zs, unique_masses, 
                                        plots_output_dir, analysis_results_output_dir, 
                                        model_name="MESA Grid Analysis", 
                                        blue_loop_output_type='all', analyze_blue_loop=False):
    """
    Generates heatmaps for blue loop crossing counts and potentially other metrics,
    and calculates time differences.
    """
    
    # --- cross_data_df already contains the processed data from cli.py ---
    # We do NOT reload it here, as cli.py already handles the loading from cross.csv
    # and passes a clean DataFrame.

    # Ensure index and columns are float for proper numerical operations and plotting
    # This is crucial if the DataFrame was loaded from CSV with string columns/index
    # The cli.py should ensure this, but a safeguard here is good.
    try:
        cross_data_df.columns = cross_data_df.columns.astype(float)
        cross_data_df.index = cross_data_df.index.astype(float)
    except ValueError as e:
        print(f"Warning: Could not convert cross_data_df columns or index to float in heatmap_generator. Error: {e}")
        print("This might indicate an issue with how the cross.csv was originally generated or loaded.")
        # If conversion fails, the heatmap might not plot correctly, or subsequent operations might break.

    # Ensure the unique_zs and unique_masses align with the DataFrame's actual order.
    # This is important for consistent plotting.
    unique_zs_sorted = cross_data_df.index.unique().sort_values().tolist()
    unique_masses_sorted = cross_data_df.columns.unique().sort_values().tolist()

    # Reindex the DataFrame to ensure it uses the sorted unique_zs and unique_masses
    # This handles cases where some (Z,M) pairs might be missing or in a different order.
    cross_data_df_reindexed = cross_data_df.reindex(index=unique_zs_sorted, columns=unique_masses_sorted)
    
    print(f"Generating heatmap for '{model_name}'...")
    
    # --- Heatmap: Blue Loop Crossing Count ---
    # Custom colormap: -1 (black), 0–5 (viridis), NaN (white)
    # The original heatmap used a different colormap and bounds. Let's adapt it.
    
    # Define colors for 0 to 5 crossings. You can customize these.
    # Example: 0=light grey, 1=light blue, 2=blue, 3=dark blue, 4=purple, 5=dark purple
    colors = ["#EEEEEE", "#A8DADC", "#457B9D", "#1D3557", "#440154", "#21918c"]
    # Or your original colors if you prefer:
    # colors = ["#440154", "#3b528b", "#21918c", "#5ec962", "#fde725", "#ffb14e"] # 0-5 szintekhez
    
    cmap = ListedColormap(colors)
    cmap.set_bad(color="white")  # NaN values will remain white
    
    # Color scale bounds: 0, 1, 2, 3, 4, 5 (6 steps)
    # If 0 represents no crossing, and you want it to be distinct from NaN,
    # then 0 should be a valid color.
    # The boundaries define where the color transitions happen.
    # For integer data 0,1,2,...:
    # bounds = [-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5] would map 0 to first color, 1 to second etc.
    # If 0 should be treated as a separate category, and values are integers:
    bounds = [0, 1, 2, 3, 4, 5, 6] # For categories 0,1,2,3,4,5
    norm = BoundaryNorm(bounds, cmap.N)

    plt.figure(figsize=(15, 10)) # Adjust figure size
    # Replace 0 with NaN if 0 should be white (no crossing) and not a distinct color
    # If 0 is a distinct category, do not replace with NaN.
    # sns.heatmap(cross_data_df_reindexed.replace(0, np.nan), annot=True, cmap=cmap, fmt=".0f", linewidths=.5, linecolor='black', norm=norm)
    
    # If 0 should be a valid color (e.g., grey), do not replace with NaN.
    # Use your original color mapping with the values directly.
    sns.heatmap(cross_data_df_reindexed, annot=True, cmap=cmap, fmt=".0f", linewidths=.5, linecolor='black', norm=norm)

    plt.title(f'Blue Loop Crossing Count Heatmap for {model_name}')
    plt.xlabel('Initial Mass (Msun)')
    plt.ylabel('Initial Metallicity (Z)')
    # Set ticks and labels according to the actual sorted unique masses and Zs
    plt.xticks(np.arange(len(unique_masses_sorted)) + 0.5, [f'{m:.1f}' for m in unique_masses_sorted], rotation=90)
    plt.yticks(np.arange(len(unique_zs_sorted)) + 0.5, [f'{z:.4f}' for z in unique_zs_sorted], rotation=0) # No rotation needed for Z
    
    # Colorbar settings
    cbar = plt.colorbar(ticks=[0.5, 1.5, 2.5, 3.5, 4.5, 5.5], boundaries=[0,1,2,3,4,5,6]) # Centered ticks for categories
    cbar.set_label("Crossing IS edge")
    cbar.ax.set_yticklabels(["0", "1", "2", "3", "4", "5"])
    
    plt.tight_layout()
    heatmap_filename = os.path.join(plots_output_dir, f'heatmap_mass_metallicity_{model_name.replace(" ", "_")}.png')
    plt.savefig(heatmap_filename, dpi=300)
    plt.close()
    print(f"Heatmap saved to {heatmap_filename}")

    # --- Time Difference Calculations and Heatmaps (if analyze_blue_loop is True) ---
    if analyze_blue_loop and blue_loop_output_type == 'all' and os.path.exists(summary_csv_path):
        print("Extracting and saving time differences...")
        try:
            summary_df = pd.read_csv(summary_csv_path)
            
            # Ensure proper data types for calculations
            summary_df['initial_mass'] = pd.to_numeric(summary_df['initial_mass'], errors='coerce')
            summary_df['initial_Z'] = pd.to_numeric(summary_df['initial_Z'], errors='coerce')
            summary_df['ms_end_age'] = pd.to_numeric(summary_df['ms_end_age'], errors='coerce')
            summary_df['min_teff_post_ms_age'] = pd.to_numeric(summary_df['min_teff_post_ms_age'], errors='coerce')

            # Calculate the difference if both values are valid
            summary_df['bl_duration_diff'] = summary_df['min_teff_post_ms_age'] - summary_df['ms_end_age']

            # Create a pivot table for the time difference heatmap
            time_diff_pivot = summary_df.pivot_table(
                index='initial_Z', 
                columns='initial_mass', 
                values='bl_duration_diff'
            )
            
            # Reindex to ensure consistency with unique_zs and unique_masses order and completeness
            time_diff_pivot = time_diff_pivot.reindex(index=unique_zs_sorted, columns=unique_masses_sorted)

            # Save time difference data to CSV
            time_diff_csv_path = os.path.join(analysis_results_output_dir, f'time_difference_ms_minteff_{model_name.replace(" ", "_")}.csv')
            time_diff_pivot.to_csv(time_diff_csv_path, index=True, index_label="Z / Mass")
            print(f"Time difference data saved to {time_diff_csv_path}")

            # Heatmap: Time Difference
            plt.figure(figsize=(15, 10)) # Adjust figure size
            sns.heatmap(time_diff_pivot, annot=True, cmap='coolwarm', fmt=".2f", linewidths=.5, linecolor='black')
            plt.title(f'Blue Loop Duration (min_Teff_post_MS - MS_End) Heatmap for {model_name}')
            plt.xlabel('Initial Mass (Msun)')
            plt.ylabel('Initial Metallicity (Z)')
            
            # Set ticks and labels for time difference heatmap as well
            plt.xticks(np.arange(len(unique_masses_sorted)) + 0.5, [f'{m:.1f}' for m in unique_masses_sorted], rotation=90)
            plt.yticks(np.arange(len(unique_zs_sorted)) + 0.5, [f'{z:.4f}' for z in unique_zs_sorted], rotation=0)
            
            plt.tight_layout()
            time_diff_heatmap_filename = os.path.join(plots_output_dir, f'heatmap_time_diff_{model_name.replace(" ", "_")}.png')
            plt.savefig(time_diff_heatmap_filename, dpi=300)
            plt.close()
            print(f"Time difference heatmap saved to {time_diff_heatmap_filename}")

        except Exception as e:
            print(f"Error generating time differences: {e}. Please ensure 'matplotlib' and 'pandas' libraries are installed and verify data integrity.")
    else:
        if analyze_blue_loop and blue_loop_output_type != 'all':
            print("Skipping time difference calculation: --blue-loop-output-type is not 'all'.")
        elif not os.path.exists(summary_csv_path):
            print(f"Skipping time difference calculation: Summary CSV not found at {summary_csv_path}")
        else:
            print("Time difference calculation skipped (not requested or not applicable).")