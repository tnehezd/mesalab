import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.path import Path
import os

# --- Instability Strip definition ---
INSTABILITY_STRIP_VERTICES = np.array([
    [3.83, 2.4],   # Top-left (Blue edge)
    [3.76, 4.5],   # Bottom-left
    [3.65, 4.5],   # Bottom-right (Red edge)
    [3.77, 2.4]    # Top-right
])
instability_path = Path(INSTABILITY_STRIP_VERTICES)

def is_in_instability_strip(log_Teff, log_L):
    """Checks if the star is within the instability strip."""
    return instability_path.contains_point((log_Teff, log_L))

# --- Function to load history.data files using np.genfromtxt (matching your existing pipeline) ---
def load_mesa_history_data_genfromtxt(filepath):
    """
    Loads MESA history.data files using np.genfromtxt, assuming the header is
    skipped and names are read from the relevant line, similar to your existing code.
    """
    try:
        # Use np.genfromtxt with the same parameters you use
        # names=True reads column names from the header line
        # comments='#' ignores lines starting with #
        # skip_header=5 means it skips the first 5 lines (0-indexed 0-4),
        # so the header (names) is assumed to be on the 6th line (index 5)
        # and data starts from the 7th line (index 6).
        data = np.genfromtxt(filepath, names=True, comments="#", skip_header=5)

        # Convert the numpy structured array to a pandas DataFrame for easier handling
        df = pd.DataFrame(data)

        # Ensure all columns are numeric, coercing errors to NaN
        for col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
        
        # Drop rows where 'model_number' is NaN, as these are often incomplete or malformed data rows
        if 'model_number' in df.columns:
            df.dropna(subset=['model_number'], inplace=True)
            if not df['model_number'].isnull().any():
                df['model_number'] = df['model_number'].astype(int)

        return df

    except Exception as e:
        print(f"Error reading {filepath} with np.genfromtxt: {e}")
        return pd.DataFrame()

# --- Main script logic ---
if __name__ == "__main__":
    # >>> UPDATE THIS PATH TO YOUR PROBLEMATIC HISTORY.DATA FILE <<<
    history_filepath = '/home/tnehezd/workdir/mesa-r23.05.1/STRANGE/nad_convos_mid/run_nad_convos_mid_14.0MSUN_z0.0165/LOGS/history.data'

    print(f"Loading data from: {history_filepath}")
    # Use the np.genfromtxt-based loader
    history_df = load_mesa_history_data_genfromtxt(history_filepath)

    if history_df.empty:
        print("Failed to load history data or data is empty. Exiting.")
    else:
        print(f"Data loaded successfully. Shape: {history_df.shape}. Plotting HR Diagram...")
        # Sort data by star_age for chronological analysis
        history_df = history_df.sort_values(by='star_age').reset_index(drop=True)

        # Check for required columns
        required_columns = ['log_Teff', 'log_L', 'center_h1', 'star_age', 'model_number', 'log_g']
        if not all(col in history_df.columns for col in required_columns):
            missing_cols = [col for col in required_columns if col not in history_df.columns]
            print(f"Error: Missing required columns for plotting or analysis: {missing_cols}. Exiting.")
        else:
            # HR Diagram plot
            plt.figure(figsize=(10, 8))
            plt.plot(history_df['log_Teff'], history_df['log_L'], color='grey', linestyle='--', label='Full Track')

            # Highlight Main Sequence (MS) phase (e.g., until center_h1 < 1e-4)
            ms_end_candidates = history_df[history_df['center_h1'] < 1e-4]
            ms_end_idx = ms_end_candidates.index[0] if not ms_end_candidates.empty else None

            if ms_end_idx is not None:
                plt.plot(history_df.loc[:ms_end_idx, 'log_Teff'], history_df.loc[:ms_end_idx, 'log_L'], color='blue', linewidth=2, label='MS Phase')
                plt.plot(history_df['log_Teff'].iloc[ms_end_idx], history_df['log_L'].iloc[ms_end_idx], 'o', color='purple', markersize=8, label='MS End')

                # Identify blue loop phase (often after a local minimum in LogL post-MS)
                post_ms_data = history_df.iloc[ms_end_idx:].copy()
                if not post_ms_data.empty and 'log_L' in post_ms_data.columns and not post_ms_data['log_L'].isnull().all():
                    logL_min_relative_idx = np.argmin(post_ms_data['log_L'].values)
                    logL_min_abs_idx = ms_end_idx + logL_min_relative_idx
                    
                    plt.plot(history_df['log_Teff'].iloc[logL_min_abs_idx], history_df['log_L'].iloc[logL_min_abs_idx], 's', color='orange', markersize=8, label='Min Post-MS L')

                    # Identify AGB start (if luminosity starts increasing again)
                    if logL_min_abs_idx < len(history_df) - 1:
                        candidate_agb_start_indices = np.where(history_df['log_L'].values[logL_min_abs_idx+1:] > history_df['log_L'].values[logL_min_abs_idx])[0]
                        
                        if candidate_agb_start_indices.size > 0:
                            agb_start_relative_idx = candidate_agb_start_indices[0]
                            agb_start_abs_idx = logL_min_abs_idx + 1 + agb_start_relative_idx
                            
                            plt.plot(history_df.iloc[agb_start_abs_idx:]['log_Teff'], history_df.iloc[agb_start_abs_idx:]['log_L'], color='red', linewidth=2, label='Blue Loop Candidate Phase (or AGB)')
                            plt.plot(history_df['log_Teff'].iloc[agb_start_abs_idx], history_df['log_L'].iloc[agb_start_abs_idx], 'x', color='green', markersize=10, label='Blue Loop/AGB Start')
                        else:
                            print("No clear AGB phase start detected (luminosity doesn't increase after minimum).")
                    else:
                        print("Not enough data points after min post-MS L to detect AGB start.")
                else:
                    print("Log_L column is missing or all NaN in post-MS data, cannot identify blue loop candidate phase.")
            else:
                print("MS end not identified. Cannot plot post-MS phases.")

            # Plot Instability Strip
            plt.fill(INSTABILITY_STRIP_VERTICES[:, 0], INSTABILITY_STRIP_VERTICES[:, 1], 'cyan', alpha=0.3, label='Instability Strip')
            plt.plot(INSTABILITY_STRIP_VERTICES[[0,1,2,3,0], 0], INSTABILITY_STRIP_VERTICES[[0,1,2,3,0], 1], 'cyan', linestyle='--', linewidth=2, label='IS Edges')

            # Mark points inside the Instability Strip
            in_is_points = history_df[history_df.apply(lambda row: is_in_instability_strip(row['log_Teff'], row['log_L']), axis=1)]
            if not in_is_points.empty:
                plt.scatter(in_is_points['log_Teff'], in_is_points['log_L'], color='purple', marker='o', s=50, edgecolors='black', zorder=5, label='Points in IS')
                print(f"Detected {len(in_is_points)} points in the Instability Strip.")
            else:
                print("No points detected in the Instability Strip according to is_in_instability_strip.")

            plt.xlabel(r'$\log(\mathrm{T_{eff}})$')
            plt.ylabel(r'$\log(\mathrm{L/L_{\odot}})$')
            plt.title('HR Diagram with Instability Strip and Evolutionary Track')
            plt.gca().invert_xaxis() # Teff increases to the left
            plt.legend()
            plt.grid(True)
            plt.tight_layout()
            plt.savefig('hr_diagram_debug.png') # Save the plot
            plt.show()