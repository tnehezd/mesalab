import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from isochrones.mist.bc import MISTBolometricCorrectionGrid
import matplotlib.ticker as ticker
import sys # Import sys for printing to stderr
import traceback # Import traceback for detailed error messages
import pkg_resources # To get installed package versions

# Initialize the bolometric correction grid ONCE when this module is loaded.
# Include Gaia bands (G, BP, RP) along with UBVRI.
try:
    # IMPORTANT: Include Gaia bands for BCs.
    bc_grid = MISTBolometricCorrectionGrid(['U', 'B', 'V', 'R', 'I', 'G', 'BP', 'RP'])
except Exception as e:
    print(f"ERROR: Error initializing MISTBolometricCorrectionGrid in blue_loop_cmd_plotter: {e}", file=sys.stderr)
    print("WARNING: Bolometric corrections will not be available for plots.", file=sys.stderr)
    bc_grid = None # Set to None if initialization fails

def z_to_feh(Z):
    """Converts metallicity (Z) to [Fe/H]."""
    Z_sun = 0.0152 # Solar metallicity from Asplund et al. (2009) or other standard
    # Ensure Z is not zero or negative to avoid log(0) or log(negative)
    if Z <= 0:
        print(f"WARNING: Z value is non-positive ({Z}). Returning NaN for [Fe/H].", file=sys.stderr)
        return np.nan
    return np.log10(Z / Z_sun)

def generate_blue_loop_plots_with_bc(grouped_detailed_dfs, output_dir, output_type_label="combined_blue_loop_data"):
    """
    Generates HRD, CMD (G_BP - G_RP vs G), and LogL-LogG plots for blue loop stellar models,
    including bolometric corrections for Gaia magnitudes.

    Args:
        grouped_detailed_dfs (dict): A dictionary where keys are Z values and
                                     values are DataFrames containing detailed
                                     stellar evolution data for that Z.
        output_dir (str): The directory where plots will be saved.
        output_type_label (str): A label to include in the output filenames.
    """
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    print(f"Generating blue loop specific HRD, CMD, and LogL-LogG plots (with BCs) to '{output_dir}'...")

    # Get installed isochrones version for debugging
    try:
        isochrones_version = pkg_resources.get_distribution('isochrones').version
        print(f"INFO: Installed isochrones version: {isochrones_version}")
    except pkg_resources.DistributionNotFound:
        print("WARNING: isochrones not installed or version not found. Cannot determine version.", file=sys.stderr)


    # Check if BC grid is available early for CMD plotting decision
    cmd_plotting_possible = bc_grid is not None
    if not cmd_plotting_possible:
        print(f"WARNING: Skipping all CMD plots due to BC grid initialization error.", file=sys.stderr)

    for Z_val, combined_df_bl in grouped_detailed_dfs.items():
        if combined_df_bl.empty:
            print(f"Warning: No blue loop data found for Z={Z_val}. Skipping plots for this metallicity.")
            continue

        unique_masses = combined_df_bl['initial_mass'].unique()
        unique_masses.sort()
        feh_val = z_to_feh(Z_val)
        
        # Check if Fe/H is valid
        if not np.isfinite(feh_val):
            print(f"ERROR: Invalid Z value ({Z_val}) resulted in non-finite [Fe/H] ({feh_val}). Skipping plots for this Z.", file=sys.stderr)
            continue

        # Set up plots for HRD, CMD, and LogL-LogG
        fig_hrd, ax_hrd = plt.subplots(figsize=(10, 8))
        fig_cmd, ax_cmd = plt.subplots(figsize=(10, 8))
        fig_logg, ax_logg = plt.subplots(figsize=(10, 8))
        
        # Keep track if at least one CMD plot was generated for this Z
        at_least_one_cmd_plot = False

        for mass_val in unique_masses:
            mass_df = combined_df_bl[combined_df_bl['initial_mass'] == mass_val].copy()

            if mass_df.empty:
                print(f"Warning: No data for M={mass_val:.1f}, Z={Z_val:.4f}. Skipping plots for this mass.")
                continue

            # HRD Plot (always possible with log_Teff and log_L from original data)
            hrd_df_for_plot = mass_df.dropna(subset=['log_Teff', 'log_L'])
            if not hrd_df_for_plot.empty:
                ax_hrd.plot(hrd_df_for_plot['log_Teff'], hrd_df_for_plot['log_L'], label=f'M={mass_val:.1f}', lw=1.5)
            else:
                print(f"Warning: No valid 'log_Teff'/'log_L' data for HRD for M={mass_val:.1f}, Z={Z_val:.4f}. Skipping HRD plot for this mass.", file=sys.stderr)

            # LogL-LogG Plot (always possible with log_g and log_L from original data)
            logg_l_df_for_plot = mass_df.dropna(subset=['log_g', 'log_L'])
            if not logg_l_df_for_plot.empty:
                ax_logg.plot(logg_l_df_for_plot['log_g'], logg_l_df_for_plot['log_L'], label=f'M={mass_val:.1f}', lw=1.5)
            else:
                print(f"Warning: No valid 'log_g'/'log_L' data for LogL-LogG for M={mass_val:.1f}, Z={Z_val:.4f}. Skipping LogL-LogG plot for this mass.", file=sys.stderr)

            # --- Bolometric Corrections and CMD Plot ---
            if cmd_plotting_possible:
                required_cols_bc = ['log_Teff', 'log_g', 'log_L']

                # Filter out rows with NaN values in critical columns before BC calculation
                mass_df_clean_bc = mass_df.dropna(subset=required_cols_bc).copy()

                if mass_df_clean_bc.empty:
                    print(f"Warning: No valid data points (after dropping NaNs in {', '.join(required_cols_bc)}) for M={mass_val:.1f}, Z={Z_val:.4f}. Skipping BC-based plots for this mass.", file=sys.stderr)
                    continue # Skip to the next mass_val if no clean data for BCs

                # Prepare data for MISTBolometricCorrectionGrid.interp
                teff_values = 10**mass_df_clean_bc['log_Teff'].values
                logg_values = mass_df_clean_bc['log_g'].values
                
                # Check for non-finite values in the arrays to be interpolated
                if not np.all(np.isfinite(teff_values)) or \
                   not np.all(np.isfinite(logg_values)):
                    print(f"ERROR: Input arrays for BC interpolation for M={mass_val:.1f}, Z={Z_val:.4f} contain non-finite values (Teff:{np.any(~np.isfinite(teff_values))}, logg:{np.any(~np.isfinite(logg_values))}). Skipping plots requiring BCs for this mass.", file=sys.stderr)
                    continue

                n_points = len(mass_df_clean_bc)
                feh_array = np.full(n_points, feh_val)
                av_array = np.zeros(n_points) # Assuming no extinction (Av = 0) for now

                try:
                    # Construct interp_data as a 2D array: [[Teff, logg, FeH, Av], ...]
                    # Ensure the data types are consistent, float64 is usually safe for numpy.
                    interp_data = np.array([teff_values, logg_values, feh_array, av_array], dtype=np.float64).T
                    
                    # Crucial check: Ensure interp_data has expected shape (N, 4)
                    if interp_data.shape[0] == 0 or interp_data.shape[1] != 4:
                        print(f"ERROR: Constructed 'interp_data' for M={mass_val:.1f}, Z={Z_val:.4f} has shape {interp_data.shape}. Expected (N, 4). Skipping plots requiring BCs for this mass.", file=sys.stderr)
                        continue

                    # --- DEBUG PRINTS ---
                    # These prints were already there and confirmed interp_data is fine.
                    # Kept for consistency if needed again.
                    # print(f"\nDEBUG: Before bc_grid.interp call for M={mass_val:.1f}, Z={Z_val:.4f}:")
                    # print(f"DEBUG: Type of interp_data: {type(interp_data)}")
                    # print(f"DEBUG: Shape of interp_data: {interp_data.shape}")
                    # print(f"DEBUG: Data type (dtype) of interp_data: {interp_data.dtype}")
                    # print(f"DEBUG: Is interp_data C-contiguous? {interp_data.flags['C_CONTIGUOUS']}")
                    # print(f"DEBUG: Is interp_data F-contiguous? {interp_data.flags['F_CONTIGUOUS']}")
                    # if interp_data.shape[0] > 0:
                    #     print(f"DEBUG: First 5 rows of interp_data:\n{interp_data[:min(5, interp_data.shape[0])]}")
                    # else:
                    #     print("DEBUG: interp_data is empty.")
                    # print(f"DEBUG: np.all(np.isfinite(interp_data)): {np.all(np.isfinite(interp_data))}")
                    # print(f"DEBUG: Number of points (n_points): {n_points}")
                    # print(f"DEBUG: FeH value: {feh_val}")
                    # print(f"DEBUG: Bands requested: {['U', 'B', 'V']}\n")
                    # --- END DEBUG PRINTS ---


                    # Calling bc_grid.interp for GAIA magnitudes (G, BP, RP)
                    # This is the crucial change!
                    bc_values = bc_grid.interp(interp_data, ['G', 'BP', 'RP'])

                    # Assign results to DataFrame columns
                    mass_df_clean_bc['BC_G'] = bc_values[:, 0]
                    mass_df_clean_bc['BC_BP'] = bc_values[:, 1]
                    mass_df_clean_bc['BC_RP'] = bc_values[:, 2]

                    # Calculate absolute bolometric magnitude (M_bol_sun = 4.74 for Sun)
                    M_bol = -2.5 * mass_df_clean_bc['log_L'].values + 4.74

                    # Calculate absolute Gaia magnitudes
                    mass_df_clean_bc['M_G'] = M_bol - mass_df_clean_bc['BC_G']
                    mass_df_clean_bc['M_BP'] = M_bol - mass_df_clean_bc['BC_BP']
                    mass_df_clean_bc['M_RP'] = M_bol - mass_df_clean_bc['BC_RP']

                    # Calculate Gaia color index
                    mass_df_clean_bc['G_BP_minus_G_RP'] = mass_df_clean_bc['M_BP'] - mass_df_clean_bc['M_RP']

                    # Plot CMD using the CLEANED data with calculated magnitudes and colors
                    # Filter out NaNs from the final G_BP-G_RP and M_G before plotting
                    cmd_plot_data = mass_df_clean_bc.dropna(subset=['G_BP_minus_G_RP', 'M_G'])
                    if not cmd_plot_data.empty:
                        ax_cmd.plot(cmd_plot_data['G_BP_minus_G_RP'], cmd_plot_data['M_G'], label=f'M={mass_val:.1f}', alpha=0.7, lw=1.5)
                        at_least_one_cmd_plot = True
                    else:
                        print(f"Warning: No valid 'G_BP_minus_G_RP'/'M_G' data for CMD for M={mass_val:.1f}, Z={Z_val:.4f} after BCs. Skipping CMD plot for this mass.", file=sys.stderr)

                except Exception as e:
                    print(f"ERROR: During BC interpolation for M={mass_val:.1f}, Z={Z_val:.4f}: {e}", file=sys.stderr)
                    # Print the full traceback to sys.stderr, this is crucial
                    traceback.print_exc(file=sys.stderr) 
                    print(f"ERROR: Skipping plots requiring BCs for M={mass_val:.1f}, Z={Z_val:.4f}.", file=sys.stderr)
            else:
                print(f"Info: BC grid not initialized. Skipping CMD plot for M={mass_val:.1f}, Z={Z_val:.4f}.")


        # HRD Plot Setup (unchanged)
        ax_hrd.set_xlabel(r'$\log_{10} T_{\mathrm{eff}}$')
        ax_hrd.set_ylabel(r'$\log_{10} (L/L_{\odot})$')
        ax_hrd.set_title(f'HR Diagram (Z={Z_val:.4f})')
        ax_hrd.invert_xaxis()
        ax_hrd.legend(title='Initial Mass', loc='best')
        ax_hrd.grid(True, linestyle='--', alpha=0.6)
        fig_hrd.tight_layout()
        hrd_filename = os.path.join(output_dir, f'HRD_Z{Z_val:.4f}_{output_type_label}.png')
        fig_hrd.savefig(hrd_filename, dpi=300)
        print(f"Saved HRD plot: {hrd_filename}")
        plt.close(fig_hrd)

        # CMD Plot Setup (Changed to Gaia bands)
        if cmd_plotting_possible and at_least_one_cmd_plot:
            ax_cmd.set_xlabel(r'$G_{BP} - G_{RP}$')
            ax_cmd.set_ylabel(r'$M_G$')
            ax_cmd.set_title(f'CMD ($G_{BP} - G_{RP}$ vs $M_G$) (Z={Z_val:.4f})')
            ax_cmd.invert_yaxis() # Brighter stars at top
            ax_cmd.legend(title='Initial Mass', loc='best')
            ax_cmd.grid(True, linestyle='--', alpha=0.6)
            fig_cmd.tight_layout()
            cmd_filename = os.path.join(output_dir, f'CMD_Z{Z_val:.4f}_{output_type_label}_Gaia.png') # Added _Gaia to filename
            fig_cmd.savefig(cmd_filename, dpi=300)
            print(f"Saved CMD plot: {cmd_filename}")
            plt.close(fig_cmd)
        else:
            plt.close(fig_cmd) # Close the empty CMD figure

        # LogL-LogG Plot Setup (unchanged)
        ax_logg.set_xlabel(r'$\log_{10} g$')
        ax_logg.set_ylabel(r'$\log_{10} (L/L_{\odot})$')
        ax_logg.set_title(f'$\log_{10} L - \log_{10} g$ Diagram (Z={Z_val:.4f})')
        ax_logg.invert_xaxis() # Larger logg (denser) to the right
        ax_logg.legend(title='Initial Mass', loc='best')
        ax_logg.grid(True, linestyle='--', alpha=0.6)
        fig_logg.tight_layout()
        logg_filename = os.path.join(output_dir, f'LogL_LogG_Z{Z_val:.4f}_{output_type_label}.png')
        fig_logg.savefig(logg_filename, dpi=300)
        print(f"Saved LogL-LogG plot: {logg_filename}")
        plt.close(fig_logg)

    print("Plot generation complete.")