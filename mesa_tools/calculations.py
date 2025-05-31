# mesa_tools/calculations.py
import numpy as np
import pandas as pd
from isochrones.mist.bc import MISTBolometricCorrectionGrid
import sys # Ezt importáljuk a robusztusabb hibaüzenetekhez és debug printekhez
import traceback # Ezt is importáljuk a teljes traceback kiírásához

# Initialize the bolometric correction grid ONCE when this module is loaded.
# It's good to keep all the bands you might need.
try:
    bc_grid = MISTBolometricCorrectionGrid(['J', 'H', 'K', 'G', 'BP', 'RP', 'g', 'r', 'i'])
except Exception as e:
    # A hibaüzeneteket a standard hibakimenetre írjuk
    print(f"ERROR: Error initializing MISTBolometricCorrectionGrid: {e}", file=sys.stderr)
    print("WARNING: Bolometric corrections will not be available.", file=sys.stderr)
    bc_grid = None # Set to None if initialization fails

def z_to_feh(Z):
    """Converts metallicity (Z) to [Fe/H]."""
    Z_sun = 0.0152 # Standard solar metallicity
    return np.log10(Z / Z_sun)

def calculate_gaia_magnitudes(df_detail, Z_value):
    """
    Calculates and adds bolometric corrections (BC_G, BC_BP, BC_RP)
    and Gaia absolute magnitudes (Mg, M_BP, M_RP, BP_RP) to a DataFrame.
    Assumes df_detail contains 'log_Teff', 'log_g', 'log_L' columns.
    """
    if df_detail.empty:
        print(f"WARNING: Empty DataFrame for Z={Z_value}, skipping BC calculation.", file=sys.stderr)
        return df_detail.copy() # Always return a copy to avoid SettingWithCopyWarning

    # Ensure required columns exist and handle NaNs early
    required_cols = ['log_Teff', 'log_g', 'log_L']
    # Create a clean DataFrame by dropping rows with NaN in critical columns
    df_clean = df_detail.dropna(subset=required_cols).copy()

    if df_clean.empty:
        print(f"WARNING: No valid data points after dropping NaNs in {required_cols} for Z={Z_value}. Skipping BC calculation.", file=sys.stderr)
        return df_detail.copy() # Return original or empty copy if nothing left

    feh = z_to_feh(Z_value)
    
    # Pre-allocate lists for BC values
    BC_G, BC_BP, BC_RP = [], [], []

    print(f"INFO: Starting BC calculation for Z={Z_value} with {len(df_clean)} points.")
    for i in range(len(df_clean)):
        # Get values using .iloc to ensure you're working on the cleaned DataFrame's indices
        teff_i_log = df_clean["log_Teff"].iloc[i]
        logg_i = df_clean["log_g"].iloc[i]
        av_i = 0.0 # Assuming Av=0

        # Convert log_Teff to linear Teff for MIST BCs
        teff_i_linear = 10**teff_i_log 
        
        # --- A KULCSFONTOSSÁGÚ JAVÍTÁS ÉS DEBUG: Egyetlen pont átadása 2D-s tömbként ---
        # Az interp függvény 2D-s tömböt vár (még 1 pont esetén is: [[Teff, logg, FeH, Av]])
        interp_params = np.array([[teff_i_linear, logg_i, feh, av_i]])

        # Debug printek a bemeneti paraméterekről
        print(f"  DEBUG: Row {i}, Params: Teff={teff_i_linear:.2f}, logg={logg_i:.2f}, feh={feh:.2f}, Av={av_i:.2f}")
        print(f"  DEBUG: interp_params shape: {interp_params.shape}, dtype: {interp_params.dtype}")
        print(f"  DEBUG: interp_params values: {interp_params}")

        # Ellenőrizzük a nem-véges értékeket *mielőtt* az interpolációt meghívnánk
        if not np.all(np.isfinite(interp_params)):
            print(f"  WARNING: Non-finite value encountered for row {i} (Teff={teff_i_linear:.2f}, logg={logg_i:.2f}, feh={feh:.2f}). Appending NaN values.", file=sys.stderr)
            BC_G.append(np.nan)
            BC_BP.append(np.nan)
            BC_RP.append(np.nan)
            continue # Ugrás a következő iterációra

        try:
            # Egyetlen hívással kérjük le mindhárom Gaia sávot ehhez a ponthoz.
            # Ez egy 1x3-as tömböt ad vissza: [[BC_G_val, BC_BP_val, BC_RP_val]]
            bc_values_for_point = bc_grid.interp(interp_params, ['G', 'BP', 'RP'])

            # Kinyerjük az egyes értékeket az 1x3-as tömbből
            BC_G.append(bc_values_for_point[0, 0])
            BC_BP.append(bc_values_for_point[0, 1])
            BC_RP.append(bc_values_for_point[0, 2])

        except Exception as e:
            # Részletesebb hibaüzenet a pont adataival
            print(f"  ERROR: During interpolation for row {i} (Teff={teff_i_linear:.2f}, logg={logg_i:.2f}, feh={feh:.2f}): {e}", file=sys.stderr)
            print(f"  ERROR: Offending interp_params: {interp_params}", file=sys.stderr)
            # Kiírjuk a teljes traceback-et, ez a legfontosabb infó a hiba forrásához
            traceback.print_exc(file=sys.stderr) 

            BC_G.append(np.nan)
            BC_BP.append(np.nan)
            BC_RP.append(np.nan)

    # Ensure the lengths match before assigning (important if some rows were skipped due to NaNs)
    if len(BC_G) != len(df_clean):
        print(f"WARNING: Number of calculated BCs ({len(BC_G)}) does not match clean DataFrame length ({len(df_clean)}). This might indicate skipped rows.", file=sys.stderr)
        # Create a temporary DataFrame with BCs and merge it back
        temp_df_bc = pd.DataFrame({'BC_G': BC_G, 'BC_BP': BC_BP, 'BC_RP': BC_RP}, index=df_clean.index)
        df_output = df_detail.copy() # Start with original df
        # Merge the BCs back based on index, this will fill NaNs where BCs were not calculated
        df_output = df_output.merge(temp_df_bc, left_index=True, right_index=True, how='left')
    else:
        df_output = df_clean.copy() # Start with the cleaned df
        df_output["BC_G"] = np.array(BC_G)
        df_output["BC_BP"] = np.array(BC_BP)
        df_output["BC_RP"] = np.array(BC_RP)

    # Calculate Mbol and absolute magnitudes only for rows where log_L is not NaN
    Mbol = -2.5 * df_output["log_L"].values + 4.74 # M_bol_sun = 4.74

    # Calculate magnitudes, ensure BC_G, BC_BP, BC_RP exist
    # These operations will propagate NaNs if BC values are NaN
    df_output["Mg"] = Mbol - df_output["BC_G"]
    df_output["M_BP"] = Mbol - df_output["BC_BP"]
    df_output["M_RP"] = Mbol - df_output["BC_RP"]
    df_output["BP_RP"] = df_output["M_BP"] - df_output["M_RP"]
    df_output["Z"] = Z_value # Ensure Z value is present in the DataFrame

    print(f"INFO: BC calculation finished for Z={Z_value}.")
    return df_output