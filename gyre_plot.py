import pandas as pd
import os
import re

# --- Konfiguráció ---
# Ezeknek meg kell egyezniük a mysett.yaml és a gyre_config.in beállításaival
output_base_dir = "/home/tnd/mesagrid/mesa_blue_loop/output"
gyre_results_base_dir = "/home/tnd/mesagrid/mesa_blue_loop/mesalab_results" # Ahol a GYRE kimenetek vannak
mesa_history_dir = "/home/tnd/mesa-r23.05.1/STRANGE/nad_convos_mid" # Ahol a MESA futtatásaid LOGS mappái vannak
inlist_name = "inlist_project" # Az inlist_name a mysett.yaml-ból

# --- Funkció a MESA run paramétereinek kinyerésére a mappanévekből ---
def parse_mesa_run_name(run_name):
    # Példa: 'run_nad_convos_mid_9.5MSUN_z0.0080'
    match_mass = re.search(r'(\d+\.?\d*)MSUN', run_name)
    match_z = re.search(r'z(\d+\.?\d*)', run_name)
    mass = float(match_mass.group(1)) if match_mass else None
    z = float(match_z.group(1)) if match_z else None
    return mass, z

# --- Adatgyűjtés ---
all_gyre_modes = []

# Végigjárjuk a GYRE eredménymappákat
for run_dir in os.listdir(gyre_results_base_dir):
    run_path = os.path.join(gyre_results_base_dir, run_dir)
    if not os.path.isdir(run_path):
        continue

    mass, z = parse_mesa_run_name(run_dir)
    if mass is None or z is None:
        print(f"Figyelem: Nem sikerült kinyerni a tömeget vagy Z-t a mappnévből: {run_dir}")
        continue

    # Keresd meg a megfelelő MESA LOGS mappát
    # A MESA LOGS mappa neve általában megegyezik a run_dir nevével
    mesa_logs_path = os.path.join(mesa_history_dir, run_dir, "LOGS")
    if not os.path.exists(mesa_logs_path):
        print(f"Hiba: Nem található MESA LOGS mappa: {mesa_logs_path}")
        continue

    history_file_path = os.path.join(mesa_logs_path, "history.data")
    if not os.path.exists(history_file_path):
        print(f"Hiba: Nem található history.data fájl: {history_file_path}")
        continue

    # Olvassuk be a MESA history.data fájlt (csak a szükséges oszlopokat)
    # A '  #' sorok kommentek a MESA history fájl elején
    mesa_history_df = pd.read_csv(history_file_path,
                                  skiprows=lambda x: x < 5 or x > 5 and ' ' not in pd.read_csv(history_file_path, nrows=x).iloc[x-1, 0], # Kihagyja a header kommenteket
                                  delim_whitespace=True, # Space-ek a elválasztók
                                  comment='#') # Kihagyja a # kezdetű sorokat
    
    # MESA history oszlopok ellenőrzése
    required_history_cols = ['model_number', 'log_L', 'log_Teff', 'star_mass']
    if not all(col in mesa_history_df.columns for col in required_history_cols):
        print(f"Figyelem: A history.data fájlban hiányzó oszlopok: {history_file_path}")
        print(f"Elérhető oszlopok: {mesa_history_df.columns.tolist()}")
        continue

    for profile_dir in os.listdir(run_path):
        profile_path = os.path.join(run_path, profile_dir)
        if not os.path.isdir(profile_path):
            continue

        # GYRE summary.h5 fájl beolvasása
        gyre_summary_path = os.path.join(profile_path, "summary.h5")
        if not os.path.exists(gyre_summary_path):
            # print(f"Figyelem: Nem található summary.h5 fájl: {gyre_summary_path}")
            continue

        try:
            gyre_df = pd.read_hdf(gyre_summary_path, key='modes') # A 'modes' kulcsot kell használni
            if gyre_df.empty:
                # print(f"Nincsenek módusok a {gyre_summary_path} fájlban, kihagyva.")
                continue
        except Exception as e:
            print(f"Hiba a {gyre_summary_path} beolvasásakor: {e}")
            continue
        
        # A profileXXX mappából kinyerjük a modellszámot
        # Példa: profile00001 -> model_number = 1
        model_num_match = re.search(r'profile(\d+)', profile_dir)
        if not model_num_match:
            print(f"Figyelem: Nem sikerült kinyerni a modellszámot a profil mappnévből: {profile_dir}")
            continue
        
        profile_model_number = int(model_num_match.group(1))

        # Keresd meg a megfelelő MESA history bejegyzést
        mesa_row = mesa_history_df[mesa_history_df['model_number'] == profile_model_number]
        if mesa_row.empty:
            print(f"Figyelem: Nem található MESA history bejegyzés a {profile_model_number} modellhez a {run_dir} futtatásban.")
            continue
        
        # Add hozzá a MESA paramétereket minden GYRE módus sorhoz
        for _, mode_row in gyre_df.iterrows():
            new_row = mode_row.copy()
            new_row['mass'] = mass
            new_row['Z'] = z
            new_row['model_number'] = profile_model_number
            new_row['log_L'] = mesa_row['log_L'].iloc[0]
            new_row['log_Teff'] = mesa_row['log_Teff'].iloc[0]
            new_row['star_mass_mesa'] = mesa_row['star_mass'].iloc[0] # A MESA history-ból származó tömeg
            
            all_gyre_modes.append(new_row)

if all_gyre_modes:
    final_df = pd.DataFrame(all_gyre_modes)
    output_csv_path = os.path.join(output_base_dir, "gyre_modes_with_mesa_params.csv")
    final_df.to_csv(output_csv_path, index=False)
    print(f"\nSikeresen összegyűjtöttük az adatokat: {output_csv_path}")
    print(f"Összesen {len(final_df)} módus került feldolgozásra.")
else:
    print("\nNem találtunk feldolgozható GYRE módust.")