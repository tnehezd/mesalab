# mesa_tools/cli.py

import argparse
import os
import pandas as pd # Mivel a plotting_routines-ba beimportálja a df-et, ide is kellhet

# Importáld az új moduláris függvényeket
from mesa_tools.data_processor import scan_mesa_runs_dirs, process_grid_analysis
from mesa_tools.profile_indexer import get_profile_numbers_for_models # Tartsd meg, ha kell
from mesa_tools.plotting_routines import generate_blue_loop_heatmap_revised

def main():
    """
    Fő függvény az argumentumok parszolására és a MESA csillagevolúciós rács elemzésének vezénylésére.
    """
    parser = argparse.ArgumentParser(description="MESA csillagevolúciós rácsok elemzése.")
    parser.add_argument("-i", "--input-dir", required=True,
                        help="Útvonal a MESA futtatási almappákat tartalmazó gyökérkönyvtárhoz.")
    parser.add_argument("-o", "--output-dir", required=True,
                        help="Útvonal az elemzési eredmények mentési könyvtárához.")
    parser.add_argument("--inlist-name", default="inlist_project",
                        help="Az inlist fájl neve a MESA futtatási könyvtárak azonosításához (alapértelmezett: inlist_project).")
    parser.add_argument("--analyze-blue-loop", action="store_true",
                        help="Engedélyezi a kék hurok elemzést és az összefoglaló/részletes fájlok generálását.")
    parser.add_argument("--generate-plots", action="store_true",
                        help="Plotok generálása minden futtatás elemzéséhez (pl. HR diagramok). Ehhez létező részletes fájlok vagy a --analyze-blue-loop opció szükséges.")
    parser.add_argument("--get-profile-numbers", action="store_true",
                        help="Meghatározza a kék hurok modellek minimális/maximális profilszámait. Ehhez létező összefoglaló vagy a --analyze-blue-loop opció szükséges.")
    parser.add_argument("--regenerate-summary", action="store_true",
                        help="Kikényszeríti a teljes kék hurok elemzés újra futtatását, még ha létezik is összefoglaló fájl. Magával vonja az --analyze-blue-loop opciót.")
    parser.add_argument("--generate-heatmap", action="store_true",
                        help="Heatmap generálása a kék hurok áthaladások számáról.")

    args = parser.parse_args()

    # Felső szintű kimeneti könyvtár létrehozása
    os.makedirs(args.output_dir, exist_ok=True)

    # Alkönyvtárak definiálása (a megfelelő függvények gondoskodnak a létrehozásukról, ha szükséges)
    plots_output_dir = os.path.join(args.output_dir, "plots") 
    if args.generate_plots or args.generate_heatmap:
        os.makedirs(plots_output_dir, exist_ok=True) 

    # --- 1. Lépés: Szkennelés és Kék Hurok Elemzés Feldolgozása ---
    # Ez a függvény mostantól kezeli a szkennelést, feldolgozást és a fő összefoglaló és részletes fájlok mentését.
    # Emellett visszaadja a feldolgozott adatokat a heatmaphez és a futtatási útvonalakat más lépésekhez.
    
    mesa_run_dirs = []
    # Csak akkor szkenneljünk, ha elemzésre van szükség, vagy újra kell generálni
    if args.analyze_blue_loop or args.regenerate_summary:
        mesa_run_dirs = scan_mesa_runs_dirs(args.input_dir, args.inlist_name)
        if not mesa_run_dirs:
            print("Nincs MESA futtatás található elemzéshez. Kilépés.")
            return

    heatmap_data_df, run_paths_data, summary_csv_path, combined_detail_output_path = \
        process_grid_analysis(mesa_run_dirs, args.output_dir, args.regenerate_summary)


    # --- 2. Lépés: Profilszámok Kinyerése (ha kérik) ---
    # Ez a szakasz a teljesség kedvéért maradt, de megjegyzést tesz, hogy a profilszámok
    # nem közvetlenül elérhetők a legacy összefoglaló formátumból.
    # Ha ez a funkció kritikus, akkor egy dedikált tárolási módszert kellene implementálni
    # a min/max modelszámokhoz a `data_processor.py`-ban.
    if args.get_profile_numbers:
        print("Profilszám meghatározás kihagyása: A modelszám részletek nem közvetlenül elérhetők a legacy összefoglaló formátumban.")
        print("Ha profilszámokra van szükséged, fontold meg az elemzési logika adaptálását, hogy külön tárolja azokat (pl. a `data_processor.py`-ban).")
        # Példa, hogyan hívnád meg, HA az adatok elérhetők lennének:
        # profile_numbers_csv_path = os.path.join(os.path.join(args.output_dir, "analysis_results"), "mesa_grid_profile_numbers.csv")
        # if heatmap_data_df is not None and run_paths_data is not None:
        #     # Feltételezve, hogy a 'min_model_number_bl' és 'max_model_number_bl' valahogy elérhetővé vált
        #     # Jelenleg nem közvetlenül hozzáférhető a legacy összefoglalóból.
        #     print("A profilszám kinyerési logikát frissíteni kell a legacy összefoglaló formátummal való kompatibilitás érdekében.")
        #     pass # Placeholder
        # else:
        #     print("Nem lehet meghatározni a profilszámokat: Elemzési adatok nem elérhetők.")

    # --- 3. Lépés: Plotok Generálása (ha kérik és az adatok elérhetők) ---
    if args.generate_plots:
        print("Plotok generálása...")
        if not run_paths_data:
            print("Plot generálás kihagyása: A futtatási útvonal információ hiányzik. Kérjük, futtassa a --analyze-blue-loop opcióval.")
        elif not os.path.exists(combined_detail_output_path):
            print(f"Figyelem: Az egyesített részletes fájl nem található itt: {combined_detail_output_path}. Plotolás kihagyása.")
        else:
            try:
                full_detail_df = pd.read_csv(combined_detail_output_path)
            except Exception as e:
                print(f"Hiba az egyesített részletes fájl olvasása közben {combined_detail_output_path}: {e}. Plotolás kihagyása.")
                return 

            unique_zs_to_plot = sorted(list(full_detail_df['initial_Z'].unique()))

            for z in unique_zs_to_plot:
                masses_for_this_z = full_detail_df[full_detail_df['initial_Z'] == z]['initial_mass'].unique()

                for mass in masses_for_this_z:
                    run_detail_df = full_detail_df[(full_detail_df['initial_Z'] == z) & (full_detail_df['initial_mass'] == mass)]
                    
                    if not run_detail_df.empty:
                        # Hívd meg itt a specifikus plotoló függvényedet (pl. plot_hr_diagram)
                        # Példa: plot_hr_diagram(run_detail_df, os.path.join(plots_output_dir, f"HR_Z{z:.4f}_M{mass:.1f}.png"))
                        pass 
                    # else: # Elnyomjuk a verbose "nincs részletes adat" figyelmeztetéseket, mivel azokat az első elemzés során kezeljük
                        # print(f"Figyelem: Nincs részletes adat a(z) {mass} tömeghez Z={z} esetén. Plot kihagyása.")
    elif args.generate_plots:
        print("Plot generálás kihagyása: A részletes adatfájlok nem lettek generálva. Kérjük, először futtassa a --analyze-blue-loop opcióval.")


    # --- 4. Lépés: Heatmap Generálása (ha kérik és az adatok elérhetők) ---
    if args.generate_heatmap:
        print("\nHeatmap generálása...")
        if summary_csv_path and os.path.exists(summary_csv_path):
            generate_blue_loop_heatmap_revised(summary_csv_path, plots_output_dir)
        else:
            print("Nem lehet heatmap-et generálni: Az összefoglaló CSV nem található vagy nem lett létrehozva. Kérjük, győződjön meg róla, hogy a --analyze-blue-loop opciót használta.")


if __name__ == "__main__":
    main()