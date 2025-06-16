import subprocess
import os

def run_gyre_simple_test(inlist_filename="gyre.in", gyre_executable_path=None):
    """
    Futtat egyetlen GYRE modellt egy adott inlist fájllal.

    Parameters
    ----------
    inlist_filename : str, optional
        Az inlist fájl neve a GYRE számára. Alapértelmezett: "gyre.in".
        Feltételezzük, hogy ez a fájl ugyanabban a mappában van, mint a szkript.
    gyre_executable_path : str, optional
        A GYRE futtatható fájl teljes elérési útja.
        Ha None, a szkript megpróbálja automatikusan megtalálni a PATH-ban,
        vagy a GYRE_DIR környezeti változó alapján.

    Raises
    ------
    FileNotFoundError
        Ha az inlist fájl nem található, vagy a GYRE futtatható fájl nincs meg.
    subprocess.CalledProcessError
        Ha a GYRE futása hibával (nem nulla visszatérési kóddal) fejeződik be.
    Exception
        Egyéb váratlan hibák esetén.
    """
    # 1. Ellenőrizzük, hogy az inlist fájl létezik-e
    if not os.path.exists(inlist_filename):
        raise FileNotFoundError(f"Inlist fájl nem található: '{inlist_filename}'. Kérlek, ellenőrizd a fájlnevet és a mappát.")

    # 2. A GYRE futtatható fájl elérési útjának meghatározása
    gyre_bin_path = None
    if gyre_executable_path:
        # Ha a felhasználó expliciten megadta az elérési utat
        gyre_bin_path = gyre_executable_path
    elif 'GYRE_DIR' in os.environ:
        # Ha a GYRE_DIR környezeti változó be van állítva
        gyre_dir = os.environ['GYRE_DIR'].strip() # Fontos: levágjuk a felesleges szóközöket, ha vannak

        # Két legvalószínűbb útvonal, amit a GYRE_DIR megadhat
        # 1. GYRE_DIR/bin/gyre (a leggyakoribb standalone telepítésnél)
        potential_path_bin = os.path.join(gyre_dir, 'bin', 'gyre')
        # 2. GYRE_DIR/gyre (ha maga a GYRE_DIR a bináris mappája)
        potential_path_root = os.path.join(gyre_dir, 'gyre')

        # Ellenőrizzük, hogy létezik-e, fájl-e, és futtatható-e
        if os.path.exists(potential_path_bin) and os.path.isfile(potential_path_bin) and os.access(potential_path_bin, os.X_OK):
            gyre_bin_path = potential_path_bin
        elif os.path.exists(potential_path_root) and os.path.isfile(potential_path_root) and os.access(potential_path_root, os.X_OK):
            gyre_bin_path = potential_path_root
        else:
            print(f"Figyelem: A **GYRE_DIR** ('{gyre_dir}') be van állítva, de a 'gyre' futtatható fájl nem található, vagy nem futtatható sem a '{potential_path_bin}', sem a '{potential_path_root}' útvonalon.")

    # 3. Utolsó próbálkozás: simán "gyre" a PATH-ban
    if not gyre_bin_path:
        try:
            # Csak ellenőrizzük, hogy a "gyre" parancs elérhető-e a PATH-ban
            subprocess.run(["which", "gyre"], capture_output=True, check=True, text=True)
            gyre_bin_path = "gyre" # Ezt fogjuk használni, ha a 'which' megtalálja
        except (subprocess.CalledProcessError, FileNotFoundError):
            pass # Nem található a PATH-ban, vagy nincs telepítve a 'which'

    # Ha még mindig nincs futtatható fájl, akkor hibát dobunk
    if not gyre_bin_path or (gyre_bin_path != "gyre" and not (os.path.exists(gyre_bin_path) and os.path.isfile(gyre_bin_path) and os.access(gyre_bin_path, os.X_OK))):
        raise FileNotFoundError(
            "**GYRE futtatható fájl nem található.** "
            "Kérlek, ellenőrizd, hogy:\n"
            "  1. A `gyre` benne van-e a **PATH** környezeti változódban.\n"
            "  2. A **GYRE_DIR** környezeti változó be van-e állítva, és a benne lévő útvonal helyes-e (pl. `$GYRE_DIR/bin/gyre` vagy `$GYRE_DIR/gyre`).\n"
            "  3. Vagy add meg a GYRE teljes elérési útját a `gyre_executable_path` paraméterben."
        )

    # 4. A GYRE parancs összeállítása és futtatása
    command = [gyre_bin_path, inlist_filename]

    print(f"**Próbálom futtatni a GYRE-t a következő paranccsal:** `{' '.join(command)}`")

    try:
        result = subprocess.run(command, capture_output=True, text=True, check=True)

        print(f"\n**GYRE futtatás SIKERES** a '{inlist_filename}' fájlhoz!")
        print("--- GYRE Standard Output (stdout) ---")
        print(result.stdout)
        if result.stderr:
            print("--- GYRE Standard Error (stderr) ---")
            print(result.stderr)
        print("--- Futás befejezve ---")

    except subprocess.CalledProcessError as e:
        print(f"\n**HIBA: GYRE futtatás SIKERTELEN** a '{inlist_filename}' fájlhoz, kilépési kód: **{e.returncode}**!")
        print("--- GYRE Standard Output (stdout) ---")
        print(e.stdout)
        print("--- GYRE Standard Error (stderr) ---")
        print(e.stderr)
        print("--- Hiba futás közben ---")
        raise # Dobd tovább a hibát
    except Exception as e:
        print(f"\n**VÁRATLAN HIBA történt a GYRE futtatása közben:** {e}")
        print("--- Hiba futás közben ---")
        raise # Dobd tovább a hibát


if __name__ == "__main__":
    # Az inlist fájl neve (most már alapértelmezett a 'gyre.in')
    my_inlist_file = "gyre.in"

    # Hagyjuk None-on, hogy a szkript automatikusan megpróbálja megtalálni a GYRE-t
    # a GYRE_DIR környezeti változó vagy a PATH alapján.
    # Ha ez nem működik, akkor itt adhatod meg a GYRE teljes útját, pl:
    # my_gyre_executable = "/home/tnehezd/sajat/gyre/bin/gyre"
    my_gyre_executable = None

    print(f"Indul a GYRE tesztfuttatás a '{my_inlist_file}' inlist fájllal.")

    try:
        run_gyre_simple_test(inlist_filename=my_inlist_file, gyre_executable_path=my_gyre_executable)
        print("\n**GYRE tesztfuttatás sikeresen befejeződött a Python szkriptből.**")
        print(f"Keresd a kimeneti fájlokat (summary_output.h5, detail_output_...) ugyanabban a mappában, ahol a szkriptet futtattad.")

    except FileNotFoundError as e:
        print(f"\n**[Futtatási hiba]:** {e}")
    except subprocess.CalledProcessError:
        print("\n**[Futtatási hiba]:** A GYRE futtatása sikertelen volt, lásd a fenti hibakimenetet.")
    except Exception as e:
        print(f"\n**[Váratlan hiba]:** {e}")

    print("\nTeszt futtatás befejeződött.")