# 1. LÉPÉS: Definiáld a gyökérkönyvtárat és a célmappát
SOURCE_ROOT_DIR="/home/tnd/mesagrid/mesa_blue_loop/output_mid/gyre_output"
DEST_DIR="/home/tnd/mesagrid/mesa_blue_loop/collected_radial_plots" # CSERÉLD EZT A KÍVÁNT CÉLMAUTVONALRA!

# 2. LÉPÉS: Hozd létre a célmappát, ha még nem létezik
mkdir -p "$DEST_DIR"

# 3. LÉPÉS: Keresd meg a PNG fájlokat, és másold át őket
#    A find parancs megkeresi az összes .png fájlt
#    Az -exec cp {} "$DEST_DIR/" ';' másolja a talált fájlt a célkönyvtárba.
#    A {} helyére illeszti be a find a talált fájl teljes elérési útját.
#    A \; jelzi az -exec parancs végét.
find "$SOURCE_ROOT_DIR" -type f -name "*.png" -exec cp {} "$DEST_DIR/" \;

echo "A PNG fájlok másolása befejeződött a(z) '$DEST_DIR' mappába."
echo "Összesen $(find "$DEST_DIR" -maxdepth 1 -type f -name "*.png" | wc -l) PNG fájl található a célmappában."
