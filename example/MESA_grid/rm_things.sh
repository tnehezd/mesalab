for d in run_*MSUN_z*; do
  if [ -d "$d" ]; then # Csak akkor folytatjuk, ha valóban egy létező mappa
    echo "Törlés a(z) $d mappában..."
    rm -rf "$d/photos" "$d/make" "$d/gyre_output"
  fi
done
echo "Kész."
