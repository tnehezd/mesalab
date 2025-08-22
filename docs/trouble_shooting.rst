Troubleshooting, Known Issues
=============================

There are '????' Characters in the Progress Bar
-----------------------------------------------

If your ``mesalab`` analysis runs show ``????`` (question marks) instead of a proper progress bar (e.g., for "Performing MESA Run Analysis"), this usually indicates an issue with your terminal environment's configuration.

``mesalab`` internally uses the `tqdm`_ library for progress bars and explicitly sets the ``TQDM_ASCII=1`` environment variable to force the use of basic ASCII characters. This is done to prevent display issues with Unicode characters. If you still see question marks despite this, please check the following:

1.  Check and Fix Your Terminal's ``locale`` Setting
....................................................

This is the most common cause of the problem. If your terminal's `locale` setting is `C` (or another non-UTF-8 encoding), it can cause issues with the terminal's basic character handling and the proper rendering of the progress bar, even with ASCII characters.

* **Check your current ``locale`` setting:**
  Open a terminal and on both **Linux** and **macOS**, run the following command:

  .. code-block:: bash

    locale

  If the output includes lines like ``LC_ALL=C`` or ``LANG=C``, this is likely the root of your problem.

* **Fix on Linux:**
  1.  Open your user's shell configuration file (``.bashrc`` if you use Bash, or ``.zshrc`` if you use Zsh) with a text editor (e.g., `nano`, `vim`):

  .. code-block:: bash

    nano ~/.bashrc
    # OR
    nano ~/.zshrc

  2.  Add the following lines to the end of the file, or ensure they are present:

  .. code-block:: bash

    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8

  (You can also use a locale specific to your language like ``hu_HU.UTF-8`` if applicable, but ``en_US.UTF-8`` is generally the most compatible and recommended for development environments.)

  3.  Save the file and exit the editor.
  4.  **Crucially:** Close your current terminal window and open a new one. The changes only take effect in a new shell session.
  5.  In the new terminal, run the ``locale`` command again to confirm that ``UTF-8`` encoding is now shown.
  6.  Try running the ``mesalab`` program again.

* **Fix on macOS:**
  On macOS, the system's language and region settings can sometimes override or conflict with shell-level locale exports, or the Terminal app itself might have a setting that interferes.

  1.  **First, try the shell configuration method (same as Linux):**
      Open your shell configuration file (``.bashrc`` or ``.zshrc``) and add/ensure the following lines:

  .. code-block:: bash

    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8

  Save the file, close your terminal, and open a new one to apply changes.

  2.  **Check Terminal.app settings (if using the default macOS Terminal):**
      Sometimes, the "Set locale environment variables on startup" option in Terminal.app preferences can cause issues.

    * Open **Terminal.app**.
    * Go to **Terminal > Settings** (or **Preferences** on older macOS versions).
    * Select the **Profiles** tab.
    * Choose your active profile (usually "Basic" or "Pro").
    * Click the **Advanced** tab.
    * In the "International" section, **uncheck** "Set locale environment variables on startup".
    * Also, ensure "Text encoding" is set to **Unicode (UTF-8)**.
    * Close and re-open Terminal.app.

  After attempting these fixes, run the ``mesalab`` program again.

2.  Check Your Terminal Emulator and Font
..........................................

If the problem persists after correcting your ``locale`` settings, your terminal emulator (e.g., GNOME Terminal, Konsole, XFCE Terminal on Linux; Terminal.app, iTerm2 on macOS) or the font it uses might be the culprit. Some fonts may lack basic drawing characters, or the terminal emulator might misinterpret them with certain font configurations.

* **Try a different terminal emulator (Linux and macOS):**
    `xterm` is a basic but highly stable terminal emulator.
    * On **Linux**, install it (if you don't have it already): ``sudo apt install xterm``, then open an ``xterm`` window and run your program there.
    * On **macOS**, `xterm` can also be installed (e.g., via Homebrew: `brew install xterm`), or you can try alternative terminal emulators like `iTerm2` (which is highly recommended by many macOS developers for its advanced features and robust character support).
    If the progress bar displays correctly in an alternative terminal, then you'll need to review the settings of your primary terminal emulator.

* **Check your terminal's font settings (Linux and macOS):**
    Ensure that your terminal emulator's settings (usually found under "Edit" -> "Preferences" or "Profiles" menu) use a widely supported **monospaced** font. Recommended fonts include: ``DejaVu Sans Mono``, ``Liberation Mono``, ``Hack``, ``Fira Code``. Apply the changes and **restart your terminal**.

.. _tqdm: https://github.com/tqdm/tqdm

----

Missing Optional Dependencies (Holoviews, PyMultiNest)
------------------------------------------------------

**Problem:**
You see warnings like:

* ``WARNING: Holoviews (and/or Bokeh) not imported. Some advanced visualizations will not be available.``
* ``WARNING: PyMultiNest not imported. MultiNest fits will not work.``

**Description:**
These warnings indicate that `mesalab` attempted to load certain **optional dependencies** (Holoviews for advanced interactive plotting, PyMultiNest for Bayesian inference fits) but found them missing in your Python environment. `mesalab` is designed to function without these, but features relying on them will be unavailable.

The `mesalab` CLI is designed to show these warnings only **once** at startup. If you see them repeatedly during the workflow, it might indicate an internal code structure issue where modules are re-attempting imports unconditionally (this should be addressed in `mesalab`'s internal code, as per developer notes). However, for the user, the core issue is the missing package.

**Solution:**
If you need the functionality provided by these packages, you must install them into your Python environment.

* **To install Holoviews (and its recommended Bokeh backend):**
  Open your terminal or command prompt and run:

  .. code-block:: bash

    pip install holoviews bokeh

  This will download and install Holoviews and Bokeh, which is a common rendering backend for Holoviews plots.

* **To install PyMultiNest:**
  Open your terminal or command prompt and run:

  .. code-block:: bash

    pip install pymultinest

**Verification:**
After running the appropriate `pip install` command(s), run your `mesalab` program again. If the installation was successful, the corresponding warnings should no longer appear at startup.

**Important Note on Python Environments:**
If you use a **virtual environment** (like `venv` or `conda`), ensure you activate that environment *before* running the `pip install` commands. This guarantees the packages are installed into the correct environment that `mesalab` uses.

----


FutureWarning: "isochrones" and "pandas"
----------------------------------------

**Problem:**
You may see a FutureWarning when running the bolometric calculation workflow. This step uses the `isochrones` package. The warning message may appear as:

  * ``/path/to/your/pyhton/site-packages/isochrones/bc.py:82: FutureWarning: The 'delim_whitespace' keyword in pd.read_csv is deprecated and will be removed in a future version. Use `sep='\s+'` instead``


This warning indicates that the `isochrones` package is using a function or syntax from the `pandas` library that is now considered deprecated. While the code still works for now, this warning is a signal that the function will be removed in a future `pandas` version, which could cause your code to break.

**Cause:**
The `requirements.txt` file xspecify a flexible version range for `pandas` (e.g., pandas>=1.0.0). This allows pip to install a newer available `pandas` version, which has deprecated a function still used by an older `isochrones` version.

**Solution:**
The most effective way to resolve this is to **upgrade the** `isochrones` **package**. 

To fix the issue, run the following command in your terminal:

  .. code-block:: bash

    pip install --upgrade isochrones

After the upgrade, the FutureWarning should no longer appear.

----

.. _trouble_shooting_gyre:

GYRE Workflow Skipped or Failed
-------------------------------

**Problem:**
You see messages indicating the GYRE workflow was skipped or encountered an error, even if the overall `mesalab` run ends with "mesalab Workflow Completed with Errors/Skipped Steps!" instead of "Finished Successfully!".

**Example messages:**

* ``WARNING: GYRE core modules not imported due to: <Error_Details>. GYRE workflow will be skipped.``
* ``GYRE workflow is enabled in configuration, but GYRE modules failed to load at startup. Skipping GYRE workflow.``
* ``GYRE Workflow Skipped: Required input CSV not found.``
* ``GYRE Workflow Encountered an Error.``

**Description:**
The GYRE workflow is critical for pulsation analysis. `mesalab` will only attempt to run it if `gyre_workflow.run_gyre_workflow` is set to `true` in your configuration. Even then, it can be skipped or fail due to several reasons:

1.  **GYRE Python Modules Not Loaded:** `mesalab`'s internal Python modules for GYRE integration (in `mesalab/gyretools/`) might not have loaded correctly at startup. This could be due to missing Python dependencies required by those modules.
2.  **Missing Input CSV for Filtered Profiles:** If `gyre_workflow.run_mode` is set to `FILTERED_PROFILES`, `mesalab` expects a specific CSV file (named by `gyre_workflow.filtered_profiles_csv_name`) to be generated by the MESA analysis, containing the profiles for GYRE to analyze. If this file is missing or empty, GYRE will be skipped.
3.  **GYRE Executable (`gyre`) Issues:** The core GYRE executables might not be installed, not be in your system's `PATH`, or encounter a runtime error.

**Solutions:**

1.  **If "GYRE core modules not imported" (Python side issue):**
    This indicates a problem with the Python dependencies required by `mesalab`'s own GYRE integration.

    * **Check `mesalab`'s installation:** Ensure your `mesalab` installation is complete and all its direct dependencies are met.
    * **Consult `mesalab`'s `requirements.txt`:** Look for all the Python dependencies listed there and install any missing ones:
        
      .. code-block:: bash

        $pip install <missing_package_name>

2.  **If "GYRE Workflow Skipped: Required input CSV not found.":**
    This means `mesalab` couldn't find the input profiles for GYRE when in `FILTERED_PROFILES` mode.

    * **Verify MESA Analysis Success:** Ensure the preceding MESA analysis workflow completed successfully and generated the necessary output files. The CSV is typically saved in the `analysis_results` subdirectory within your `output_dir`.
    * **Check `filtered_profiles_csv_name`:** Confirm that the `gyre_workflow.filtered_profiles_csv_name` in your `mesalab` config matches the name of the CSV file expected to be generated.
    * **Check for empty data:** It's possible the MESA analysis ran, but no profiles met the filtering criteria for GYRE. Review your MESA analysis settings and expected output.

3.  **If "GYRE Workflow Encountered an Error." (likely `gyre` executable issues):**
    This usually points to problems with the external GYRE software itself or its accessibility.

    * **Install GYRE:**
      The `mesalab` requires the GYRE pulsation code to be installed separately. You can find comprehensive installation instructions for GYRE (including compilation steps) in its official documentation. For GYRE **v7.0**, refer to:
      `https://gyre.readthedocs.io/en/v7.0/ref-guide/installation.html`
      Follow these instructions to compile and install GYRE on your system.

    * **Set the `GYRE_DIR` Environment Variable:**
      After successfully installing GYRE, you **must** set the `GYRE_DIR` environment variable to point to the top-level directory of your GYRE installation (the directory containing `bin`, `data`, `doc`, etc.). This helps `mesalab` (and other tools) locate the GYRE executables and data files.

      * **On Linux/macOS (bash/zsh):**
        Add the following line to your `~/.bashrc`, `~/.zshrc`, or `~/.profile` file:
      
      .. code-block:: bash

        export GYRE_DIR="/path/to/your/gyre_installation"
        export PATH="$GYRE_DIR/bin:$PATH" # Add GYRE executables to your PATH

      Replace `/path/to/your/gyre_installation` with the actual path to your GYRE directory. After modifying the file, open a **new terminal** or run `source ~/.bashrc` (or the appropriate file) to apply the changes.


    * **Check `PATH` Environment Variable (manual verification):**
      Even after setting `GYRE_DIR`, it's crucial that `gyre` found by your shell.
      * **Linux/macOS:** In your terminal, run `echo $GYRE_DIR`. It should return the path to your GYRE installation.

    * **Explicit Executable Paths (in `mesalab` config - Optional):**
      While setting `GYRE_DIR` and `PATH` is the recommended way, you can also explicitly tell `mesalab` where to find the GYRE executables in your configuration YAML:
    
      .. code-block:: console

        gyre_workflow:
            run_gyre_workflow: true
            # ...
            gyre_executable: /path/to/your/gyre_installation/bin/gyre

      Replace `/path/to/your/gyre_installation/bin/gyre` with the actual full paths.


4.  **Final Verification:**
    After attempting these solutions, run the `mesalab` program again. The GYRE workflow should now execute successfully if all dependencies and paths are correctly configured.



.. _trouble_shooting_mesa:

RSP Workflow Skipped or Failed
------------------------------

**Problem:**
You see messages indicating the RSP workflow was skipped or encountered an error, even if the overall `mesalab` run ends with "mesalab Workflow Completed with Errors/Skipped Steps!" instead of "Finished Successfully!".

**Example messages:**

    * ``WARNING: RSP core modules not imported due to: <Error_Details>. RSP workflow will be skipped.``
    * ``RSP workflow is enabled in configuration, but RSP modules failed to load at startup. Skipping RSP workflow.``
    * ``RSP Workflow Encountered an Error.``
    * ``CRITICAL: ERROR: Configured 'mesa_binary_dir' ('/path/to/mesa/star/work') is invalid or 'star' not found within it. Cannot run RSP workflow.``

**Description:**
The RSP workflow is critical for pulsation analysis. `mesalab` will only attempt to run it if `rsp_workflow.run_rsp_workflow` is set to `true` in your configuration. Even then, it can be skipped or fail due to some reasons:

1.  **RSP Python Modules Not Loaded:** `mesalab`'s internal Python modules for RSP integration (in `mesalab/rsptools/`) might not have loaded correctly at startup. This could be due to missing Python dependencies required by those modules.
2. **RSP Executable (`rn`) Issues:** The core MESA executables might not be installed, not be in your system's `PATH`, or encounter a runtime error.

**Solutions:**

1.  **If "RSP core modules not imported" (Python side issue):**
    This indicates a problem with the Python dependencies required by `mesalab`'s own RSP integration.
    * **Check `mesalab`'s installation:** Ensure your `mesalab` installation is complete and all its direct dependencies are met.
    * **Consult `mesalab`'s `requirements.txt`:** Look for all the Python dependencies listed there, and install any missing ones:
        
      .. code-block:: bash

        $pip install <missing_package_name>

2.  **If "RSP Workflow Encountered an Error." (`rn` executable issues):**
    This usually points to problems with the external MESA software itself or its accessibility.

    * **Install MESA:**
      The `mesalab` requires MESA to be installed separately. You can find comprehensive installation instructions for MESA (including compilation steps) in its official documentation. For MESA version **23.05.1**, refer to:
      `https://docs.mesastar.org/en/23.05.1/`
      Follow these instructions to compile and install MESA on your system.

    * **Set the `MESA_DIR` Environment Variable:**
      After successfully installing MESA, you **must** set the `MESA_DIR` environment variable to point to the top-level directory of your MESA installation. This helps `mesalab` (and other tools) locate the MESA executables and data files.

      **On Linux/macOS (bash/zsh):**
      add the following line to your `~/.bashrc`, `~/.zshrc`, or `~/.profile` file:

      .. code-block:: bash

        export MESA_DIR="/path/to/your/mesa_installation"
        export PATH="$MESA_DIR/bin:$PATH" # Add MESA installation directory to your PATH

      Replace `/path/to/your/mesa_installation` with the actual path to your MESA directory. After modifying the file, open a **new terminal** or run `source ~/.bashrc` (or the appropriate file) to apply the changes.


    * **Check `PATH` Environment Variable (manual verification):**
      Even after setting `MESA_DIR`, it's crucial that the directory is found by your shell.
      * **Linux/macOS:** In your terminal, run `echo $MESA_DIR`. It should return the path to your installed MESA.

    * **Explicit Executable Paths (in `mesalab` config - Optional):**
      While setting `MESA_DIR` and `PATH` is the recommended way, you can also explicitly tell `mesalab` where to find the RSP executables in your configuration YAML:

      .. code-block:: console

        rsp_workflow:
            run_rsp_workflow: true
            # ...
            mesa_binary_dir: /path/to/your/mesa_installation/star/work

      Replace `/path/to/your/mesa_installation/star/work` with the actual full paths.

    * **Missing or Uncompiled MESA Binaries**:
      
      This CRITICAL error message indicates that the necessary MESA executables were not found in the specified mesa_binary_dir. You need to enter the star/work folder and compile the executables.
        
      * **Solution:** 
      
      Navigate to the star/work directory of your MESA installation and run the compilation commands:

      .. code-block:: bash

        cd /path/to/your/mesa_installation/star/work
        ./clean
        ./mk

3.  **Final Verification:**
    
    After attempting these solutions, run the `mesalab` program again. The RSP workflow should now execute successfully if all dependencies and paths are correctly configured.

    

.. _tqdm: https://github.com/tqdm/tqdm


