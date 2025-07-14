Troubleshooting, Known Issues
=============================

There are '????' Characters in the Progress Bar
-----------------------------------------------


If your ``mesalab`` analysis runs show ``????`` (question marks) instead of a proper progress bar (e.g., for "Performing MESA Run Analysis"), this usually indicates an issue with your terminal environment's configuration.

``mesalab`` internally uses the `tqdm`_ library for progress bars and explicitly sets the ``TQDM_ASCII=1`` environment variable to force the use of basic ASCII characters. This is done to prevent display issues with Unicode characters. If you still see question marks despite this, please check the following:

1.  Check and Fix Your Terminal's ``locale`` Setting


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

If the problem persists after correcting your ``locale`` settings, your terminal emulator (e.g., GNOME Terminal, Konsole, XFCE Terminal on Linux; Terminal.app, iTerm2 on macOS) or the font it uses might be the culprit. Some fonts may lack basic drawing characters, or the terminal emulator might misinterpret them with certain font configurations.

* **Try a different terminal emulator (Linux and macOS):**
    `xterm` is a basic but highly stable terminal emulator.
    * On **Linux**, install it (if you don't have it already): ``sudo apt install xterm``, then open an ``xterm`` window and run your program there.
    * On **macOS**, `xterm` can also be installed (e.g., via Homebrew: `brew install xterm`), or you can try alternative terminal emulators like `iTerm2` (which is highly recommended by many macOS developers for its advanced features and robust character support).
    If the progress bar displays correctly in an alternative terminal, then you'll need to review the settings of your primary terminal emulator.

* **Check your terminal's font settings (Linux and macOS):**
    Ensure that your terminal emulator's settings (usually found under "Edit" -> "Preferences" or "Profiles" menu) use a widely supported **monospaced** font. Recommended fonts include: ``DejaVu Sans Mono``, ``Liberation Mono``, ``Hack``, ``Fira Code``. Apply the changes and **restart your terminal**.

.. _tqdm: https://github.com/tqdm/tqdm