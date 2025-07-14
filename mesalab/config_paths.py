# mesalab/config_paths.py

import os
import logging

logger = logging.getLogger(__name__)

# --- Global Paths and Default Settings (less about fixed paths, more about utilities) ---

# Default output base directory (can be overridden by CLI or YAML)
DEFAULT_OUTPUT_BASE_DIR = './mesalab_results'

# Directory where GYRE inlist template files are located (relative to project root)
GYRE_INLIST_TEMPLATE_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'config')
# Assuming your GYRE inlist templates are in the 'config' folder

def set_environment_variables_for_executables(config_data):
    """
    Sets or updates the PATH environment variable to include the GYRE and MESA SDK
    bin directories, based on the resolved paths from the config_data.

    Args:
        config_data (addict.Dict): The resolved configuration object containing
                                   'mesasdk_root' and 'gyre_dir'.
    """
    # Add MESASDK_ROOT/bin to PATH if available
    mesasdk_bin_path = None
    if config_data.general_settings.mesasdk_root:
        mesasdk_bin_path = os.path.join(config_data.general_settings.mesasdk_root, 'bin')
        if os.path.isdir(mesasdk_bin_path):
            if 'PATH' in os.environ and mesasdk_bin_path not in os.environ['PATH']:
                os.environ['PATH'] = f"{mesasdk_bin_path}:{os.environ['PATH']}"
                logger.debug(f"MESASDK bin added to PATH: {mesasdk_bin_path}")
            elif 'PATH' not in os.environ:
                os.environ['PATH'] = mesasdk_bin_path
                logger.debug(f"PATH set to MESASDK bin: {mesasdk_bin_path}")
        else:
            logger.warning(f"MESASDK bin directory not found: {mesasdk_bin_path}. Check MESASDK_ROOT.")
            mesasdk_bin_path = None # Mark as not successfully added

    # Add GYRE_DIR/bin to PATH if available
    gyre_bin_path = None
    if config_data.general_settings.gyre_dir:
        gyre_bin_path = os.path.join(config_data.general_settings.gyre_dir, 'bin')
        if os.path.isdir(gyre_bin_path):
            if 'PATH' in os.environ and gyre_bin_path not in os.environ['PATH']:
                os.environ['PATH'] = f"{gyre_bin_path}:{os.environ['PATH']}"
                logger.debug(f"GYRE bin added to PATH: {gyre_bin_path}")
            elif 'PATH' not in os.environ:
                os.environ['PATH'] = gyre_bin_path
                logger.debug(f"PATH set to GYRE bin: {gyre_bin_path}")
        else:
            logger.warning(f"GYRE bin directory not found: {gyre_bin_path}. Check GYRE_DIR.")
            gyre_bin_path = None # Mark as not successfully added

    # Check if necessary executables are now in PATH and accessible
    # You might want to add more robust checks here (e.g., using shutil.which)
    if gyre_bin_path:
        gyre_executable_path = os.path.join(gyre_bin_path, 'gyre')
        if not os.path.exists(gyre_executable_path) or not os.access(gyre_executable_path, os.X_OK):
             logger.warning(f"GYRE executable not found or not executable at {gyre_executable_path}. "
                            "GYRE workflow might fail. Please ensure 'gyre' is installed and executable in its bin folder.")
    else:
        logger.warning("GYRE_DIR was not properly set or its bin directory is missing. GYRE workflow might fail.")

    if mesasdk_bin_path:
        make_executable_path = os.path.join(mesasdk_bin_path, 'make') # Example for MESASDK check
        if not os.path.exists(make_executable_path) or not os.access(make_executable_path, os.X_OK):
            logger.warning(f"MESA SDK 'make' executable not found or not executable at {make_executable_path}. "
                           "MESA related tasks (if implemented) might fail. Please ensure MESA SDK is correctly installed.")
    else:
        logger.warning("MESASDK_ROOT was not properly set or its bin directory is missing. MESA SDK dependent tasks might fail.")
