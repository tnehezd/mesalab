# mesalab/config_paths.py - REVISED: Simplified MESASDK_ROOT handling

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
    Sets or updates the PATH environment variable to include the GYRE 'bin' directory
    and the MESA SDK root directory, based on the resolved paths from the config_data.

    Args:
        config_data (argparse.Namespace): The resolved configuration object containing
                                          'mesasdk_root' and 'gyre_dir'.
    """
    # Add MESASDK_ROOT to PATH if available (no 'bin' subdir assumed, no 'make' check)
    mesasdk_root_path = None
    if config_data.general_settings.mesasdk_root:
        mesasdk_root_path = config_data.general_settings.mesasdk_root # Use the path directly
        if os.path.isdir(mesasdk_root_path):
            if 'PATH' in os.environ and mesasdk_root_path not in os.environ['PATH']:
                os.environ['PATH'] = f"{mesasdk_root_path}:{os.environ['PATH']}"
                logger.debug(f"MESASDK_ROOT added to PATH: {mesasdk_root_path}")
            elif 'PATH' not in os.environ:
                os.environ['PATH'] = mesasdk_root_path
                logger.debug(f"PATH set to MESASDK_ROOT: {mesasdk_root_path}")
        else:
            logger.warning(f"MESASDK_ROOT directory not found: {mesasdk_root_path}. Check MESASDK_ROOT configuration.")
            mesasdk_root_path = None # Mark as not successfully added

    # Add GYRE_DIR/bin to PATH if available (this logic remains the same as before)
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
            logger.warning(f"GYRE bin directory not found: {gyre_bin_path}. Check GYRE_DIR configuration.")
            # Removed the print statement here, relying on logger.warning
            gyre_bin_path = None # Mark as not successfully added

    # Check if necessary executables are now in PATH and accessible
    # This check is only for GYRE, as per your request for MESASDK_ROOT simplification.
    if gyre_bin_path:
        gyre_executable_path = os.path.join(gyre_bin_path, 'gyre')
        if not os.path.exists(gyre_executable_path) or not os.access(gyre_executable_path, os.X_OK):
             logger.warning(f"GYRE executable not found or not executable at {gyre_executable_path}. "
                            "GYRE workflow might fail. Please ensure 'gyre' is installed and executable in its bin folder.")
    else:
        logger.warning("GYRE_DIR was not properly set or its bin directory is missing. GYRE workflow might fail.")

    # Removed the MESA SDK 'make' executable check as it's not needed for your use case.
    if mesasdk_root_path is None:
        logger.warning("MESASDK_ROOT was not properly set. MESA SDK dependent tasks might fail.")