import os
import sys
import logging
import glob
from typing import Union  # ADD THIS LINE

logger = logging.getLogger(__name__)

# Change str | None to Union[str, None]
def find_mesa_star_dir_in_release(mesa_release_dir: str) -> Union[str, None]:
    """
    Attempts to auto-detect the 'star' directory within the provided MESA release directory.
    This is typically where MESA inlists and module source codes are.

    Args:
        mesa_release_dir (str): The path to the specific MESA release directory
                                (e.g., '/Users/.../mesa-r23.05.01').

    Returns:
        Union[str, None]: The full path to the 'star' directory if found, otherwise None.
    """
    if not mesa_release_dir:
        logger.debug("MESA release directory is not set, cannot auto-detect mesa_star_dir.")
        return None

    star_dir_path = os.path.join(mesa_release_dir, 'star')
    if os.path.isdir(star_dir_path):
        logger.info(f"Auto-detected mesa_star_dir: {star_dir_path}")
        return star_dir_path
    else:
        logger.critical(
            f"ERROR: 'star' directory not found directly within the specified MESA release directory ('{mesa_release_dir}'). "
            "This is required for the RSP workflow. Please ensure your MESA installation is correct."
        )
        return None

# Change str | None to Union[str, None]
def find_mesa_binary_dir(mesa_star_dir: str) -> Union[str, None]:
    """
    Attempts to find the directory containing MESA executables like 'rn' (run_star_extras)
    within the MESA 'star' directory. Checks common locations after compilation.

    Args:
        mesa_star_dir (str): The path to the MESA 'star' directory
                             (e.g., '/path/to/mesa/star').

    Returns:
        Union[str, None]: The full path to the directory containing 'rn' if found, otherwise None.
    """
    if not mesa_star_dir:
        logger.debug("mesa_star_dir is not set, cannot auto-detect mesa_binary_dir.")
        return None

    # Common location 1: star/work/
    work_dir = os.path.join(mesa_star_dir, 'work')
    if os.path.isdir(work_dir) and os.path.exists(os.path.join(work_dir, 'rn')):
        logger.info(f"Auto-detected mesa_binary_dir: {work_dir} (found 'rn' in star/work)")
        return work_dir

    # Common location 2: star/test_suite/*/work/ (for specific test cases)
    test_suite_pattern = os.path.join(mesa_star_dir, 'test_suite', '*', 'work')
    potential_work_dirs = glob.glob(test_suite_pattern)
    for p_work_dir in potential_work_dirs:
        if os.path.isdir(p_work_dir) and os.path.exists(os.path.join(p_work_dir, 'rn')):
            logger.info(f"Auto-detected mesa_binary_dir: {p_work_dir} (found 'rn' in test_suite/*/work)")
            return p_work_dir

    # Fallback: If 'rn' is directly in mesa_star_dir (less common)
    if os.path.exists(os.path.join(mesa_star_dir, 'rn')):
        logger.warning(f"Found 'rn' directly in mesa_star_dir: {mesa_star_dir}. This is unusual but will be used.")
        return mesa_star_dir

    logger.critical(
        f"ERROR: MESA executable 'rn' not found in common locations within '{mesa_star_dir}'. "
        "Please ensure MESA's 'star' module has been compiled and 'rn' exists in a 'work' directory."
    )
    return None


def set_environment_variables_for_executables(config_data: dict):  # Using dict type hint as it's addict.Dict
    """
    Sets necessary environment variables (like PATH, MESA_DIR, GYRE_DIR for subprocesses)
    and resolves specific MESA paths (mesa_star_dir, mesa_binary_dir) based on the config_data.
    Exits if critical MESA paths cannot be resolved.
    """

    is_mesa_dependent_workflow_enabled = (
        config_data.gyre_workflow.get('run_gyre_workflow', False) or
        config_data.rsp_workflow.get('run_rsp_workflow', False)
    )

    if not is_mesa_dependent_workflow_enabled:
        logger.info("No MESA-dependent workflows are enabled. Skipping path validation and environment variable setup.")
        return

    mesasdk_root_path = config_data.general_settings.get('mesasdk_root')
    mesa_dir_path = config_data.general_settings.get('mesa_dir')  # e.g., '/Users/.../mesa-r23.05.01'
    gyre_dir_path = config_data.general_settings.get('gyre_dir')

    # Set MESA_DIR for subprocesses
    if mesa_dir_path:
        os.environ['MESA_DIR'] = mesa_dir_path
        logger.debug(f"Environment variable MESA_DIR set to: {os.environ['MESA_DIR']}")
    else:
        logger.warning(
            "MESA_DIR (specific release) is not set in config, MESA_DIR environment variable will not be set for subprocesses. "
            "This may cause issues for MESA's own utilities."
        )

    # Set MESASDK_ROOT for subprocesses if SDK tools are used
    if mesasdk_root_path:
        os.environ['MESASDK_ROOT'] = mesasdk_root_path
        logger.debug(f"Environment variable MESASDK_ROOT set to: {os.environ['MESASDK_ROOT']}")
    else:
        logger.warning(
            "MESASDK_ROOT is not set in config, MESASDK_ROOT environment variable will not be set for subprocesses. "
            "Some SDK tools might not be found."
        )

    # Set GYRE_DIR for subprocesses
    if gyre_dir_path:
        os.environ['GYRE_DIR'] = gyre_dir_path
        logger.debug(f"Environment variable GYRE_DIR set to: {os.environ['GYRE_DIR']}")
    else:
        logger.warning("GYRE_DIR is not set in config, GYRE_DIR environment variable will not be set.")

    # Auto-detect mesa_star_dir if not explicitly set
    if not config_data.general_settings.get('mesa_star_dir'):
        logger.info("'mesa_star_dir' not explicitly set in config. Attempting to auto-detect from MESA_DIR.")
        detected_star_dir = find_mesa_star_dir_in_release(mesa_dir_path)
        if detected_star_dir:
            config_data.general_settings.mesa_star_dir = detected_star_dir
            logger.info(f"Auto-detected 'mesa_star_dir' to: {config_data.general_settings.mesa_star_dir}")
        else:
            sys.exit(1)  # Critical path not found, cannot proceed
    else:
        explicit_star_dir = config_data.general_settings.get('mesa_star_dir')
        if not os.path.isdir(explicit_star_dir):
            logger.critical(
                f"ERROR: Explicitly set 'mesa_star_dir' ('{explicit_star_dir}') is not a valid directory. "
                "This is required for the RSP workflow. Please correct it in config.yaml."
            )
            sys.exit(1)
        logger.info(f"Using explicitly set 'mesa_star_dir': {explicit_star_dir}")

    # Auto-detect mesa_binary_dir if not explicitly set
    if not config_data.general_settings.get('mesa_binary_dir'):
        logger.info("'mesa_binary_dir' not explicitly set in config. Attempting to auto-detect from mesa_star_dir.")
        detected_binary_dir = find_mesa_binary_dir(config_data.general_settings.mesa_star_dir)
        if detected_binary_dir:
            config_data.general_settings.mesa_binary_dir = detected_binary_dir
            logger.info(f"Auto-detected 'mesa_binary_dir' to: {config_data.general_settings.mesa_binary_dir}")
        else:
            sys.exit(1)  # Critical binary path not found, cannot proceed
    else:
        explicit_binary_dir = config_data.general_settings.get('mesa_binary_dir')
        if not os.path.isdir(explicit_binary_dir):
            logger.critical(
                f"ERROR: Explicitly set 'mesa_binary_dir' ('{explicit_binary_dir}') is not a valid directory. "
                "This is required for the RSP workflow. Please correct it in config.yaml."
            )
            sys.exit(1)
        if not os.path.exists(os.path.join(explicit_binary_dir, 'rn')):
            logger.critical(
                f"ERROR: MESA executable 'rn' not found in explicitly set 'mesa_binary_dir' ('{explicit_binary_dir}'). "
                "Please check the directory and your MESA installation."
            )
            sys.exit(1)
        logger.info(f"Using explicitly set 'mesa_binary_dir': {explicit_binary_dir}")
