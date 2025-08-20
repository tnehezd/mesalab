# mesalab/rsptools/rsp_inlist_generator.py

import pandas as pd
import numpy as np
import os
import logging
import sys
import re # Import the regular expression module

logger = logging.getLogger(__name__)

def generate_mesa_rsp_inlists(
    detail_df: pd.DataFrame,
    mesa_output_base_dir: str,
    rsp_inlist_template_path: str,
    rsp_output_subdir: str
) -> list[str]:
    """
    Generates MESA inlist files for RSP simulations, each loading a specific
    MESA profile from the provided detail_df.

    Args:
        detail_df (pd.DataFrame): DataFrame containing 'initial_mass', 'initial_Z',
                                  'model_number', 'log_Teff', 'log_L', 'run_dir_path',
                                  and 'initial_Y'.
        mesa_output_base_dir (str): The root directory where MESA run output folders are.
                                    (e.g., 'output/mesa_runs')
        rsp_inlist_template_path (str): Path to the MESA RSP inlist template file.
        rsp_output_subdir (str): Base directory for generated MESA RSP runs.
    
    Returns:
        list[str]: A list of paths to the generated MESA RSP inlist files.
    """
    if detail_df.empty:
        logger.warning("Input detail_df is empty. No MESA RSP inlists will be generated.")
        return []

    if not os.path.exists(rsp_inlist_template_path):
        logger.error(f"MESA RSP inlist template not found at: {rsp_inlist_template_path}")
        return []

    os.makedirs(rsp_output_subdir, exist_ok=True)
    
    generated_inlists = []

    try:
        with open(rsp_inlist_template_path, 'r') as f:
            template_content = f.read()
    except IOError as e:
        logger.error(f"Failed to read RSP inlist template from {rsp_inlist_template_path}: {e}")
        return []

    logger.info(f"Starting generation of MESA RSP inlists for {len(detail_df)} profiles.")

    for _, row in detail_df.iterrows():
        try:
            mass = row['initial_mass']
            Z = row['initial_Z']
            model_number = int(row['model_number'])
            log_Teff = row['log_Teff']
            log_L = row['log_L']
            
            Teff = round(10**log_Teff, 0)
            L = round(10**log_L, 0)

            initial_Y = row.get('initial_Y', np.nan)
            
            if pd.isna(initial_Y):
                X_val = 0.730
                logger.warning(f"Initial_Y missing for M={mass:.1f}, Z={Z:.4f}, model={model_number}. Using default RSP_X={X_val:.3f}d0.")
            else:
                X_val = 1.0 - initial_Y - Z
                if X_val < 0 or X_val > 1:
                    logger.warning(f"Calculated X ({X_val:.3f}) is out of [0, 1] range. Using default RSP_X=0.730d0.")
                    X_val = 0.730

            rsp_max_model_num = 1200
            
            # Construct the new structured output path
            run_folder_name = f"run_{mass:.1f}MSUN_z{Z:.4f}"
            model_folder_name = f"model{model_number:04d}"
            
            this_rsp_run_root_dir = os.path.join(rsp_output_subdir, run_folder_name, model_folder_name)
            os.makedirs(this_rsp_run_root_dir, exist_ok=True)
            
            save_model_filename = f'rsp_final_M{mass:.1f}Z{Z:.4f}Mod{model_number}.mod'

            # Define a dictionary of patterns and their replacement strings
            replacements = {
                r"RSP_mass\s*=\s*.*": f'RSP_mass = {mass:.1f}d0',
                r"RSP_Teff\s*=\s*.*": f'RSP_Teff = {int(Teff):d}',
                r"RSP_L\s*=\s*.*": f'RSP_L = {int(L):d}',
                r"RSP_Z\s*=\s*.*": f'RSP_Z = {Z:.4f}d0',
                r"Zbase\s*=\s*.*": f'Zbase = {Z:.4f}d0',
                r"save_model_filename\s*=\s*'[^']*'": f"save_model_filename = '{save_model_filename}'",
                r"max_model_number\s*=\s*.*": f"max_model_number = {rsp_max_model_num}",
                r"RSP_X\s*=\s*.*": f'RSP_X = {X_val:.3f}d0'
            }
            
            # Apply all replacements using a loop
            current_inlist_content = template_content
            for pattern, new_string in replacements.items():
                current_inlist_content = re.sub(pattern, new_string, current_inlist_content, flags=re.IGNORECASE)

            inlist_path = os.path.join(this_rsp_run_root_dir, "inlist_rsp")
            
            with open(inlist_path, 'w') as f:
                f.write(current_inlist_content)
                
            generated_inlists.append(inlist_path)
            logger.info(f"Generated MESA RSP inlist: {inlist_path}")

        except KeyError as ke:
            logger.error(f"Missing expected column in detail_df for RSP inlist generation: {ke}. Skipping this profile.")
            continue
        except Exception as e:
            mass_info = f"M={row.get('initial_mass', 'N/A')}, Z={row.get('initial_Z', 'N/A')}, model={row.get('model_number', 'N/A')}"
            logger.error(f"Error generating RSP inlist for {mass_info}: {e}", exc_info=True)
            continue

    if not generated_inlists:
        logger.warning("No RSP inlist files were generated. Check detail_df content and template.")

    return generated_inlists