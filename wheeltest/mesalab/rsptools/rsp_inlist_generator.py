# mesalab/rsptools/rsp_inlist_generator.py

import pandas as pd
import numpy as np
import os
import logging
import sys

logger = logging.getLogger(__name__)

def generate_mesa_rsp_inlists(
    detail_df: pd.DataFrame,
    mesa_output_base_dir: str, # Base directory for MESA outputs (where run_dirs are located) - *not directly used if create_RSP_model is true*
    rsp_inlist_template_path: str,
    rsp_output_subdir: str # Where the generated MESA RSP inlists and their LOGS/plots will go
) -> list[str]:
    """
    Generates MESA inlist files for RSP simulations, each loading a specific
    MESA profile from the provided detail_df.

    Args:
        detail_df (pd.DataFrame): DataFrame containing 'initial_mass', 'initial_Z',
                                  'model_number', 'log_Teff', 'log_L', 'run_dir_path',
                                  and 'initial_Y'.
        mesa_output_base_dir (str): The root directory where MESA run output folders are.
                                    (e.g., 'output/mesa_runs') - This parameter is not directly
                                    used if RSP is creating a model from scratch based on parameters.
        rsp_inlist_template_path (str): Path to the MESA RSP inlist template file.
        rsp_output_subdir (str): Base directory for generated MESA RSP runs.
    
    Returns:
        list[str]: A list of paths to the generated MESA RSP inlist files.
        
    Example:
        >>> import pandas as pd
        >>> import os
        >>> from mesalab.rsptools.rsp_inlist_generator import generate_mesa_rsp_inlists
        >>>
        >>> # 1. Create a mock DataFrame with the required data.
        >>> data = {
        ...     'initial_mass': [1.0, 1.2],
        ...     'initial_Z': [0.014, 0.003],
        ...     'model_number': [500, 750],
        ...     'log_Teff': [3.75, 3.82],
        ...     'log_L': [1.5, 2.1],
        ...     'initial_Y': [0.27, 0.28],
        ...     'run_dir_path': ['/path/to/run1', '/path/to/run2']
        ... }
        >>> detail_df = pd.DataFrame(data)
        >>>
        >>> # 2. Define your paths. Ensure these directories and files exist on your system.
        >>> mesa_out_dir = '/path/to/your/mesa_output'
        >>> template_path = '/path/to/your/inlist_rsp_template'
        >>> rsp_out_dir = './rsp_inlists'
        >>>
        >>> # 3. Call the function to generate the inlist files.
        >>> generated_inlists = generate_mesa_rsp_inlists(
        ...     detail_df=detail_df,
        ...     mesa_output_base_dir=mesa_out_dir,
        ...     rsp_inlist_template_path=template_path,
        ...     rsp_output_subdir=rsp_out_dir
        ... )
        >>>
        >>> # 4. Check the results.
        >>> for inlist in generated_inlists:
        ...     print(f"Generated: {inlist}")
        Generated: ./rsp_inlists/run_1.0MSUN_z0.0140/model0500/inlist_rsp
        Generated: ./rsp_inlists/run_1.2MSUN_z0.0030/model0750/inlist_rsp
    """
    if detail_df.empty:
        logger.warning("Input detail_df is empty. No MESA RSP inlists will be generated.")
        return []

    if not os.path.exists(rsp_inlist_template_path):
        logger.error(f"MESA RSP inlist template not found at: {rsp_inlist_template_path}")
        return []

    # Ensure the base RSP output directory exists
    os.makedirs(rsp_output_subdir, exist_ok=True)
    
    generated_inlists = []

    # Read the template inlist once
    try:
        with open(rsp_inlist_template_path, 'r') as f:
            template_content = f.read()
    except IOError as e:
        logger.error(f"Failed to read RSP inlist template from {rsp_inlist_template_path}: {e}")
        return []

    logger.info(f"Starting generation of MESA RSP inlists for {len(detail_df)} profiles.")

    # Iterate through each row (each stellar model) in the detail_df
    for index, row in detail_df.iterrows():
        try:
            mass = row['initial_mass']
            Z = row['initial_Z']
            model_number = int(row['model_number'])
            log_Teff = row['log_Teff']
            log_L = row['log_L']
            # run_dir_path = row['run_dir_path'] # Not directly needed if creating model from scratch

            # Convert log values to eigenear scale
            Teff = round(10**log_Teff, 0) # Round to integer for Teff
            L = round(10**log_L, 0) # Round to integer for L

            # --- Get initial_Y and calculate X_val ---
            initial_Y = row.get('initial_Y', np.nan) # Safely get 'initial_Y', defaults to NaN if not present
            
            if pd.isna(initial_Y):
                # Fallback to a default X if initial_Y is missing or NaN
                X_val = 0.730 
                logger.warning(f"Initial_Y missing for M={mass:.1f}, Z={Z:.4f}, model={model_number}. Using default RSP_X={X_val:.3f}d0.")
            else:
                # Calculate X using X + Y + Z = 1
                X_val = 1.0 - initial_Y - Z
                if X_val < 0 or X_val > 1: # Basic sanity check for X value
                    logger.warning(f"Calculated X ({X_val:.3f}) is out of [0, 1] range for M={mass:.1f}, Z={Z:.4f}, Y={initial_Y:.3f}, model={model_number}. Using default RSP_X=0.730d0.")
                    X_val = 0.730
            # --- END NEW ---

            # Fixed value for RSP max_model_number as per template.
            # IMPORTANT: This assumes the template always has 'max_model_number = 1200'
            rsp_max_model_num = 1200 

            # --- Construct the new structured output path ---
            run_folder_name = f"run_{mass:.1f}MSUN_z{Z:.4f}" 
            model_folder_name = f"model{model_number:04d}" 
            
            # Full path for this specific RSP run's directory
            this_rsp_run_root_dir = os.path.join(rsp_output_subdir, run_folder_name, model_folder_name)
            
            # Create the nested directories if they don't exist
            os.makedirs(this_rsp_run_root_dir, exist_ok=True)
            
            # Define the output model filename for this RSP run (MESA will save its .mod here)
            # Example template string to replace: "save_model_filename = 'rsp_cepheid_6M_cycle0.mod'"
            save_model_filename = f'rsp_final_M{mass:.1f}Z{Z:.4f}Mod{model_number}.mod'
            
            # Replace placeholders in template - NOW TARGETING THE ACTUAL VALUES IN YOUR TEMPLATE
            current_inlist_content = template_content \
                .replace('RSP_mass = 6d0', f'RSP_mass = {mass:.1f}d0') \
                .replace('RSP_Teff = 4892', f'RSP_Teff = {int(Teff):d}') \
                .replace('RSP_L = 4660', f'RSP_L = {int(L):d}') \
                .replace('RSP_Z = 0.003d0', f'RSP_Z = {Z:.4f}d0') \
                .replace('Zbase = 0.003d0', f'Zbase = {Z:.4f}d0') \
                .replace("save_model_filename = 'rsp_cepheid_6M_cycle0.mod'", f"save_model_filename = '{save_model_filename}'") \
                .replace('max_model_number = 1192', f'max_model_number = {rsp_max_model_num}')
            
            # Replace RSP_X placeholder with the calculated X_val
            current_inlist_content = current_inlist_content.replace('RSP_X = 0.730d0', f'RSP_X = {X_val:.3f}d0')

            # Write the new inlist file inside the specific run directory
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
            continue # Allow other inlists to be generated if one fails.

    if not generated_inlists:
        logger.warning("No RSP inlist files were generated. Check detail_df content and template.")

    return generated_inlists