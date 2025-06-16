import subprocess
import os
import multiprocessing
import shutil
import glob
import f90nml # Make sure you've run: pip install f90nml

# --- GYRE Execution Function (slightly modified for config parameters) ---

def run_single_gyre_model(
    model_profile_path,
    gyre_inlist_template_path,
    output_dir,
    gyre_executable,
    num_gyre_threads
):
    """
    Runs a single GYRE model for a given MESA profile.
    Dynamically generates the inlist file for the specific run.
    """
    os.makedirs(output_dir, exist_ok=True)
    
    # 1. Generate the inlist file for the current run
    # Read the template inlist
    nml = f90nml.read(gyre_inlist_template_path)

    # Set the MESA profile path in the inlist
    nml['model']['file'] = os.path.abspath(model_profile_path)

    # Set the output file names to be in the specific output directory
    # We only provide the filename here; the subprocess.run cwd parameter handles the directory
#    nml['nad_output']['summary_file'] = 'summary_output.h5'
#    nml['nad_output']['detail_template'] = 'detail_output_%ID_%N'
#    nml['nad_output']['detail_file_format'] = 'HDF' # Ensure HDF format

    # Generate a unique inlist filename for this run
    profile_base_name = os.path.basename(model_profile_path).replace('.data.GYRE', '')
    run_inlist_path = os.path.join(output_dir, f'gyre_inlist_{profile_base_name}.in')
    nml.write(run_inlist_path, force=True) # Add force=True to overwrite existing files
    
    # 2. Set OpenMP threads
    # This sets the number of cores GYRE will use for *one* run
    os.environ['OMP_NUM_THREADS'] = str(num_gyre_threads)

    # 3. Assemble and run the GYRE command
    command = [gyre_executable, os.path.basename(run_inlist_path)] # Just the filename because we're using cwd

    print(f"**[{os.path.basename(output_dir)}]** Attempting to run GYRE with command: `{gyre_executable} {os.path.basename(run_inlist_path)}` (OMP_NUM_THREADS={num_gyre_threads})")

    try:
        # Important: cwd=output_dir, so GYRE writes the generated inlist and outputs there
        result = subprocess.run(command, capture_output=True, text=True, check=True, cwd=output_dir)

        print(f"**[{os.path.basename(output_dir)}] GYRE run SUCCESSFUL**!")
        print("--- Standard Output (stdout) ---")
        print(result.stdout)
        if result.stderr:
            print("--- Standard Error (stderr) ---")
            print(result.stderr)
        print("--- Run finished ---")

    except subprocess.CalledProcessError as e:
        print(f"**[{os.path.basename(output_dir)}] ERROR: GYRE run FAILED**, exit code: **{e.returncode}**!")
        print("--- Standard Output (stdout) ---")
        print(e.stdout)
        print("--- Standard Error (stderr) ---")
        print(e.stderr)
        print("--- Error during run ---")
        raise # Re-raise the exception
    except Exception as e:
        print(f"**[{os.path.basename(output_dir)}] UNEXPECTED ERROR occurred during GYRE run:** {e}")
        print("--- Error during run ---")
        raise


# --- Main mesalab Controller Script ---

def main():
    config_file = 'gyre_config.in'

    if not os.path.exists(config_file):
        raise FileNotFoundError(f"Configuration file '{config_file}' not found. Please create it.")

    print(f"**Reading configuration from '{config_file}'...**")
    config = f90nml.read(config_file)

    # Accessing settings from the config file
    setup_cfg = config['setup']
    run_cfg = config['run_control']
    gyre_cfg = config['gyre_options']

    # --- Setup Paths ---
    mesa_dir = setup_cfg['mesa_dir']
    mesasdk_root = setup_cfg['mesasdk_root']
    gyre_dir = setup_cfg['gyre_dir']
    output_base_dir = setup_cfg['output_base_dir']

    # Ensure output base directory exists
    os.makedirs(output_base_dir, exist_ok=True)
    print(f"**Output will be saved in: '{os.path.abspath(output_base_dir)}'**")

    # Determine GYRE executable path
    gyre_executable = os.path.join(gyre_dir, 'bin', 'gyre')
    
    # Verify GYRE executable exists and is runnable
    if not (os.path.exists(gyre_executable) and os.path.isfile(gyre_executable) and os.access(gyre_executable, os.X_OK)):
        # Fallback: check if GYRE_DIR itself is the binary dir
        gyre_executable_fallback = os.path.join(gyre_dir, 'gyre')
        if (os.path.exists(gyre_executable_fallback) and os.path.isfile(gyre_executable_fallback) and os.access(gyre_executable_fallback, os.X_OK)):
            gyre_executable = gyre_executable_fallback
        else:
            raise FileNotFoundError(f"GYRE executable not found or not executable at '{gyre_executable}' or '{gyre_executable_fallback}'. "
                                    f"Please check your 'gyre_dir' in '{config_file}'.")
    print(f"**GYRE executable found at: '{gyre_executable}'**")


    # --- Run MESA (Placeholder) ---
    if run_cfg['run_mesa']:
        print("\n--- MESA Run (Not yet implemented) ---")
        # In the future, MESA execution logic would go here
        pass

    # --- Run GYRE ---
    if run_cfg['run_gyre']:
        print("\n--- GYRE Run Starting ---")

        # Find MESA profiles
        mesa_profile_source_dir = os.path.join(mesa_dir, gyre_cfg['mesa_profile_base_dir'])
        
        # Check if the base directory for profiles exists
        if not os.path.exists(mesa_profile_source_dir):
            raise FileNotFoundError(f"MESA profile base directory not found: '{mesa_profile_source_dir}'. "
                                    f"Please check 'mesa_dir' and 'mesa_profile_base_dir' in '{config_file}'.")

        # Use glob to find all matching profile files
        profile_paths = sorted(glob.glob(os.path.join(mesa_profile_source_dir, gyre_cfg['mesa_profile_pattern'])))
        
        if not profile_paths:
            print(f"**WARNING:** No MESA profile files found in '{mesa_profile_source_dir}' matching pattern '{gyre_cfg['mesa_profile_pattern']}'. Skipping GYRE runs.")
        else:
            print(f"**Found {len(profile_paths)} MESA profile(s) to process.**")
            
            tasks = []
            for i, profile_path in enumerate(profile_paths):
                # Create a unique output directory for each GYRE run
                profile_filename = os.path.basename(profile_path)
                run_output_name = profile_filename.replace('.data.GYRE', '') # Clean name for directory
                run_output_dir = os.path.join(output_base_dir, run_output_name)
                
                tasks.append((
                    profile_path,
                    gyre_cfg['gyre_inlist_template'], # Path to your base gyre.in
                    run_output_dir,
                    gyre_executable,
                    gyre_cfg['num_gyre_threads']
                ))

            if gyre_cfg['enable_parallel']:
                print(f"**Parallel GYRE execution enabled.** Running {gyre_cfg['max_concurrent_gyre_runs']} job(s) concurrently.")
                with multiprocessing.Pool(processes=gyre_cfg['max_concurrent_gyre_runs']) as pool:
                    pool.starmap(run_single_gyre_model, tasks)
            else:
                print("**Parallel GYRE execution disabled.** Running jobs sequentially.")
                for task in tasks:
                    run_single_gyre_model(*task)
        
        print("\n--- GYRE Run Finished ---")

    # --- Run Analysis (Placeholder) ---
    if run_cfg['run_analysis']:
        print("\n--- Analysis Run (Not yet implemented) ---")
        # In the future, analysis logic would go here, processing GYRE outputs
        pass

    # --- Run Plots (Placeholder) ---
    if run_cfg['run_plots']:
        print("\n--- Plotting Run (Not yet implemented) ---")
        # In the future, plotting logic would go here, using analysis results
        pass

    print("\n**mesalab execution complete.**")

if __name__ == "__main__":
    try:
        main()
    except FileNotFoundError as e:
        print(f"\n**[Critical Error]:** {e}")
    except subprocess.CalledProcessError as e:
        print(f"\n**[GYRE Execution Error]:** A GYRE process failed. Check the output above for details.")
    except Exception as e:
        print(f"\n**[Unexpected Error]:** An unexpected error occurred: {e}")
        import traceback
        traceback.print_exc() # Print full traceback for debugging