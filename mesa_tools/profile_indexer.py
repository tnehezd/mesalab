import os
import pandas as pd
import numpy as np

def get_profile_numbers_for_models(profiles_index_path, target_model_numbers):
    """
    Reads a profiles.index file and returns the profile numbers for the given target model numbers.
    It finds the exact profile number if available, otherwise it returns None.

    Args:
        profiles_index_path (str): Path to the profiles.index file.
        target_model_numbers (list): A list of model numbers for which to find profile numbers.

    Returns:
        dict: A dictionary where keys are target model numbers and values are their corresponding
              profile numbers, or None if not found or if the profiles.index file is invalid.
    """
    if not os.path.exists(profiles_index_path):
        print(f"Warning: profiles.index not found at {profiles_index_path}")
        return {num: None for num in target_model_numbers}

    model_to_profile_map = {}
    
    try:
        with open(profiles_index_path, 'r') as f:
            for line in f:
                parts = line.strip().split()
                if len(parts) >= 3:
                    try:
                        model_number = int(parts[0])
                        profile_number = int(parts[2])
                        model_to_profile_map[model_number] = profile_number
                    except ValueError:
                        continue # Skip lines with non-integer model/profile numbers
    except Exception as e:
        print(f"Error reading profiles.index at {profiles_index_path}: {e}")
        return {num: None for num in target_model_numbers}

    results = {}
    for target_model_number in target_model_numbers:
        if target_model_number is None or pd.isna(target_model_number):
            results[target_model_number] = None
            continue

        # Find the exact match or the nearest available profile number
        # MESA's profiles.index should ideally have a direct mapping.
        # If we need 'smaller' and 'larger' profiles for interpolation,
        # we would extend this logic. For now, let's assume we want the profile
        # that *is* the model number, or the closest *saved* profile.
        
        # Simplest approach: check if the model number itself is a profile.
        # If not, find the closest available profile based on model_number.
        
        if target_model_number in model_to_profile_map:
            results[target_model_number] = model_to_profile_map[target_model_number]
        else:
            # If the exact model number isn't a saved profile, find the closest one
            # Find all model numbers that are saved as profiles
            saved_model_numbers = np.array(list(model_to_profile_map.keys()))
            
            if len(saved_model_numbers) == 0:
                results[target_model_number] = None
                continue

            # Find the absolute difference
            diff = np.abs(saved_model_numbers - target_model_number)
            
            # Find the index of the minimum difference
            closest_idx = np.argmin(diff)
            
            # Get the closest saved model number
            closest_model_number = saved_model_numbers[closest_idx]
            
            results[target_model_number] = model_to_profile_map[closest_model_number]
            # print(f"  Note: Model {target_model_number} not found in profiles.index. Using closest profile for model {closest_model_number} (profile {results[target_model_number]}).")

    return results

# --- Example Usage for local testing ---
if __name__ == "__main__":
    # IMPORTANT: Replace with a valid path to a MESA run's LOGS directory
    # For example, if your run is at /path/to/mesa/run/my_star/
    # then profiles_index_path would be /path/to/mesa/run/my_star/LOGS/profiles.index
    
    # This assumes a structure like: base_dir/run_name/LOGS/profiles.index
    # For testing, you'd likely point it to a specific run you know.
    
    test_run_base_path = "/home/tnehezd/workdir/mesa-r23.05.1/STRANGE/nad_convos_mid/run_nad_convos_mid_11.5MSUN_z0.0200"
    test_profiles_index_path = os.path.join(test_run_base_path, 'LOGS', 'profiles.index')

    print(f"Testing profile indexer for: {test_profiles_index_path}")

    # Example target model numbers that might be min/max from blue loop analysis
    target_models = [1903, 1906, 2000] # Example: 1903 (min_model), 1906 (max_model) and another one
    
    found_profiles = get_profile_numbers_for_models(test_profiles_index_path, target_models)

    if found_profiles:
        print("\nFound Profile Numbers:")
        for model_num, profile_num in found_profiles.items():
            print(f"  Model {model_num}: Profile {profile_num}")
    else:
        print("Could not find profile numbers for the target models.")