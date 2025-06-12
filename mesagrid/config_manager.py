# mesagrid/config_manager.py
import toml
import os

def load_config(config_path=None):
    """
    Loads configuration from a TOML file.

    Args:
        config_path (str, optional): The path to the TOML configuration file.
                                     If None, it tries to load from the default path
                                     (project_root/config.toml).

    Returns:
        dict: A dictionary containing the loaded configuration.
              Returns an empty dictionary if the file is not found or invalid.
    """
    if config_path is None:
        # Try to find config.toml in the project root relative to this script's location
        current_dir = os.path.dirname(os.path.abspath(__file__))
        project_root = os.path.join(current_dir, '..', '..') # Go up two levels from mesagrid/
        config_path = os.path.join(project_root, 'config.toml')

    if not os.path.exists(config_path):
        print(f"Warning: Configuration file not found at '{config_path}'. Using default values or program defaults.")
        return {} # Return empty dict if not found

    try:
        with open(config_path, 'r') as f:
            config = toml.load(f)
        return config
    except toml.TomlDecodeError as e:
        print(f"Error parsing configuration file '{config_path}': {e}. Using program defaults.")
        return {}
    except Exception as e:
        print(f"An unexpected error occurred while loading config '{config_path}': {e}. Using program defaults.")
        return {}

# Example usage (for testing this module directly)
if __name__ == "__main__":
    config = load_config()
    print("Loaded Config:", config)
    print("MESA Output Subdir:", config.get('paths', {}).get('mesa_output_subdir', 'LOGS (default in code)'))
    print("Inlist Filename:", config.get('filenames', {}).get('inlist_filename', 'inlist (default in code)'))