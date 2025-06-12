import os

def count_lines_of_code(start_path='.'):
    """
    Counts lines of code (LOC) in Python files within a given directory
    and its subdirectories. Ignores blank lines and comments.

    Args:
        start_path (str): The starting directory to scan. Defaults to the current directory.

    Returns:
        int: The total count of non-blank, non-comment lines of code.
    """
    total_loc = 0
    python_files_scanned = 0
    
    # NEW: Added a very early print statement to confirm script execution
    print("--- LOC Counter Started ---") 
    print(f"Scanning for Python files in: {os.path.abspath(start_path)}")

    for root, _, files in os.walk(start_path):
        for file in files:
            if file.endswith('.py'):
                filepath = os.path.join(root, file)
                python_files_scanned += 1
                try:
                    with open(filepath, 'r', encoding='utf-8') as f:
                        for line in f:
                            stripped_line = line.strip()
                            # Ignore blank lines and lines that are purely comments or multi-line comment starts/ends
                            if stripped_line and \
                               not stripped_line.startswith('#') and \
                               not stripped_line.startswith('"""') and \
                               not stripped_line.startswith("'''") and \
                               not stripped_line.endswith('"""') and \
                               not stripped_line.endswith("'''"):
                                total_loc += 1
                except Exception as e:
                    print(f"Warning: Could not read file {filepath} due to error: {e}")
    
    print(f"\n--- LOC Count Summary ---")
    print(f"Python files scanned: {python_files_scanned}")
    print(f"Total non-blank, non-comment lines of Python code (LOC): {total_loc}")
    return total_loc

if __name__ == "__main__":
    # You can specify the path if your script is not in the project root
    # For example: count_lines_of_code('/path/to/your/mesa_project')
    
    # If this script is in your MESA project's root, just run with default:
    project_loc = count_lines_of_code('.') 

    print(f"\nJOSS LOC Requirement: Submissions under 1000 LOC are usually flagged, under 300 LOC are desk rejected.")
    if project_loc < 300:
        print(f"Your project's LOC ({project_loc}) is BELOW the 300 LOC desk rejection threshold. This might be an issue.")
    elif project_loc < 1000:
        print(f"Your project's LOC ({project_loc}) is below the 1000 LOC flagging threshold. Editors might review it more closely for 'substantial scholarly effort'.")
    else:
        print(f"Your project's LOC ({project_loc}) meets or exceeds the 1000 LOC guideline. Great!")
