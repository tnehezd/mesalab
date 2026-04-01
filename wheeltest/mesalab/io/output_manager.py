#mesalab/io/output_manager.py
import os
import logging

def create_output_directories(output_dir, analyze_blue_loop, should_generate_plots, should_generate_blue_loop_plots_with_bc):
    """
    Creates all necessary output subdirectories.

    Args:
        output_dir (str): Base output directory.
        analyze_blue_loop (bool): Flag to determine if the 'detail_files' directory should be created.
        should_generate_plots (bool): Flag to determine if the general 'plots' directory should be created.
        should_generate_blue_loop_plots_with_bc (bool): Flag to determine if 'blue_loop_plots_bc' directory should be created.

    Returns:
        tuple: Paths to (analysis_results_sub_dir, plots_sub_dir, blue_loop_plots_bc_sub_dir, detail_files_output_dir)

    Example:
        >>> from mesalab.io import output_manager
        >>> output_manager.create_output_directories("output", True, True, False)
        ('output/analysis_results', 'output/plots', 'output/blue_loop_plots_bc', 'output/detail_files')

    """
    analysis_results_sub_dir = os.path.join(output_dir, "analysis_results")
    plots_sub_dir = os.path.join(output_dir, "plots")
    blue_loop_plots_bc_sub_dir = os.path.join(output_dir, "blue_loop_plots_bc")
    detail_files_output_dir = os.path.join(output_dir, "detail_files")

    # Always create analysis_results as it's central to the output
    os.makedirs(analysis_results_sub_dir, exist_ok=True)
    logging.info(f"Created/ensured directory: {analysis_results_sub_dir}")

    if analyze_blue_loop:
        os.makedirs(detail_files_output_dir, exist_ok=True)
        logging.info(f"Created/ensured directory: {detail_files_output_dir}")
    if should_generate_plots:
        os.makedirs(plots_sub_dir, exist_ok=True)
        logging.info(f"Created/ensured directory: {plots_sub_dir}")
    if should_generate_blue_loop_plots_with_bc:
        os.makedirs(blue_loop_plots_bc_sub_dir, exist_ok=True)
        logging.info(f"Created/ensured directory: {blue_loop_plots_bc_sub_dir}")
    
    return analysis_results_sub_dir, plots_sub_dir, blue_loop_plots_bc_sub_dir, detail_files_output_dir

def get_analysis_file_paths(analysis_results_sub_dir):
    """
    Returns the full paths for the main analysis CSV files.

    Args:
        analysis_results_sub_dir (str): Path to the analysis results subdirectory.

    Returns:
        tuple: (summary_csv_path, cross_csv_path)

    Example:
        >>> from mesalab.io import output_manager    
        >>> output_manager.get_analysis_file_paths("output/analysis_results")
        ('output/analysis_results/mesa_grid_analysis_summary.csv',
         'output/analysis_results/mesa_grid_cross.csv')
    """
    summary_csv_path = os.path.join(analysis_results_sub_dir, "mesa_grid_analysis_summary.csv")
    cross_csv_path = os.path.join(analysis_results_sub_dir, "mesa_grid_cross.csv")
    return summary_csv_path, cross_csv_path