import os
import argparse
import yaml
import sys
import logging
from datetime import datetime

try:
    from addict import Dict
except ImportError:
    print("Error: 'addict' library not found. Please install it using 'pip install addict'.", file=sys.stderr)
    sys.exit(1)

logger = logging.getLogger(__name__)

def parsing_options():
    """
    Parses command-line arguments and loads configuration from a YAML file.
    It combines default settings, YAML file settings, environment variables,
    and command-line arguments in order of increasing priority.
    Performs initial validation of critical paths.
    """
    # 1. Define command-line arguments.
    parser = argparse.ArgumentParser(
        description="Analyze MESA stellar evolution grid runs and generate GYRE input files.",
        add_help=False
    )

    parser.add_argument("--config", type=str, default="config.yaml",
                        help="Path to a YAML configuration file.")

    # General Settings (can be overridden by CLI)
    parser.add_argument("-i", "--input-dir", type=str, default=None,
                        help="Override general_settings.input_dir. Directory containing MESA run subdirectories.")
    parser.add_argument("-o", "--output-dir", type=str,
                        help="Override general_settings.output_dir. Output base directory for analysis results.")
    parser.add_argument("--inlist-name", type=str,
                        help="Override general_settings.inlist_name. Inlist filename to identify runs.")
    parser.add_argument("--force-reanalysis", action="store_true",
                        help="Override general_settings.force_reanalysis. Force reanalysis even if summary files exist.")
    parser.add_argument("--debug", action="store_true",
                        help="Override general_settings.debug. Enable debug mode for more verbose logging output.")

    # Explicit CLI arguments for MESA/GYRE paths
    parser.add_argument('--mesasdk-root', type=str,
                        help='Override MESASDK_ROOT path. (CLI > Env Var > config.yaml).')
    parser.add_argument('--mesa-dir', type=str,
                        help='Override MESA_DIR (specific MESA release) path. (CLI > Env Var > config.yaml).')
    # NEW: Added CLI arg for mesa_binary_dir
    parser.add_argument('--mesa-binary-dir', type=str,
                        help='Override MESA_BINARY_DIR path (where `rn` and `star` executables are). (CLI > Env Var > config.yaml).')
    parser.add_argument('--gyre-dir', type=str,
                        help='Override GYRE_DIR path. (CLI > Env Var > config.yaml).')

    # Blue Loop Analysis Settings (remain the same)
    parser.add_argument("--analyze-blue-loop", action="store_true", help="Override blue_loop_analysis.analyze_blue_loop. Perform blue loop analysis.")
    parser.add_argument("--blue-loop-output-type", choices=['summary', 'all'], help="Override blue_loop_analysis.blue_loop_output_type. Blue loop output type for detail files.")

    # Plotting Settings (remain the same)
    parser.add_argument("--generate-heatmaps", action="store_true", help="Override plotting_settings.generate_heatmaps. Generate heatmaps from cross-grid data.")
    parser.add_argument("--generate-hr-diagrams", type=str, choices=['none', 'all', 'drop_zams'], help='Override plotting_settings.generate_hr_diagrams. Control HR diagram generation.')
    parser.add_argument("--generate-blue-loop-plots-with-bc", action="store_true", help="Override plotting_settings.generate_blue_loop_plots_with_bc. Generate blue loop plots including bolometric corrections.")

    # GYRE Workflow Settings (remain the same)
    gyre_group = parser.add_argument_group('GYRE Workflow Settings')
    gyre_group.add_argument('--run-gyre-workflow', action='store_true', help='Override gyre_workflow.run_gyre_workflow. Execute the full GYRE workflow.')
    gyre_group.add_argument('--gyre-run-mode', type=str, choices=['ALL_PROFILES', 'FILTERED_PROFILES'], help='Override gyre_workflow.run_mode. Set the GYRE run mode.')
    gyre_group.add_argument('--gyre-threads', type=int, help='Override gyre_workflow.num_gyre_threads. Number of OpenMP threads for each GYRE run.')
    gyre_group.add_argument('--gyre-parallel', type=str, choices=['True', 'False'], help='Override gyre_workflow.enable_parallel. Enable/disable parallel GYRE runs (True/False).')
    gyre_group.add_argument('--gyre-max-concurrent', type=int, help='Override gyre_workflow.max_concurrent_gyre_runs. Maximum number of concurrent GYRE runs.')
    gyre_group.add_argument('--gyre-inlist-template-path', type=str, help='Override gyre_workflow.gyre_inlist_template_path. Full or relative path to the GYRE inlist template file.')

    # RSP Workflow Settings (remain the same)
    rsp_group = parser.add_argument_group('RSP Workflow Settings')
    rsp_group.add_argument('--run-rsp-workflow', action='store_true', help='Override rsp_workflow.run_rsp_workflow. Execute the full RSP workflow.')
    rsp_group.add_argument('--rsp-inlist-template-path', type=str, help='Override rsp_workflow.rsp_inlist_template_path. Full or relative path to the RSP inlist template file.')
    rsp_group.add_argument('--rsp-mesa-output-base-dir', type=str, help='Override rsp_workflow.rsp_mesa_output_base_dir. Base directory for RSP MESA output files.')

    cli_args, unknown_args = parser.parse_known_args()

    # Early debug logging setup
    if cli_args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
        logger.debug("Debug logging enabled via CLI in config_parser (early stage).")
    else:
        logging.getLogger().setLevel(logging.INFO)
        logger.info("Default logging level is INFO (early stage).")

    # 2. Define default configuration values
    default_config = {
        'general_settings': {
            'input_dir': None,
            'output_dir': './mesalab_output',
            'inlist_name': 'inlist_project',
            'force_reanalysis': False,
            'debug': False,
            'mesasdk_root': None,
            'mesa_dir': None,
            'mesa_binary_dir': None,
            'gyre_dir': None
        },
        'blue_loop_analysis': {
            'analyze_blue_loop': True,
            'blue_loop_output_type': 'all'
        },
        'plotting_settings': {
            'generate_heatmaps': False,
            'generate_hr_diagrams': 'none',
            'generate_blue_loop_plots_with_bc': False,
            'generate_plots': False
        },
        'gyre_workflow': {
            'run_gyre_workflow': True,
            'gyre_inlist_template_path': 'config/gyre.in',
            'run_mode': 'ALL_PROFILES',
            'num_gyre_threads': 1,
            'enable_parallel': False,
            'max_concurrent_gyre_runs': 4,
            'filtered_profiles_csv_name': 'sorted_blue_loop_profiles.csv'
        },
        'rsp_workflow': {
            'run_rsp_workflow': False,
            'rsp_inlist_template_path': 'config/rsp.inlist_template',
            'rsp_mesa_output_base_dir': './rsp_mesa_profiles'
        }
    }

    final_config_dict = Dict(default_config)

    # 3. Load user-provided YAML configuration
    user_yaml_config = {}
    config_file_path = cli_args.config
    resolved_config_file_path = os.path.abspath(config_file_path)

    if os.path.exists(resolved_config_file_path):
        try:
            with open(resolved_config_file_path, 'r') as f:
                user_yaml_config = yaml.safe_load(f)
                if user_yaml_config is None:
                    user_yaml_config = {}
            logger.info(f"Loaded configuration from '{resolved_config_file_path}'.")
        except yaml.YAMLError as e:
            logger.critical(f"Error parsing YAML configuration file '{resolved_config_file_path}': {e}")
            sys.exit(1)
        except Exception as e:
            logger.critical(f"An unexpected error occurred while loading config from '{resolved_config_file_path}': {e}")
            sys.exit(1)
    else:
        logger.warning(f"Configuration file '{resolved_config_file_path}' not found. Using default settings and CLI arguments.")

    # Merge YAML config into final_config_dict (YAML overrides defaults)
    final_config_dict.update(user_yaml_config)

    # --- Apply Environment Variables (Override YAML, but overridden by CLI) ---

    # MESASDK_ROOT (SDK installation dir)
    env_mesasdk_root = os.getenv('MESASDK_ROOT')
    if env_mesasdk_root and os.path.isdir(env_mesasdk_root):
        if final_config_dict.general_settings.mesasdk_root != env_mesasdk_root:
            logger.info(f"MESASDK_ROOT set from $MESASDK_ROOT environment variable: {env_mesasdk_root} (overriding YAML if present).")
            final_config_dict.general_settings.mesasdk_root = env_mesasdk_root

    # MESA_DIR (specific MESA release dir)
    env_mesa_dir = os.getenv('MESA_DIR')
    if env_mesa_dir and os.path.isdir(env_mesa_dir):
        if final_config_dict.general_settings.mesa_dir != env_mesa_dir:
            logger.info(f"MESA_DIR set from $MESA_DIR environment variable: {env_mesa_dir} (overriding YAML if present).")
            final_config_dict.general_settings.mesa_dir = env_mesa_dir

    # MESA_BINARY_DIR (new env var check)
    env_mesa_binary_dir = os.getenv('MESA_BINARY_DIR')
    if env_mesa_binary_dir and os.path.isdir(env_mesa_binary_dir):
        if final_config_dict.general_settings.mesa_binary_dir != env_mesa_binary_dir:
            logger.info(f"MESA_BINARY_DIR set from $MESA_BINARY_DIR environment variable: {env_mesa_binary_dir} (overriding YAML if present).")
            final_config_dict.general_settings.mesa_binary_dir = env_mesa_binary_dir

    # GYRE_DIR
    env_gyre_dir = os.getenv('GYRE_DIR')
    if env_gyre_dir and os.path.isdir(env_gyre_dir):
        if final_config_dict.general_settings.gyre_dir != env_gyre_dir:
            logger.info(f"GYRE_DIR set from $GYRE_DIR environment variable: {env_gyre_dir} (overriding YAML if present).")
            final_config_dict.general_settings.gyre_dir = env_gyre_dir
    elif final_config_dict.general_settings.gyre_dir and not os.path.isdir(final_config_dict.general_settings.gyre_dir):
        logger.error(f"GYRE_DIR set in config to '{final_config_dict.general_settings.gyre_dir}' but it's not a valid directory. GYRE dependent tasks might fail.")
    else:
        logger.debug("GYRE_DIR not explicitly set in config and $GYRE_DIR not found or not valid.")
    # --- END Environment Variable Application ---


    # 4. Apply CLI arguments, overriding everything else
    for arg_action in parser._actions:
        arg_name = arg_action.dest
        cli_value = getattr(cli_args, arg_name, None)

        if arg_name == 'config':
            continue

        cli_set_explicitly = False
        if isinstance(arg_action, (argparse._StoreTrueAction, argparse._StoreFalseAction)):
            if cli_value is not None:
                if cli_value != arg_action.default:
                    cli_set_explicitly = True
        else:
            if cli_value is not None and cli_value != arg_action.default:
                cli_set_explicitly = True

        if cli_set_explicitly:
            logger.debug(f"Applying CLI override: --{arg_name} = {cli_value}")
            if arg_name in ['input_dir', 'output_dir', 'inlist_name', 'force_reanalysis', 'debug',
                            'mesasdk_root', 'mesa_dir', 'mesa_binary_dir', 'gyre_dir']:
                final_config_dict.general_settings[arg_name] = cli_value
            elif arg_name in ['analyze_blue_loop', 'blue_loop_output_type']:
                final_config_dict.blue_loop_analysis[arg_name] = cli_value
            elif arg_name in ['generate_heatmaps', 'generate_hr_diagrams', 'generate_blue_loop_plots_with_bc']:
                final_config_dict.plotting_settings[arg_name] = cli_value
            elif arg_name == 'run_gyre_workflow':
                final_config_dict.gyre_workflow.run_gyre_workflow = cli_value
            elif arg_name == 'gyre_run_mode':
                final_config_dict.gyre_workflow.run_mode = cli_value
            elif arg_name == 'gyre_threads':
                final_config_dict.gyre_workflow.num_gyre_threads = cli_value
            elif arg_name == 'gyre_parallel':
                final_config_dict.gyre_workflow.enable_parallel = (cli_value.lower() == 'true')
            elif arg_name == 'gyre_max_concurrent':
                final_config_dict.gyre_workflow.max_concurrent_gyre_runs = cli_value
            elif arg_name == 'gyre_inlist_template_path':
                final_config_dict.gyre_workflow.gyre_inlist_template_path = cli_value
            elif arg_name == 'run_rsp-workflow':
                final_config_dict.rsp_workflow.run_rsp_workflow = cli_value
            elif arg_name == 'rsp_inlist_template_path':
                final_config_dict.rsp_workflow.rsp_inlist_template_path = cli_value
            elif arg_name == 'rsp_mesa_output_base_dir':
                final_config_dict.rsp_workflow.rsp_mesa_output_base_dir = cli_value
            else:
                logger.debug(f"CLI argument '{arg_name}' with value '{cli_value}' was provided but not explicitly mapped to a config setting. It will be ignored.")

    # Set internal flag for plotting based on other plotting settings
    final_config_dict.plotting_settings.generate_plots = (
        final_config_dict.plotting_settings.generate_heatmaps or
        final_config_dict.plotting_settings.generate_hr_diagrams != 'none' or
        final_config_dict.plotting_settings.generate_blue_loop_plots_with_bc
    )
    if final_config_dict.plotting_settings.generate_plots:
        logger.debug("Internal 'generate_plots' flag set to True as a specific plotting option is enabled.")

    # 5. Final validation for required arguments and paths
    # Input directory is always required
    if final_config_dict.general_settings.input_dir is None:
        logger.critical("ERROR: 'input_dir' must be specified either via command-line (--input-dir) or in the config file.")
        sys.exit(1)

    # --- Conditional validation for dependencies based on enabled workflows ---
    is_gyre_or_rsp_workflow_enabled = (
        final_config_dict.gyre_workflow.get('run_gyre_workflow', False) or
        final_config_dict.rsp_workflow.get('run_rsp_workflow', False)
    )

    if is_gyre_or_rsp_workflow_enabled:
        logger.info("A GYRE or RSP workflow is enabled. Validating required paths.")
        
        # Validate MESASDK_ROOT
        if final_config_dict.general_settings.mesasdk_root:
            if not os.path.isdir(final_config_dict.general_settings.mesasdk_root):
                logger.critical(f"ERROR: Resolved MESASDK_ROOT path is not a valid directory: '{final_config_dict.general_settings.mesasdk_root}'. Please ensure your $MESASDK_ROOT environment variable or config.yaml setting is correct.")
                sys.exit(1)
            logger.info(f"Validated MESASDK_ROOT: {final_config_dict.general_settings.mesasdk_root}")
        else:
            # If MESA_DIR is not set, try to infer it from MESASDK_ROOT
            if final_config_dict.general_settings.mesasdk_root:
                logger.info(f"MESA_DIR not set. Attempting to auto-select the latest MESA release within MESASDK_ROOT: {final_config_dict.general_settings.mesasdk_root}")
                latest_mesa_release = None
                latest_version_num = 0
                for item in os.listdir(final_config_dict.general_settings.mesasdk_root):
                    if item.startswith('mesa-r') and os.path.isdir(os.path.join(final_config_dict.general_settings.mesasdk_root, item)):
                        try:
                            version_str = item.replace('mesa-r', '').replace('.', '')
                            version_num = int(version_str)
                            if version_num > latest_version_num:
                                latest_version_num = version_num
                                latest_mesa_release = os.path.join(final_config_dict.general_settings.mesasdk_root, item)
                        except ValueError:
                            continue
                
                if latest_mesa_release:
                    final_config_dict.general_settings.mesa_dir = latest_mesa_release
                    logger.info(f"Auto-selected MESA_DIR as the latest found within MESASDK_ROOT: {latest_mesa_release}")
                else:
                    logger.critical(f"ERROR: MESA_DIR could not be determined within MESASDK_ROOT: '{final_config_dict.general_settings.mesasdk_root}'. Please ensure your MESA installation is correct.")
                    sys.exit(1)
            else:
                logger.critical("ERROR: Neither MESA_DIR nor MESASDK_ROOT are set or could be determined. MESA-dependent workflows cannot proceed without at least one of them being valid.")
                sys.exit(1)
                
        # Validate MESA_DIR
        if final_config_dict.general_settings.mesa_dir:
            if not os.path.isdir(final_config_dict.general_settings.mesa_dir):
                logger.critical(f"ERROR: Resolved MESA_DIR path is not a valid directory: '{final_config_dict.general_settings.mesa_dir}'. Please ensure your $MESA_DIR environment variable or config.yaml setting is correct.")
                sys.exit(1)
            logger.info(f"Validated MESA_DIR: {final_config_dict.general_settings.mesa_dir}")
        else:
            logger.critical("ERROR: MESA_DIR is not set and could not be determined from MESASDK_ROOT. Cannot proceed with MESA-dependent workflows.")
            sys.exit(1)


        # Validate GYRE_DIR
        if final_config_dict.general_settings.gyre_dir:
            if not os.path.isdir(final_config_dict.general_settings.gyre_dir):
                logger.critical(f"ERROR: Resolved GYRE_DIR path is not a valid directory: '{final_config_dict.general_settings.gyre_dir}'. Please ensure your $GYRE_DIR environment variable or config.yaml setting is correct.")
                sys.exit(1)
            logger.info(f"Validated GYRE_DIR: {final_config_dict.general_settings.gyre_dir}")
    
    # --- GYRE Workflow specific validations ---
    if final_config_dict.gyre_workflow.get('run_gyre_workflow', False):
        logger.debug("GYRE workflow enabled. Performing final validation of GYRE parameters.")
        required_gyre_params = [
            'run_mode', 'gyre_inlist_template_path', 'num_gyre_threads',
            'enable_parallel', 'max_concurrent_gyre_runs'
        ]
        for param in required_gyre_params:
            if getattr(final_config_dict.gyre_workflow, param, None) is None:
                if param in ['num_gyre_threads', 'max_concurrent_gyre_runs'] and not isinstance(final_config_dict.gyre_workflow.get(param), (int, float)):
                    logger.critical(f"Missing or invalid required GYRE workflow parameter: 'gyre_workflow.{param}'. Please check config.yaml or CLI arguments.")
                    sys.exit(1)
                elif param == 'enable_parallel' and not isinstance(final_config_dict.gyre_workflow.get(param), bool):
                    logger.critical(f"Missing or invalid required GYRE workflow parameter: 'gyre_workflow.{param}'. Please check config.yaml or CLI arguments.")
                    sys.exit(1)
                else:
                    logger.critical(f"Missing required GYRE workflow parameter: 'gyre_workflow.{param}'. Please check config.yaml or CLI arguments.")
                    sys.exit(1)
            if param in ['num_gyre_threads', 'max_concurrent_gyre_runs'] and final_config_dict.gyre_workflow[param] <= 0:
                logger.critical(f"GYRE workflow parameter 'gyre_workflow.{param}' must be a positive integer.")
                sys.exit(1)
        gyre_template_path_to_check = final_config_dict.gyre_workflow.gyre_inlist_template_path
        if not os.path.isabs(gyre_template_path_to_check):
            gyre_template_path_to_check = os.path.abspath(gyre_template_path_to_check)
        if not os.path.exists(gyre_template_path_to_check):
            logger.critical(f"GYRE inlist template file not found at: '{gyre_template_path_to_check}'. Please ensure the path is correct in your config or via CLI.")
            sys.exit(1)

    # --- RSP Workflow specific validations ---
    if final_config_dict.rsp_workflow.get('run_rsp_workflow', False):
        logger.debug("RSP workflow enabled. Performing final validation of RSP parameters.")
        required_rsp_params = ['rsp_inlist_template_path', 'rsp_mesa_output_base_dir']
        for param in required_rsp_params:
            if getattr(final_config_dict.rsp_workflow, param, None) is None:
                logger.critical(f"Missing required RSP workflow parameter: 'rsp_workflow.{param}'. Please check config.yaml or CLI arguments.")
                sys.exit(1)
        rsp_template_path_to_check = final_config_dict.rsp_workflow.rsp_inlist_template_path
        if not os.path.isabs(rsp_template_path_to_check):
            rsp_template_path_to_check = os.path.abspath(rsp_template_path_to_check)
        if not os.path.exists(rsp_template_path_to_check):
            logger.critical(f"RSP inlist template file not found at: '{rsp_template_path_to_check}'. Please ensure the path is correct in your config or via CLI.")
            sys.exit(1)
        rsp_output_base_dir = final_config_dict.rsp_workflow.rsp_mesa_output_base_dir
        if not os.path.isabs(rsp_output_base_dir):
            rsp_output_base_dir = os.path.abspath(rsp_output_base_dir)
        parent_dir_rsp = os.path.dirname(rsp_output_base_dir)
        if not os.path.exists(parent_dir_rsp):
            logger.critical(f"Parent directory for RSP MESA output base directory does not exist: '{parent_dir_rsp}'. Please create it or provide a valid path.")
            sys.exit(1)
        if not os.access(parent_dir_rsp, os.W_OK):
            logger.critical(f"Parent directory for RSP MESA output base directory is not writable: '{parent_dir_rsp}'. Please check permissions.")
            sys.exit(1)

    # Adjust logging level one final time after all config has been processed
    if final_config_dict.general_settings.debug:
        logging.root.setLevel(logging.DEBUG)
        logger.setLevel(logging.DEBUG)
        logger.debug("Debug mode enabled after full config merge in config_parser.")
    else:
        logging.root.setLevel(logging.INFO)
        logger.info("Default logging level is INFO after full config merge in config_parser.")

    logger.info(f"Final resolved configuration: {final_config_dict}")
    return final_config_dict

if __name__ == '__main__':
    print("Running config_parser.py directly for testing purposes.")
    print("You can try: python -m mesalab.io.config_parser --input-dir /tmp/test --debug --run-rsp-workflow")
    print("Or with a config file: python -m mesalab.io.config_parser --config your_config.yaml")

    dummy_config_content = """
    general_settings:
      input_dir: /path/to/your/mesa_runs
      output_dir: ./mesalab_output_test
      mesasdk_root: /Applications/mesasdk_test
      mesa_dir: /Users/tnehezd/Documents/Munka/MESA230501/mesa-r23.05.01_test
      mesa_binary_dir: /Users/tnehezd/Documents/Munka/MESA230501/mesa-r23.05.01_test/star/work
      gyre_dir: /path/to/gyre_test
      debug: false
    rsp_workflow:
      run_rsp_workflow: true
      rsp_inlist_template_path: config/rsp.inlist_template_test
      rsp_mesa_output_base_dir: ./rsp_mesa_profiles_test
    """
    with open("test_config.yaml", "w") as f:
        f.write(dummy_config_content)
    
    try:
        sys.argv = ['config_parser.py', '--config', 'test_config.yaml', '--input-dir', '/tmp/cli_input', '--force-reanalysis']
        os.environ['MESA_DIR'] = '/env/mesa/dir'
        os.environ['MESASDK_ROOT'] = '/env/mesasdk/root'
        os.environ['MESA_BINARY_DIR'] = '/env/mesa/binary/dir'
        os.environ['GYRE_DIR'] = '/env/gyre/dir'

        parsed_config = parsing_options()
        print("\n--- Parsed Configuration (from __main__ block) ---")
        print(parsed_config)
        print(f"Input Dir: {parsed_config.general_settings.input_dir}")
        print(f"MESA SDK Root: {parsed_config.general_settings.mesasdk_root}")
        print(f"MESA Dir: {parsed_config.general_settings.mesa_dir}")
        print(f"MESA Binary Dir: {parsed_config.general_settings.mesa_binary_dir}")
        print(f"GYRE Dir: {parsed_config.general_settings.gyre_dir}")
        print(f"Force Reanalysis: {parsed_config.general_settings.force_reanalysis}")
        print(f"RSP workflow enabled: {parsed_config.rsp_workflow.run_rsp_workflow}")

    except SystemExit as e:
        print(f"Parsing exited with code: {e.code}")
    finally:
        if os.path.exists("test_config.yaml"):
            os.remove("test_config.yaml")
        if 'MESA_DIR' in os.environ:
            del os.environ['MESA_DIR']
        if 'MESASDK_ROOT' in os.environ:
            del os.environ['MESASDK_ROOT']
        if 'MESA_BINARY_DIR' in os.environ:
            del os.environ['MESA_BINARY_DIR']
        if 'GYRE_DIR' in os.environ:
            del os.environ['GYRE_DIR']
        sys.argv = [sys.argv[0]]