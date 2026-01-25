"""Main entry point for COBOL AST Parser.

This module provides the CLI interface for analyzing COBOL programs.
"""

import argparse
import json
import sys
import logging
import time
from datetime import datetime
from pathlib import Path
from typing import Optional, List

import yaml

from preprocessor import CopyResolver, SourceFormat, detect_format, normalize_source
from parser import CobolParser, ParseError
from cobol_ast import ASTBuilder
from analyzers import ImpactAnalyzer
from output import JSONWriter, VariableFilter

__version__ = "1.3.0"


def setup_logging(level: str = "INFO", quiet: bool = False) -> None:
    """Configure logging.

    Args:
        level: Logging level string (DEBUG, INFO, WARNING, ERROR)
        quiet: If True, suppress all output except errors
    """
    if quiet:
        level = "ERROR"
    logging.basicConfig(
        level=getattr(logging, level.upper()),
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )


def load_config(config_path: Optional[Path] = None) -> dict:
    """Load configuration from YAML file.

    Args:
        config_path: Path to configuration file

    Returns:
        Configuration dictionary
    """
    default_config = {
        "copybook_paths": ["."],
        "cobol_extensions": [".cob", ".cbl", ".cobol"],
        "parser": {
            "max_copy_depth": 16,
            "default_format": "fixed",
            "auto_detect_format": True,
        },
        "output": {
            "include_line_numbers": True,
            "include_modification_type": True,
            "pretty_print": True,
            "indent_size": 2,
        },
        "logging": {"level": "INFO"},
    }

    if config_path and config_path.exists():
        with open(config_path, "r") as f:
            user_config = yaml.safe_load(f)
            if user_config:
                # Merge user config with defaults
                for key, value in user_config.items():
                    if isinstance(value, dict) and key in default_config:
                        default_config[key].update(value)
                    else:
                        default_config[key] = value

    return default_config


def analyze_cobol_file(
    source_path: Path,
    copybook_paths: Optional[List[Path]] = None,
    resolve_copies: bool = True,
    output_path: Optional[Path] = None,
    config: Optional[dict] = None,
    include_source_info: bool = False,
) -> dict:
    """Analyze a COBOL source file.

    Args:
        source_path: Path to COBOL source file
        copybook_paths: Paths to search for copybooks
        resolve_copies: Whether to resolve COPY statements
        output_path: Optional path to write JSON output
        config: Configuration dictionary
        include_source_info: Whether to include source file metadata

    Returns:
        Analysis results dictionary with execution timing
    """
    start_time = time.perf_counter()
    config = config or load_config()
    logger = logging.getLogger(__name__)

    # Read source file
    logger.info(f"Reading source file: {source_path}")
    source = source_path.read_text(encoding="utf-8", errors="replace")
    source_lines_count = len(source.splitlines())

    # Detect format
    source_lines = source.splitlines()
    source_format = detect_format(source_lines)
    logger.info(f"Detected source format: {source_format.value}")

    # Resolve COPY statements
    if resolve_copies:
        copy_paths = copybook_paths or [Path(p) for p in config.get("copybook_paths", ["."])]
        copy_paths = [source_path.parent] + copy_paths  # Add source directory

        resolver = CopyResolver(copy_paths)
        try:
            source = resolver.resolve(source, source_path.name)
            logger.info("COPY statements resolved successfully")
        except Exception as e:
            logger.warning(f"Error resolving COPY statements: {e}")

    # Normalize source
    source = normalize_source(source, source_format)

    # Parse
    logger.info("Parsing COBOL source...")
    parser = CobolParser(use_generated=False)  # Use simplified parser
    try:
        parse_tree = parser.parse(source)
    except ParseError as e:
        logger.error(f"Parse error: {e}")
        raise

    # Build AST
    logger.info("Building AST...")
    builder = ASTBuilder()
    program = builder.build(parse_tree)
    logger.info(f"Program name: {program.name}")

    # Analyze
    logger.info("Performing impact analysis...")
    analyzer = ImpactAnalyzer(program)
    analyzer.analyze()

    # Generate output
    output = analyzer.generate_output()

    # Calculate execution time
    end_time = time.perf_counter()
    execution_time_seconds = end_time - start_time

    # Add execution metadata
    output["execution_time_seconds"] = round(execution_time_seconds, 4)

    # Add source file info if requested
    if include_source_info:
        output["source_info"] = {
            "file_path": str(source_path.absolute()),
            "file_name": source_path.name,
            "source_format": source_format.value,
            "lines_count": source_lines_count,
        }

    # Write to file if path provided
    if output_path:
        output_config = config.get("output", {})
        writer = JSONWriter(
            pretty_print=output_config.get("pretty_print", True),
            indent=output_config.get("indent_size", 2),
            include_line_numbers=output_config.get("include_line_numbers", True),
            include_modification_types=output_config.get("include_modification_type", True),
        )
        writer.write(output, output_path)
        logger.info(f"Output written to: {output_path}")

    return output


def handle_analyze(args) -> int:
    """Handle the analyze subcommand.

    Args:
        args: Parsed arguments

    Returns:
        Exit code
    """
    logger = logging.getLogger(__name__)

    # Validate input
    if not args.source.exists():
        logger.error(f"Source file not found: {args.source}")
        return 1

    if not args.source.is_file():
        logger.error(f"Source path is not a file: {args.source}")
        return 1

    # Create output directory if specified and doesn't exist
    if args.output_dir:
        try:
            args.output_dir.mkdir(parents=True, exist_ok=True)
            logger.info(f"Output directory: {args.output_dir}")
        except OSError as e:
            logger.error(f"Failed to create output directory: {e}")
            return 1

    # Load config
    config = load_config(args.config)

    try:
        # Analyze
        output = analyze_cobol_file(
            source_path=args.source,
            copybook_paths=args.copybook_paths,
            resolve_copies=not args.no_copy_resolution,
            config=config,
            include_source_info=args.include_source_info,
        )

        # Store execution time before transformations
        execution_time = output.get("execution_time_seconds", 0)

        # Handle compact output - transform the full output to compact format
        if args.compact:
            # Convert full output to compact by deduplicating variables per section
            compact_sections = {}
            for name, modifications in output.get("sections_and_paragraphs", {}).items():
                var_to_records = {}
                for mod in modifications:
                    var = mod.get("variable", "")
                    records = mod.get("affected_records", [])
                    if var not in var_to_records:
                        var_to_records[var] = set()
                    var_to_records[var].update(records)
                compact_sections[name] = [
                    {"variable": var, "affected_records": sorted(list(recs))}
                    for var, recs in var_to_records.items()
                ]
            output = {
                "analysis_date": output.get("analysis_date"),
                "program_name": output.get("program_name", "UNKNOWN"),
                "execution_time_seconds": execution_time,
                "sections_and_paragraphs": compact_sections,
            }
            if args.include_source_info and "source_info" in output:
                output["source_info"] = output.get("source_info")

        # Handle summary only
        if args.summary_only:
            output = {
                "analysis_date": output.get("analysis_date"),
                "program_name": output.get("program_name", "UNKNOWN"),
                "execution_time_seconds": execution_time,
                "summary": output.get("summary", {}),
            }
            if args.include_source_info and "source_info" in output:
                output["source_info"] = output.get("source_info")

        # Output
        output_config = config.get("output", {})
        writer = JSONWriter(
            pretty_print=output_config.get("pretty_print", True),
            indent=output_config.get("indent_size", 2),
        )

        if args.output_dir:
            # Generate output filename
            program_name = output.get("program_name", "UNKNOWN")
            output_filename = args.output_filename.format(program_name=program_name)
            output_path = args.output_dir / output_filename
            writer.write(output, output_path)
            if not args.quiet:
                print(f"Analysis written to: {output_path}")
                print(f"Execution time: {execution_time:.4f} seconds")
        else:
            print(writer.write(output))

        return 0

    except ParseError as e:
        logger.error(f"Parse error: {e}")
        return 1
    except Exception as e:
        logger.exception(f"Analysis failed: {e}")
        return 1


def handle_filter_by_variable(args) -> int:
    """Handle the filter-by-variable subcommand.

    Args:
        args: Parsed arguments

    Returns:
        Exit code
    """
    logger = logging.getLogger(__name__)

    # Validate input JSON file
    if not args.input_json.exists():
        logger.error(f"Input JSON file not found: {args.input_json}")
        return 1

    if not args.input_json.is_file():
        logger.error(f"Input path is not a file: {args.input_json}")
        return 1

    # Get variable names
    variable_names = []

    if args.variables:
        variable_names = args.variables
    elif args.variables_file:
        if not args.variables_file.exists():
            logger.error(f"Variables file not found: {args.variables_file}")
            return 1
        try:
            with open(args.variables_file, "r") as f:
                variable_names = [line.strip() for line in f if line.strip()]
        except Exception as e:
            logger.error(f"Failed to read variables file: {e}")
            return 1

    if not variable_names:
        logger.error("At least one variable name is required")
        return 1

    # Create output directory if specified
    if args.output_dir:
        try:
            args.output_dir.mkdir(parents=True, exist_ok=True)
            logger.info(f"Output directory: {args.output_dir}")
        except OSError as e:
            logger.error(f"Failed to create output directory: {e}")
            return 1

    try:
        # Load input JSON
        with open(args.input_json, "r") as f:
            analysis_data = json.load(f)

        # Validate JSON structure
        if "sections_and_paragraphs" not in analysis_data:
            logger.error("Invalid analysis JSON: missing 'sections_and_paragraphs' field")
            return 1

        # Create filter and execute
        var_filter = VariableFilter(analysis_data)
        include_redefines = not args.no_redefines
        include_ancestor_mods = not args.no_ancestor_mods
        result = var_filter.filter(
            variable_names,
            include_redefines=include_redefines,
            include_ancestor_mods=include_ancestor_mods
        )

        # Log warnings for variables not found
        not_found = result.get("summary", {}).get("variables_not_found", [])
        for var in not_found:
            logger.warning(f"Variable not found in analysis: {var}")

        # Output
        writer = JSONWriter(pretty_print=True, indent=2)

        if args.output_dir:
            # Generate output filename
            program_name = result.get("program_name", "UNKNOWN")
            output_filename = args.output_filename.format(program_name=program_name)
            output_path = args.output_dir / output_filename
            writer.write(result, output_path)
            if not args.quiet:
                print(f"Filter output written to: {output_path}")
                print(f"Variables requested: {len(variable_names)}")
                print(f"Variables found: {result['summary']['variables_found']}")
                if not_found:
                    print(f"Variables not found: {', '.join(not_found)}")
        else:
            print(writer.write(result))

        return 0

    except json.JSONDecodeError as e:
        logger.error(f"Invalid JSON in input file: {e}")
        return 1
    except Exception as e:
        logger.exception(f"Filter operation failed: {e}")
        return 1


def handle_analyze_and_filter(args) -> int:
    """Handle the analyze-and-filter subcommand.

    Combines analyze and filter-by-variable in a single operation.

    Args:
        args: Parsed arguments

    Returns:
        Exit code
    """
    logger = logging.getLogger(__name__)

    # Validate input
    if not args.source.exists():
        logger.error(f"Source file not found: {args.source}")
        return 1

    if not args.source.is_file():
        logger.error(f"Source path is not a file: {args.source}")
        return 1

    # Get variable names
    variable_names = []

    if args.variables:
        variable_names = args.variables
    elif args.variables_file:
        if not args.variables_file.exists():
            logger.error(f"Variables file not found: {args.variables_file}")
            return 1
        try:
            with open(args.variables_file, "r") as f:
                variable_names = [line.strip() for line in f if line.strip()]
        except Exception as e:
            logger.error(f"Failed to read variables file: {e}")
            return 1

    if not variable_names:
        logger.error("At least one variable name is required")
        return 1

    # Create output directory if specified and doesn't exist
    if args.output_dir:
        try:
            args.output_dir.mkdir(parents=True, exist_ok=True)
            logger.info(f"Output directory: {args.output_dir}")
        except OSError as e:
            logger.error(f"Failed to create output directory: {e}")
            return 1

    # Load config
    config = load_config(args.config)

    try:
        # Step 1: Analyze the COBOL source
        logger.info("Step 1: Analyzing COBOL source...")
        analysis_output = analyze_cobol_file(
            source_path=args.source,
            copybook_paths=args.copybook_paths,
            resolve_copies=not args.no_copy_resolution,
            config=config,
            include_source_info=args.include_source_info,
        )

        analysis_execution_time = analysis_output.get("execution_time_seconds", 0)
        program_name = analysis_output.get("program_name", "UNKNOWN")

        # Write analysis output if output directory specified
        output_config = config.get("output", {})
        writer = JSONWriter(
            pretty_print=output_config.get("pretty_print", True),
            indent=output_config.get("indent_size", 2),
        )

        analysis_output_path = None
        if args.output_dir:
            analysis_filename = args.analysis_filename.format(program_name=program_name)
            analysis_output_path = args.output_dir / analysis_filename
            writer.write(analysis_output, analysis_output_path)
            logger.info(f"Analysis output written to: {analysis_output_path}")

        # Step 2: Filter by variables
        logger.info("Step 2: Filtering by variables...")
        var_filter = VariableFilter(analysis_output)
        include_redefines = not args.no_redefines
        include_ancestor_mods = not args.no_ancestor_mods
        filter_result = var_filter.filter(
            variable_names,
            include_redefines=include_redefines,
            include_ancestor_mods=include_ancestor_mods
        )

        filter_execution_time = filter_result.get("execution_time_seconds", 0)

        # Log warnings for variables not found
        not_found = filter_result.get("summary", {}).get("variables_not_found", [])
        for var in not_found:
            logger.warning(f"Variable not found in analysis: {var}")

        # Write filter output
        filter_output_path = None
        if args.output_dir:
            filter_filename = args.filter_filename.format(program_name=program_name)
            filter_output_path = args.output_dir / filter_filename
            writer.write(filter_result, filter_output_path)
            logger.info(f"Filter output written to: {filter_output_path}")

        # Print summary
        if not args.quiet:
            if args.output_dir:
                print(f"Analysis written to: {analysis_output_path}")
                print(f"Filter output written to: {filter_output_path}")
            print(f"Analysis execution time: {analysis_execution_time:.4f} seconds")
            print(f"Filter execution time: {filter_execution_time:.4f} seconds")
            print(f"Total execution time: {analysis_execution_time + filter_execution_time:.4f} seconds")
            print(f"Variables requested: {len(variable_names)}")
            print(f"Variables found: {filter_result['summary']['variables_found']}")
            if not_found:
                print(f"Variables not found: {', '.join(not_found)}")

        # If no output directory, print filter result to stdout
        if not args.output_dir:
            print(writer.write(filter_result))

        return 0

    except ParseError as e:
        logger.error(f"Parse error: {e}")
        return 1
    except Exception as e:
        logger.exception(f"Analysis and filter failed: {e}")
        return 1


def create_analyze_parser(subparsers):
    """Create the analyze subcommand parser.

    Args:
        subparsers: Subparsers object from main parser

    Returns:
        The analyze subparser
    """
    analyze_parser = subparsers.add_parser(
        "analyze",
        help="Analyze COBOL source file",
        description="Analyze a COBOL source file to identify variable modifications and their impact on data structures.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s source.cob -o ./output
  %(prog)s source.cob -o ./output --compact
  %(prog)s source.cob -o ./output -c copybooks/ -c shared/
  %(prog)s source.cob -o ./output --no-copy-resolution
  %(prog)s source.cob --summary-only
        """,
    )

    # Required arguments
    analyze_parser.add_argument(
        "source",
        type=Path,
        help="Path to the COBOL source file to analyze",
    )

    # Output options
    output_group = analyze_parser.add_argument_group("Output Options")
    output_group.add_argument(
        "-o", "--output-dir",
        type=Path,
        dest="output_dir",
        help="Output directory for analysis results (created if it doesn't exist)",
    )
    output_group.add_argument(
        "--output-filename",
        type=str,
        default="{program_name}-analysis.json",
        help="Output filename pattern. Use {program_name} as placeholder (default: {program_name}-analysis.json)",
    )
    output_group.add_argument(
        "--compact",
        action="store_true",
        help="Generate compact output (unique variables per section, no duplicates)",
    )
    output_group.add_argument(
        "--summary-only",
        action="store_true",
        help="Output only the summary section (minimal output)",
    )
    output_group.add_argument(
        "--include-source-info",
        action="store_true",
        help="Include source file metadata in output (path, format, line count)",
    )

    # Copybook options
    copybook_group = analyze_parser.add_argument_group("Copybook Options")
    copybook_group.add_argument(
        "-c", "--copybook-path",
        type=Path,
        action="append",
        dest="copybook_paths",
        metavar="PATH",
        help="Path to search for copybooks (can be specified multiple times)",
    )
    copybook_group.add_argument(
        "--no-copy-resolution",
        action="store_true",
        help="Skip COPY statement resolution",
    )

    # Configuration options
    config_group = analyze_parser.add_argument_group("Configuration")
    config_group.add_argument(
        "--config",
        type=Path,
        metavar="FILE",
        help="Path to YAML configuration file",
    )

    # Logging options
    logging_group = analyze_parser.add_argument_group("Logging")
    logging_group.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose output (debug level logging)",
    )
    logging_group.add_argument(
        "-q", "--quiet",
        action="store_true",
        help="Suppress all output except errors",
    )

    analyze_parser.set_defaults(func=handle_analyze)
    return analyze_parser


def create_filter_parser(subparsers):
    """Create the filter-by-variable subcommand parser.

    Args:
        subparsers: Subparsers object from main parser

    Returns:
        The filter-by-variable subparser
    """
    filter_parser = subparsers.add_parser(
        "filter-by-variable",
        help="Filter analysis output by variable names",
        description="Transform section-centric analysis output into a variable-centric view.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s analysis.json -v WS-TOTAL CUST-BALANCE
  %(prog)s analysis.json --variables-file vars.txt
  %(prog)s analysis.json -v WS-TOTAL -o ./filtered
  %(prog)s analysis.json -v WS-TOTAL --no-redefines
        """,
    )

    # Required arguments
    filter_parser.add_argument(
        "input_json",
        type=Path,
        help="Path to JSON output from 'analyze' command",
    )

    # Variable specification (mutually exclusive)
    var_group = filter_parser.add_argument_group("Variable Specification (one required)")
    var_exclusive = var_group.add_mutually_exclusive_group(required=True)
    var_exclusive.add_argument(
        "-v", "--variables",
        type=str,
        nargs="+",
        metavar="VAR",
        help="Variable names to filter (space-separated)",
    )
    var_exclusive.add_argument(
        "--variables-file",
        type=Path,
        metavar="FILE",
        help="File containing variable names (one per line)",
    )

    # Output options
    output_group = filter_parser.add_argument_group("Output Options")
    output_group.add_argument(
        "-o", "--output-dir",
        type=Path,
        dest="output_dir",
        help="Output directory for filtered results (default: stdout)",
    )
    output_group.add_argument(
        "--output-filename",
        type=str,
        default="{program_name}-variable-filter.json",
        help="Output filename pattern (default: {program_name}-variable-filter.json)",
    )
    output_group.add_argument(
        "--no-redefines",
        action="store_true",
        help="Exclude REDEFINES-related modifications",
    )
    output_group.add_argument(
        "--no-ancestor-mods",
        action="store_true",
        help="Exclude ancestor group modifications from filter output",
    )

    # Logging options
    logging_group = filter_parser.add_argument_group("Logging")
    logging_group.add_argument(
        "-q", "--quiet",
        action="store_true",
        help="Suppress all output except errors",
    )

    filter_parser.set_defaults(func=handle_filter_by_variable, verbose=False)
    return filter_parser


def create_analyze_and_filter_parser(subparsers):
    """Create the analyze-and-filter subcommand parser.

    Args:
        subparsers: Subparsers object from main parser

    Returns:
        The analyze-and-filter subparser
    """
    parser = subparsers.add_parser(
        "analyze-and-filter",
        help="Analyze COBOL source and filter by variables in one step",
        description="Analyze a COBOL source file and immediately filter the results by variable names. Produces both the full analysis JSON and the variable-filtered JSON.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s source.cob -v WS-TOTAL CUST-BALANCE -o ./output
  %(prog)s source.cob --variables-file vars.txt -o ./output
  %(prog)s source.cob -v WS-TOTAL --no-redefines -o ./output
  %(prog)s source.cob -v WS-TOTAL -c copybooks/ -o ./output
        """,
    )

    # Required arguments
    parser.add_argument(
        "source",
        type=Path,
        help="Path to the COBOL source file to analyze",
    )

    # Variable specification (mutually exclusive, required)
    var_group = parser.add_argument_group("Variable Specification (one required)")
    var_exclusive = var_group.add_mutually_exclusive_group(required=True)
    var_exclusive.add_argument(
        "-v", "--variables",
        type=str,
        nargs="+",
        metavar="VAR",
        help="Variable names to filter (space-separated)",
    )
    var_exclusive.add_argument(
        "--variables-file",
        type=Path,
        metavar="FILE",
        help="File containing variable names (one per line)",
    )

    # Output options
    output_group = parser.add_argument_group("Output Options")
    output_group.add_argument(
        "-o", "--output-dir",
        type=Path,
        dest="output_dir",
        help="Output directory for both JSON files (created if it doesn't exist)",
    )
    output_group.add_argument(
        "--analysis-filename",
        type=str,
        default="{program_name}-analysis.json",
        help="Analysis output filename pattern (default: {program_name}-analysis.json)",
    )
    output_group.add_argument(
        "--filter-filename",
        type=str,
        default="{program_name}-variable-filter.json",
        help="Filter output filename pattern (default: {program_name}-variable-filter.json)",
    )
    output_group.add_argument(
        "--no-redefines",
        action="store_true",
        help="Exclude REDEFINES-related modifications from filter output",
    )
    output_group.add_argument(
        "--no-ancestor-mods",
        action="store_true",
        help="Exclude ancestor group modifications from filter output",
    )
    output_group.add_argument(
        "--include-source-info",
        action="store_true",
        help="Include source file metadata in analysis output",
    )

    # Copybook options
    copybook_group = parser.add_argument_group("Copybook Options")
    copybook_group.add_argument(
        "-c", "--copybook-path",
        type=Path,
        action="append",
        dest="copybook_paths",
        metavar="PATH",
        help="Path to search for copybooks (can be specified multiple times)",
    )
    copybook_group.add_argument(
        "--no-copy-resolution",
        action="store_true",
        help="Skip COPY statement resolution",
    )

    # Configuration options
    config_group = parser.add_argument_group("Configuration")
    config_group.add_argument(
        "--config",
        type=Path,
        metavar="FILE",
        help="Path to YAML configuration file",
    )

    # Logging options
    logging_group = parser.add_argument_group("Logging")
    logging_group.add_argument(
        "-V", "--verbose",
        action="store_true",
        dest="verbose",
        help="Enable verbose output (debug level logging)",
    )
    logging_group.add_argument(
        "-q", "--quiet",
        action="store_true",
        help="Suppress all output except errors",
    )

    parser.set_defaults(func=handle_analyze_and_filter)
    return parser


def main():
    """Main CLI entry point."""
    # Backwards compatibility: if first arg is not a subcommand, assume 'analyze'
    subcommands = ['analyze', 'filter-by-variable', 'analyze-and-filter']
    if len(sys.argv) > 1 and sys.argv[1] not in subcommands + ['-h', '--help', '--version']:
        sys.argv.insert(1, 'analyze')

    parser = argparse.ArgumentParser(
        prog="cobol-analyzer",
        description="COBOL Source Code Analyzer - Analyzes COBOL programs to identify variable modifications and their impact on data structures.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Commands:
  analyze             Analyze COBOL source file (default)
  filter-by-variable  Filter analysis output by variable names
  analyze-and-filter  Analyze and filter in one step (produces both JSON files)

Examples:
  %(prog)s analyze source.cob -o ./output
  %(prog)s source.cob -o ./output  # backwards compatible
  %(prog)s filter-by-variable analysis.json -v WS-TOTAL
  %(prog)s analyze-and-filter source.cob -v WS-TOTAL -o ./output

For more information on a command, use: %(prog)s <command> --help
        """,
    )

    # Version
    parser.add_argument(
        "--version",
        action="version",
        version=f"%(prog)s {__version__}",
    )

    # Create subparsers
    subparsers = parser.add_subparsers(
        title="commands",
        dest="command",
        metavar="<command>",
    )

    # Add subcommands
    create_analyze_parser(subparsers)
    create_filter_parser(subparsers)
    create_analyze_and_filter_parser(subparsers)

    args = parser.parse_args()

    # Show help if no command specified
    if not args.command:
        parser.print_help()
        sys.exit(0)

    # Validate conflicting options for commands with verbose option
    if args.command in ("analyze", "analyze-and-filter") and getattr(args, 'verbose', False) and args.quiet:
        parser.error("--verbose and --quiet cannot be used together")

    # Setup logging
    log_level = "DEBUG" if getattr(args, 'verbose', False) else "INFO"
    setup_logging(log_level, quiet=getattr(args, 'quiet', False))

    # Execute the appropriate handler
    sys.exit(args.func(args))


if __name__ == "__main__":
    main()
