"""Main entry point for COBOL AST Parser.

This module provides the CLI interface for analyzing COBOL programs
and mapping paragraphs to the variables they may modify.
"""

import argparse
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
from output import JSONWriter, ParagraphVariablesMapper

__version__ = "2.0.0"


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


def handle_paragraph_variables_map(args) -> int:
    """Handle the paragraph-variables-map subcommand.

    Analyzes a COBOL source file and generates a paragraph-centric view
    showing which variables may change within each SECTION or PARAGRAPH.

    Args:
        args: Parsed arguments

    Returns:
        Exit code (0 for success, 1 for failure)
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
        # Analyze the COBOL source
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

        # Generate paragraph-variables map
        mapper = ParagraphVariablesMapper(analysis_output)
        include_redefines = not args.no_redefines
        include_ancestor_mods = not args.no_ancestor_mods
        map_result = mapper.map(
            include_redefines=include_redefines,
            include_ancestor_mods=include_ancestor_mods
        )

        map_execution_time = map_result.get("execution_time_seconds", 0)
        total_execution_time = analysis_execution_time + map_execution_time

        # Write map output
        map_output_path = None
        if args.output_dir:
            map_filename = args.output_filename.format(program_name=program_name)
            map_output_path = args.output_dir / map_filename
            writer.write(map_result, map_output_path)

        # Print summary
        if not args.quiet:
            if args.output_dir:
                print(f"Analysis written to: {analysis_output_path}")
                print(f"Paragraph variables map written to: {map_output_path}")
                print(f"Execution time: {total_execution_time:.4f} seconds")

        # If no output directory, print map result to stdout
        if not args.output_dir:
            print(writer.write(map_result))

        return 0

    except ParseError as e:
        logger.error(f"Parse error: {e}")
        return 1
    except Exception as e:
        logger.exception(f"Paragraph variables map operation failed: {e}")
        return 1


def create_paragraph_variables_map_parser(subparsers):
    """Create the paragraph-variables-map subcommand parser.

    Args:
        subparsers: Subparsers object from main parser

    Returns:
        The paragraph-variables-map subparser
    """
    parser = subparsers.add_parser(
        "paragraph-variables-map",
        help="Analyze COBOL source and map paragraphs to changed variables",
        description="Analyze a COBOL source file and generate a paragraph-centric view showing which variables may change within each SECTION or PARAGRAPH, with Level 01 record membership including REDEFINES resolution. Produces both the full analysis JSON and the paragraph-variables map JSON.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s source.cob -o ./output
  %(prog)s source.cob -o ./output --no-redefines
  %(prog)s source.cob -o ./output --no-ancestor-mods
  %(prog)s source.cob -o ./output -c copybooks/
        """,
    )

    # Required arguments
    parser.add_argument(
        "source",
        type=Path,
        help="Path to the COBOL source file to analyze",
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
        "--output-filename",
        type=str,
        default="{program_name}-paragraph-variables.json",
        help="Paragraph variables map filename pattern (default: {program_name}-paragraph-variables.json)",
    )
    output_group.add_argument(
        "--no-redefines",
        action="store_true",
        help="Exclude REDEFINES-affected variables from map output",
    )
    output_group.add_argument(
        "--no-ancestor-mods",
        action="store_true",
        help="Exclude ancestor-modified variables from map output",
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
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose output (debug level logging)",
    )
    logging_group.add_argument(
        "-q", "--quiet",
        action="store_true",
        help="Suppress all output except errors",
    )

    parser.set_defaults(func=handle_paragraph_variables_map)
    return parser


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        prog="cobol-analyzer",
        description="COBOL Paragraph Variables Mapper - Analyzes COBOL programs to map paragraphs/sections to the variables they may modify.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s paragraph-variables-map source.cob -o ./output
  %(prog)s paragraph-variables-map source.cob -o ./output -c copybooks/
  %(prog)s paragraph-variables-map source.cob -o ./output --no-redefines

For more information: %(prog)s paragraph-variables-map --help
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

    # Add the only subcommand
    create_paragraph_variables_map_parser(subparsers)

    args = parser.parse_args()

    # Show help if no command specified
    if not args.command:
        parser.print_help()
        sys.exit(0)

    # Validate conflicting options
    if getattr(args, 'verbose', False) and args.quiet:
        parser.error("--verbose and --quiet cannot be used together")

    # Setup logging
    log_level = "DEBUG" if getattr(args, 'verbose', False) else "INFO"
    setup_logging(log_level, quiet=getattr(args, 'quiet', False))

    # Execute the appropriate handler
    sys.exit(args.func(args))


if __name__ == "__main__":
    main()
