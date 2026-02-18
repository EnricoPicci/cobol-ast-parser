"""Main entry point for COBOL AST Parser.

This module provides the CLI interface for analyzing COBOL programs
and mapping paragraphs to the variables they may modify.
"""

import argparse
import sys
import logging
from pathlib import Path
from typing import Any, Dict, Optional

import yaml  # type: ignore[import-untyped]

from parser import ParseError
from output import JSONWriter
from cobol_ast import __version__
from cobol_ast.api import analyze_paragraph_variables, AnalysisOptions, AnalysisError


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
    default_config: Dict[str, Any] = {
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
                    if isinstance(value, dict) and isinstance(default_config.get(key), dict):
                        default_config[key].update(value)
                    else:
                        default_config[key] = value

    return default_config


def handle_paragraph_variables_map(args) -> int:
    """Handle the paragraph-variables-map subcommand.

    Analyzes a COBOL source file and generates a paragraph-centric view
    showing which variables may change within each SECTION or PARAGRAPH.

    This is a thin CLI wrapper around the public API (analyze_paragraph_variables).

    Args:
        args: Parsed arguments

    Returns:
        Exit code (0 for success, 1 for failure)
    """
    logger = logging.getLogger(__name__)

    # Create output directory if specified and doesn't exist
    if args.output_dir:
        try:
            args.output_dir.mkdir(parents=True, exist_ok=True)
            logger.info(f"Output directory: {args.output_dir}")
        except OSError as e:
            logger.error(f"Failed to create output directory: {e}")
            return 1

    # Load config for output formatting
    config = load_config(args.config)

    try:
        # Build options from CLI arguments
        options = AnalysisOptions(
            copybook_paths=args.copybook_paths,
            resolve_copies=not args.no_copy_resolution,
            include_redefines=not args.no_redefines,
            include_ancestor_mods=not args.no_ancestor_mods,
            include_source_info=args.include_source_info,
        )

        # Call the public API
        result = analyze_paragraph_variables(args.source, options)

        # Setup JSON writer from config
        output_config = config.get("output", {})
        writer = JSONWriter(
            pretty_print=output_config.get("pretty_print", True),
            indent=output_config.get("indent_size", 2),
        )

        # Write analysis output if output directory specified
        analysis_output_path = None
        if args.output_dir:
            analysis_filename = args.analysis_filename.format(program_name=result.program_name)
            analysis_output_path = args.output_dir / analysis_filename
            writer.write(result.analysis, analysis_output_path)

        # Write paragraph-variables map output
        map_output_path = None
        if args.output_dir:
            map_filename = args.output_filename.format(program_name=result.program_name)
            map_output_path = args.output_dir / map_filename
            writer.write(result.paragraph_variables, map_output_path)

        # Print summary
        if not args.quiet:
            if args.output_dir:
                print(f"Analysis written to: {analysis_output_path}")
                print(f"Paragraph variables map written to: {map_output_path}")
                print(f"Execution time: {result.execution_time_seconds:.4f} seconds")

        # If no output directory, print paragraph-variables map to stdout
        if not args.output_dir:
            print(writer.write(result.paragraph_variables))

        return 0

    except FileNotFoundError as e:
        logger.error(str(e))
        return 1
    except ParseError as e:
        logger.error(f"Parse error: {e}")
        return 1
    except AnalysisError as e:
        logger.error(str(e))
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
