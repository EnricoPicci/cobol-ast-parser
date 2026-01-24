"""Main entry point for COBOL AST Parser.

This module provides the CLI interface for analyzing COBOL programs.
"""

import argparse
import sys
import logging
from pathlib import Path
from typing import Optional, List

import yaml

from preprocessor import CopyResolver, SourceFormat, detect_format, normalize_source
from parser import CobolParser, ParseError
from cobol_ast import ASTBuilder
from analyzers import ImpactAnalyzer
from output import JSONWriter


def setup_logging(level: str = "INFO") -> None:
    """Configure logging.

    Args:
        level: Logging level string (DEBUG, INFO, WARNING, ERROR)
    """
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
) -> dict:
    """Analyze a COBOL source file.

    Args:
        source_path: Path to COBOL source file
        copybook_paths: Paths to search for copybooks
        resolve_copies: Whether to resolve COPY statements
        output_path: Optional path to write JSON output
        config: Configuration dictionary

    Returns:
        Analysis results dictionary
    """
    config = config or load_config()
    logger = logging.getLogger(__name__)

    # Read source file
    logger.info(f"Reading source file: {source_path}")
    source = source_path.read_text(encoding="utf-8", errors="replace")

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


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="COBOL Source Code Analyzer",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  cobol-analyzer source.cob
  cobol-analyzer source.cob -o analysis.json
  cobol-analyzer source.cob -c copybooks/ -o analysis.json
  cobol-analyzer source.cob --no-copy-resolution
        """,
    )

    parser.add_argument(
        "source",
        type=Path,
        help="Path to COBOL source file",
    )
    parser.add_argument(
        "-o", "--output",
        type=Path,
        help="Output JSON file path",
    )
    parser.add_argument(
        "-c", "--copybook-path",
        type=Path,
        action="append",
        dest="copybook_paths",
        help="Path to search for copybooks (can be specified multiple times)",
    )
    parser.add_argument(
        "--config",
        type=Path,
        help="Path to configuration file",
    )
    parser.add_argument(
        "--no-copy-resolution",
        action="store_true",
        help="Skip COPY statement resolution",
    )
    parser.add_argument(
        "--compact",
        action="store_true",
        help="Generate compact output (unique variables per section)",
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose output",
    )
    parser.add_argument(
        "--summary-only",
        action="store_true",
        help="Output only the summary section",
    )

    args = parser.parse_args()

    # Setup logging
    log_level = "DEBUG" if args.verbose else "INFO"
    setup_logging(log_level)
    logger = logging.getLogger(__name__)

    # Validate input
    if not args.source.exists():
        logger.error(f"Source file not found: {args.source}")
        sys.exit(1)

    # Load config
    config = load_config(args.config)

    try:
        # Analyze
        output = analyze_cobol_file(
            source_path=args.source,
            copybook_paths=args.copybook_paths,
            resolve_copies=not args.no_copy_resolution,
            config=config,
        )

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
                "program_name": output.get("program_name", "UNKNOWN"),
                "sections_and_paragraphs": compact_sections,
            }

        # Handle summary only
        if args.summary_only:
            output = {
                "program_name": output.get("program_name", "UNKNOWN"),
                "summary": output.get("summary", {}),
            }

        # Output
        output_config = config.get("output", {})
        writer = JSONWriter(
            pretty_print=output_config.get("pretty_print", True),
            indent=output_config.get("indent_size", 2),
        )

        if args.output:
            writer.write(output, args.output)
            print(f"Analysis written to: {args.output}")
        else:
            print(writer.write(output))

    except ParseError as e:
        logger.error(f"Parse error: {e}")
        sys.exit(1)
    except Exception as e:
        logger.exception(f"Analysis failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
