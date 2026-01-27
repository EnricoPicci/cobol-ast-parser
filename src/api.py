"""Public API for COBOL paragraph-variables analysis.

This module provides the programmatic interface for analyzing COBOL programs.
Use these functions instead of calling CLI internals directly.

Example:
    from api import analyze_paragraph_variables, AnalysisOptions

    result = analyze_paragraph_variables(
        source_path=Path("program.cob"),
        options=AnalysisOptions(
            copybook_paths=[Path("./copybooks")],
        ),
    )

    # Access both JSON outputs (same as CLI produces)
    print(result.analysis)             # Full analysis JSON
    print(result.paragraph_variables)  # Paragraph-variables map JSON
"""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, List, Dict, Any


class AnalysisError(Exception):
    """Raised when COBOL analysis fails."""
    pass


@dataclass
class AnalysisOptions:
    """Options for COBOL analysis.

    Attributes:
        copybook_paths: Paths to search for copybooks (in addition to source directory)
        resolve_copies: Whether to resolve COPY statements (default: True)
        include_redefines: Include REDEFINES-affected variables in output (default: True)
        include_ancestor_mods: Include ancestor-modified variables in output (default: True)
        include_source_info: Include source file metadata in output (default: False)
    """
    copybook_paths: Optional[List[Path]] = None
    resolve_copies: bool = True
    include_redefines: bool = True
    include_ancestor_mods: bool = True
    include_source_info: bool = False


@dataclass
class AnalysisResult:
    """Result of paragraph-variables analysis.

    Contains both JSON outputs that the CLI command generates:
    - analysis: Full analysis output ({program_name}-analysis.json)
    - paragraph_variables: Mapped view ({program_name}-paragraph-variables.json)

    Attributes:
        program_name: Name of the analyzed COBOL program
        analysis: Full analysis dict (same as {program_name}-analysis.json)
        paragraph_variables: Mapped dict (same as {program_name}-paragraph-variables.json)
        execution_time_seconds: Total execution time for both analysis and mapping
        source_info: Source file metadata (if include_source_info was True)
    """
    program_name: str
    analysis: Dict[str, Any]
    paragraph_variables: Dict[str, Any]
    execution_time_seconds: float
    source_info: Optional[Dict[str, Any]] = None


def analyze_paragraph_variables(
    source_path: Path,
    options: Optional[AnalysisOptions] = None,
) -> AnalysisResult:
    """Analyze a COBOL source file and return paragraph-variables mapping.

    This is the main entry point for programmatic use. Returns both JSON
    outputs that the CLI command would generate.

    Args:
        source_path: Path to the COBOL source file
        options: Analysis options (uses defaults if not provided)

    Returns:
        AnalysisResult containing both:
        - analysis: Full analysis dict (same as {program_name}-analysis.json)
        - paragraph_variables: Mapped dict (same as {program_name}-paragraph-variables.json)

    Raises:
        FileNotFoundError: If source file doesn't exist
        ParseError: If COBOL source cannot be parsed
        AnalysisError: If analysis fails for other reasons

    Example:
        >>> from api import analyze_paragraph_variables, AnalysisOptions
        >>> from pathlib import Path
        >>>
        >>> # Simple usage with defaults
        >>> result = analyze_paragraph_variables(Path("myprogram.cob"))
        >>> print(result.program_name)
        >>> print(result.paragraph_variables)
        >>>
        >>> # With custom options
        >>> options = AnalysisOptions(
        ...     copybook_paths=[Path("./copybooks")],
        ...     include_redefines=False,
        ...     include_source_info=True,
        ... )
        >>> result = analyze_paragraph_variables(Path("myprogram.cob"), options)
    """
    # Import here to avoid circular imports and keep module lightweight
    import time
    from preprocessor import CopyResolver, detect_format, normalize_source
    from parser import CobolParser, ParseError
    from cobol_ast import ASTBuilder
    from analyzers import ImpactAnalyzer
    from output import ParagraphVariablesMapper

    # Use defaults if no options provided
    if options is None:
        options = AnalysisOptions()

    # Validate input
    if not source_path.exists():
        raise FileNotFoundError(f"Source file not found: {source_path}")

    if not source_path.is_file():
        raise FileNotFoundError(f"Source path is not a file: {source_path}")

    start_time = time.perf_counter()

    try:
        # Read source file
        source = source_path.read_text(encoding="utf-8", errors="replace")
        source_lines_count = len(source.splitlines())

        # Detect format
        source_lines = source.splitlines()
        source_format = detect_format(source_lines)

        # Resolve COPY statements
        line_mapping = None
        original_line_count = source_lines_count
        if options.resolve_copies:
            copy_paths = options.copybook_paths or []
            copy_paths = [source_path.parent] + list(copy_paths)

            resolver = CopyResolver(copy_paths)
            try:
                source = resolver.resolve(source, source_path.name)
                line_mapping = resolver.line_mapping
                original_line_count = resolver.original_line_count
            except Exception as e:
                # Log warning but continue - COPY resolution failure is not fatal
                pass

        # Normalize source
        source = normalize_source(source, source_format)

        # Parse
        parser = CobolParser(use_generated=False)
        try:
            parse_tree = parser.parse(source)
        except ParseError:
            raise  # Re-raise ParseError as-is

        # Build AST
        builder = ASTBuilder()
        program = builder.build(parse_tree)

        # Analyze
        analyzer = ImpactAnalyzer(program)
        analyzer.analyze()

        # Generate analysis output
        analysis_output = analyzer.generate_output()

        # Add source file info if requested
        if options.include_source_info:
            analysis_output["source_info"] = {
                "file_path": str(source_path.absolute()),
                "file_name": source_path.name,
                "source_format": source_format.value,
                "lines_count": source_lines_count,
            }

        # Add line mapping for converting expanded line numbers to original
        if line_mapping:
            analysis_output["_line_mapping"] = {
                str(k): {
                    "original_line": v.original_line,
                    "source_file": v.source_file,
                    "is_copybook": v.is_copybook,
                }
                for k, v in line_mapping.items()
            }
            analysis_output["_original_line_count"] = original_line_count

        # Generate paragraph-variables map
        mapper = ParagraphVariablesMapper(analysis_output)
        paragraph_variables_output = mapper.map(
            include_redefines=options.include_redefines,
            include_ancestor_mods=options.include_ancestor_mods
        )

        # Add source_info to paragraph_variables output if requested
        if options.include_source_info and "source_info" in analysis_output:
            paragraph_variables_output["source_info"] = analysis_output["source_info"]

        # Calculate total execution time
        end_time = time.perf_counter()
        total_execution_time = end_time - start_time

        # Add execution time to analysis output
        analysis_output["execution_time_seconds"] = round(total_execution_time, 4)

        return AnalysisResult(
            program_name=program.name,
            analysis=analysis_output,
            paragraph_variables=paragraph_variables_output,
            execution_time_seconds=round(total_execution_time, 4),
            source_info=analysis_output.get("source_info"),
        )

    except ParseError:
        raise  # Re-raise ParseError as-is
    except FileNotFoundError:
        raise  # Re-raise FileNotFoundError as-is
    except Exception as e:
        raise AnalysisError(f"Analysis failed: {e}") from e
