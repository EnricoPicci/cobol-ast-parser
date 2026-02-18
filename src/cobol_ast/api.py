"""Public API for COBOL paragraph-variables analysis.

This module provides the programmatic interface for analyzing COBOL programs.
Use these functions instead of calling CLI internals directly.

Example:
    from cobol_ast import analyze_paragraph_variables, AnalysisOptions

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
from typing import Optional, List, Dict, Any, Tuple, TYPE_CHECKING

if TYPE_CHECKING:
    from .nodes import DataItem
    from analyzers.data_analyzer import MemoryRegion


def _convert_to_original_line(
    expanded_line: int,
    line_mapping: Dict[int, Any],
    original_line_count: int,
) -> int:
    """Convert an expanded line number to the original source line number.

    When COPY statements are resolved, line numbers in the expanded source
    may not match the original source file. This function converts expanded
    line numbers back to original line numbers.

    Args:
        expanded_line: Line number in the expanded (COPY-resolved) source
        line_mapping: Dictionary mapping expanded line numbers to original info
        original_line_count: Number of lines in the original source file

    Returns:
        Original line number in the source file
    """
    if not line_mapping:
        return expanded_line

    # Look up the mapping
    mapping_info = line_mapping.get(expanded_line)
    if mapping_info:
        return mapping_info.original_line

    # Fallback: if line is beyond original count, it's likely from copybook
    # Try to find the nearest mapped line going backwards
    if original_line_count > 0 and expanded_line > original_line_count:
        for line_num in range(expanded_line, 0, -1):
            mapping_info = line_mapping.get(line_num)
            if mapping_info and not mapping_info.is_copybook:
                return mapping_info.original_line

    return expanded_line


def _apply_line_mapping_to_analysis(
    analysis_output: Dict[str, Any],
    line_mapping: Dict[int, Any],
    original_line_count: int,
) -> None:
    """Apply line mapping conversion to analysis output in-place.

    Converts expanded line numbers to original line numbers for:
    - "line_number" in sections_and_paragraphs modifications
    - "definition_line" in memory_regions

    Args:
        analysis_output: The analysis output dictionary to modify
        line_mapping: Dictionary mapping expanded line numbers to original info
        original_line_count: Number of lines in the original source file
    """
    if not line_mapping:
        return

    # Convert line_number in sections_and_paragraphs
    sections_and_paragraphs = analysis_output.get("sections_and_paragraphs", {})
    for section_name, modifications in sections_and_paragraphs.items():
        for mod in modifications:
            if "line_number" in mod:
                mod["line_number"] = _convert_to_original_line(
                    mod["line_number"], line_mapping, original_line_count
                )

    # Convert definition_line in memory_regions
    memory_regions = analysis_output.get("memory_regions", {})
    for var_name, region_info in memory_regions.items():
        if "definition_line" in region_info:
            region_info["definition_line"] = _convert_to_original_line(
                region_info["definition_line"], line_mapping, original_line_count
            )


class AnalysisError(Exception):
    """Raised when COBOL analysis fails."""
    pass


@dataclass
class _CopyResolutionOutput:
    """Internal result of copybook resolution orchestration."""
    resolved_source: str
    line_mapping: Dict[int, Any]
    original_line_count: int
    warnings: List[str]


def _resolve_copies(
    source: str,
    source_path: Path,
    copybook_paths: Optional[List[Path]] = None,
) -> _CopyResolutionOutput:
    """Single implementation of copybook resolution orchestration.

    Resolves COPY statements in the source using CopyResolver and returns
    the resolved source along with line mapping metadata.

    Args:
        source: The COBOL source text to resolve
        source_path: Path to the source file (used to derive default search dir)
        copybook_paths: Additional paths to search for copybooks

    Returns:
        _CopyResolutionOutput with resolved source, line mapping, and warnings
    """
    from preprocessor import CopyResolver

    copy_paths = list(copybook_paths or [])
    copy_paths.insert(0, source_path.parent)

    resolver = CopyResolver(copy_paths)
    resolved = resolver.resolve(source, source_path.name)

    return _CopyResolutionOutput(
        resolved_source=resolved,
        line_mapping=resolver.line_mapping,
        original_line_count=resolver.original_line_count,
        warnings=resolver.warnings,
    )


@dataclass
class CopybookResolutionOptions:
    """Options for copybook resolution.

    Attributes:
        copybook_paths: Additional paths to search for copybooks. The source file's
            directory is always searched by default.
    """
    copybook_paths: Optional[List[Path]] = None


@dataclass
class CopybookResolutionResult:
    """Result of copybook resolution.

    Attributes:
        resolved_source: COBOL source text with all COPY statements expanded
        line_mapping: Maps resolved line numbers to original source locations.
            Each key is a line number in the resolved source; each value is a
            LineMapping with original_line, source_file, and is_copybook fields.
        execution_time_seconds: Time taken for resolution
        warnings: Warning messages (e.g., copybooks not found)
    """
    resolved_source: str
    line_mapping: Dict[int, Any]
    execution_time_seconds: float
    warnings: List[str] = field(default_factory=list)


def resolve_copybooks(
    source_path: Path,
    options: Optional[CopybookResolutionOptions] = None,
) -> CopybookResolutionResult:
    """Resolve all COPY statements in a COBOL source file.

    Reads the source file, expands all COPY statements by inlining the
    referenced copybook content, and returns the resolved source text
    along with a line mapping that traces each line back to its origin.

    No format detection, normalization, or parsing is performed.

    Args:
        source_path: Path to the COBOL source file
        options: Resolution options (uses defaults if not provided)

    Returns:
        CopybookResolutionResult with resolved source and line mapping

    Raises:
        FileNotFoundError: If source file doesn't exist
        AnalysisError: If resolution fails for other reasons

    Example:
        >>> from cobol_ast import resolve_copybooks, CopybookResolutionOptions
        >>> from pathlib import Path
        >>>
        >>> result = resolve_copybooks(Path("program.cob"))
        >>> print(result.resolved_source)
        >>>
        >>> # With extra copybook search paths
        >>> options = CopybookResolutionOptions(
        ...     copybook_paths=[Path("./copybooks")],
        ... )
        >>> result = resolve_copybooks(Path("program.cob"), options)
        >>> for line_num, mapping in result.line_mapping.items():
        ...     print(f"Line {line_num}: from {mapping.source_file}")
    """
    import time

    if options is None:
        options = CopybookResolutionOptions()

    if not source_path.exists():
        raise FileNotFoundError(f"Source file not found: {source_path}")

    if not source_path.is_file():
        raise FileNotFoundError(f"Source path is not a file: {source_path}")

    start_time = time.perf_counter()

    try:
        source = source_path.read_text(encoding="utf-8", errors="replace")

        resolution = _resolve_copies(
            source, source_path, options.copybook_paths
        )

        end_time = time.perf_counter()

        return CopybookResolutionResult(
            resolved_source=resolution.resolved_source,
            line_mapping=resolution.line_mapping,
            execution_time_seconds=round(end_time - start_time, 4),
            warnings=resolution.warnings,
        )

    except FileNotFoundError:
        raise
    except Exception as e:
        raise AnalysisError(f"Copybook resolution failed: {e}") from e


@dataclass
class AnalysisOptions:
    """Options for COBOL analysis.

    Attributes:
        copybook_paths: Additional paths to search for copybooks. The source file's
            directory is always searched by default.
        resolve_copies: Whether to resolve COPY statements (default: True)
        include_redefines: Include REDEFINES-affected variables in output (default: True)
        include_ancestor_mods: Include ancestor-modified variables in output (default: True)
        include_source_info: Include source file metadata in output (default: True)
    """
    copybook_paths: Optional[List[Path]] = None
    resolve_copies: bool = True
    include_redefines: bool = True
    include_ancestor_mods: bool = True
    include_source_info: bool = True


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
        variable_index: Inverted index for linking DataDivisionTree nodes to paragraphs.
            Structure: {defined_in_record: {"start:end": {"variable_name": str, "modifying_paragraphs": [str], "accessing_paragraphs": [str]}}}
            Enables lookup: index[node.defined_in_record][f"{node.position.start}:{node.position.end}"]["modifying_paragraphs"]
        execution_time_seconds: Total execution time for both analysis and mapping
        source_info: Source file metadata (if include_source_info was True)
        warnings: List of warning messages from preprocessing (e.g., copybooks not found)
    """
    program_name: str
    analysis: Dict[str, Any]
    paragraph_variables: Dict[str, Any]
    variable_index: Dict[str, Dict[str, Dict[str, Any]]]
    execution_time_seconds: float
    source_info: Optional[Dict[str, Any]] = None
    warnings: List[str] = field(default_factory=list)


def _build_variable_index(
    paragraph_variables: Dict[str, Any]
) -> Dict[str, Dict[str, Dict[str, Any]]]:
    """Build an inverted index from paragraph_variables for DataDivisionTree linking.

    Creates an index structure that enables O(1) lookup from a DataDivisionTree node
    to the list of paragraphs that may modify or access that variable.

    Args:
        paragraph_variables: The paragraph_variables output from ParagraphVariablesMapper

    Returns:
        Nested dict with structure:
        {
            defined_in_record: {
                "start:end": {
                    "variable_name": str,
                    "modifying_paragraphs": [paragraph_name, ...],
                    "accessing_paragraphs": [paragraph_name, ...]
                }
            }
        }

    Example usage:
        >>> index = result.variable_index
        >>> node = selected_data_item_node  # from DataDivisionTree
        >>> key = f"{node.position['start']}:{node.position['end']}"
        >>> entry = index.get(node.defined_in_record, {}).get(key)
        >>> if entry:
        ...     modifying = entry["modifying_paragraphs"]
        ...     accessing = entry["accessing_paragraphs"]
    """
    index: Dict[str, Dict[str, Dict[str, Any]]] = {}

    paragraphs_data = paragraph_variables.get("paragraphs", {})

    for para_name, variables in paragraphs_data.items():
        for var_name, var_info in variables.items():
            # Get the defined_in_record - this now always uses the raw format
            # (e.g., FILLER$1) to match DataDivisionTree's defined_in_record
            defined_in_record = var_info.get("defined_in_record", "")

            # Get position - skip if no position info
            position = var_info.get("position")
            if not position:
                continue

            start = position.get("start")
            end = position.get("end")
            if start is None or end is None:
                continue

            # Build the position key
            pos_key = f"{start}:{end}"

            # Initialize nested dicts if needed
            if defined_in_record not in index:
                index[defined_in_record] = {}

            if pos_key not in index[defined_in_record]:
                index[defined_in_record][pos_key] = {
                    "variable_name": var_name,
                    "modifying_paragraphs": [],
                    "accessing_paragraphs": []
                }

            # Check if this variable is modified in this paragraph
            modification_lines = var_info.get("modification_lines", [])
            if modification_lines:
                # Add this paragraph to modifying list (avoid duplicates)
                if para_name not in index[defined_in_record][pos_key]["modifying_paragraphs"]:
                    index[defined_in_record][pos_key]["modifying_paragraphs"].append(para_name)

            # Check if this variable is accessed in this paragraph
            access_lines = var_info.get("access_lines", [])
            if access_lines:
                # Add this paragraph to accessing list (avoid duplicates)
                if para_name not in index[defined_in_record][pos_key]["accessing_paragraphs"]:
                    index[defined_in_record][pos_key]["accessing_paragraphs"].append(para_name)

    return index


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
        >>> from cobol_ast import analyze_paragraph_variables, AnalysisOptions
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
    from preprocessor import detect_format, normalize_source
    from parser import CobolParser, ParseError
    from . import ASTBuilder
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
        warnings: List[str] = []
        if options.resolve_copies:
            resolution = _resolve_copies(source, source_path, options.copybook_paths)
            source = resolution.resolved_source
            line_mapping = resolution.line_mapping
            original_line_count = resolution.original_line_count
            warnings = resolution.warnings

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

        # Convert expanded line numbers to original line numbers
        if line_mapping:
            _apply_line_mapping_to_analysis(
                analysis_output, line_mapping, original_line_count
            )

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

        # Build variable index for DataDivisionTree linking
        variable_index = _build_variable_index(paragraph_variables_output)

        # Calculate total execution time
        end_time = time.perf_counter()
        total_execution_time = end_time - start_time

        # Add execution time to analysis output
        analysis_output["execution_time_seconds"] = round(total_execution_time, 4)

        return AnalysisResult(
            program_name=program.name,
            analysis=analysis_output,
            paragraph_variables=paragraph_variables_output,
            variable_index=variable_index,
            execution_time_seconds=round(total_execution_time, 4),
            source_info=analysis_output.get("source_info"),
            warnings=warnings,
        )

    except ParseError:
        raise  # Re-raise ParseError as-is
    except FileNotFoundError:
        raise  # Re-raise FileNotFoundError as-is
    except Exception as e:
        raise AnalysisError(f"Analysis failed: {e}") from e


@dataclass
class TreeOptions:
    """Options for DATA DIVISION tree generation.

    Attributes:
        copybook_paths: Additional paths to search for copybooks. The source file's
            directory is always searched by default.
        resolve_copies: Whether to resolve COPY statements (default: True)
        include_filler: Include FILLER items in output (default: True)
        include_88_levels: Include 88-level condition names in output (default: True)
        include_source_info: Include source file metadata in output (default: True)
    """
    copybook_paths: Optional[List[Path]] = None
    resolve_copies: bool = True
    include_filler: bool = True
    include_88_levels: bool = True
    include_source_info: bool = True


@dataclass
class DataItemNode:
    """A node representing a COBOL data item in the tree structure.

    Attributes:
        name: Data item name (e.g., "WS-EMPLOYEE-RECORD")
        level: COBOL level number (01-49, 66, 77, 88)
        picture: PICTURE clause if present
        usage: USAGE clause if present
        value: VALUE clause if present
        occurs: OCCURS count if present
        occurs_depending_on: OCCURS DEPENDING ON variable name if present
        redefines: Name of item being redefined if REDEFINES clause present
        is_group: True if this is a group item (has non-88-level children)
        is_filler: True if this is a FILLER item
        line_number: Line number in source file (after COPY expansion)
        copybook_source: Name of copybook file if this item came from a COPY statement
        copybook: Name of copybook that provides the content for this item (for FILLER
            items whose children come from a COPY statement)
        position: Memory position info (start, end, size) if calculable
        defined_in_record: Name of the Level 01 record that contains this data item.
            This enables linking with paragraph_variables output for variable lookup.
            For Level 01 records, this equals their own name. For 77-level items,
            this also equals their own name as they are standalone.
        children: List of child DataItemNode objects
    """
    name: str
    level: int
    picture: Optional[str] = None
    usage: Optional[str] = None
    value: Optional[str] = None
    occurs: Optional[int] = None
    occurs_depending_on: Optional[str] = None
    redefines: Optional[str] = None
    is_group: bool = False
    is_filler: bool = False
    line_number: int = 0
    copybook_source: Optional[str] = None
    copybook: Optional[str] = None
    position: Optional[Dict[str, int]] = None
    defined_in_record: Optional[str] = None
    children: List["DataItemNode"] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        result: Dict[str, Any] = {
            "name": self.name,
            "level": self.level,
        }

        # Only include optional fields if they have values
        if self.picture is not None:
            result["picture"] = self.picture
        if self.usage is not None:
            result["usage"] = self.usage
        if self.value is not None:
            result["value"] = self.value
        if self.occurs is not None:
            result["occurs"] = self.occurs
        if self.occurs_depending_on is not None:
            result["occurs_depending_on"] = self.occurs_depending_on
        if self.redefines is not None:
            result["redefines"] = self.redefines
        if self.is_group:
            result["is_group"] = True
        if self.is_filler:
            result["is_filler"] = True
        if self.line_number > 0:
            result["line_number"] = self.line_number
        if self.copybook_source is not None:
            result["copybook_source"] = self.copybook_source
        if self.copybook is not None:
            result["copybook"] = self.copybook
        if self.position is not None:
            result["position"] = self.position
        if self.defined_in_record is not None:
            result["defined_in_record"] = self.defined_in_record
        if self.children:
            result["children"] = [child.to_dict() for child in self.children]

        return result


@dataclass
class DataDivisionSection:
    """A DATA DIVISION section (e.g., WORKING-STORAGE, FILE, LINKAGE).

    Attributes:
        name: Section name (e.g., "WORKING-STORAGE")
        records: List of Level 01 record nodes in this section
    """
    name: str
    records: List[DataItemNode] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            "name": self.name,
            "records": [record.to_dict() for record in self.records],
        }


@dataclass
class DataDivisionTree:
    """Complete DATA DIVISION tree structure.

    Attributes:
        program_name: Name of the COBOL program
        sections: List of DATA DIVISION sections with their records
        all_records: Flat list of all Level 01 records (regardless of section)
        summary: Summary statistics about the data structure
        execution_time_seconds: Time taken to generate the tree
        source_info: Source file metadata (if include_source_info was True)
        warnings: List of warning messages from preprocessing (e.g., copybooks not found)
    """
    program_name: str
    sections: List[DataDivisionSection]
    all_records: List[DataItemNode]
    summary: Dict[str, Any]
    execution_time_seconds: float
    source_info: Optional[Dict[str, Any]] = None
    warnings: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        result: Dict[str, Any] = {
            "program_name": self.program_name,
            "sections": [section.to_dict() for section in self.sections],
            "all_records": [record.to_dict() for record in self.all_records],
            "summary": self.summary,
            "execution_time_seconds": self.execution_time_seconds,
        }
        if self.source_info is not None:
            result["source_info"] = self.source_info
        if self.warnings:
            result["warnings"] = self.warnings
        return result


@dataclass
class CombinedOptions:
    """Options for combined analysis and tree generation.

    Attributes:
        copybook_paths: Additional paths to search for copybooks. The source file's
            directory is always searched by default.
        resolve_copies: Whether to resolve COPY statements (default: True)
        include_redefines: Include REDEFINES-affected variables in output (default: True)
        include_ancestor_mods: Include ancestor-modified variables in output (default: True)
        include_source_info: Include source file metadata in output (default: True)
        include_filler: Include FILLER items in tree output (default: True)
        include_88_levels: Include 88-level condition names in tree output (default: True)
    """
    copybook_paths: Optional[List[Path]] = None
    resolve_copies: bool = True
    include_redefines: bool = True
    include_ancestor_mods: bool = True
    include_source_info: bool = True
    include_filler: bool = True
    include_88_levels: bool = True

    def to_analysis_options(self) -> AnalysisOptions:
        """Convert to AnalysisOptions for paragraph variables analysis."""
        return AnalysisOptions(
            copybook_paths=self.copybook_paths,
            resolve_copies=self.resolve_copies,
            include_redefines=self.include_redefines,
            include_ancestor_mods=self.include_ancestor_mods,
            include_source_info=self.include_source_info,
        )

    def to_tree_options(self) -> TreeOptions:
        """Convert to TreeOptions for data division tree generation."""
        return TreeOptions(
            copybook_paths=self.copybook_paths,
            resolve_copies=self.resolve_copies,
            include_filler=self.include_filler,
            include_88_levels=self.include_88_levels,
            include_source_info=self.include_source_info,
        )


@dataclass
class CombinedResult:
    """Result of combined analysis and tree generation.

    Attributes:
        program_name: Name of the analyzed COBOL program
        data_division_tree: Hierarchical tree view of DATA DIVISION
        analysis_result: Paragraph variables analysis result
        execution_time_seconds: Total execution time for the combined analysis
        warnings: List of warning messages from preprocessing (e.g., copybooks not found)
    """
    program_name: str
    data_division_tree: DataDivisionTree
    analysis_result: AnalysisResult
    execution_time_seconds: float
    warnings: List[str] = field(default_factory=list)


def _build_copybook_line_map(original_source: str) -> Dict[str, int]:
    """Build a map of copybook names to their COPY statement line numbers.

    Scans the original source (before COPY resolution) to find all COPY
    statements and map copybook names to their line numbers.

    Args:
        original_source: The original COBOL source before COPY resolution

    Returns:
        Dictionary mapping copybook names (uppercase) to line numbers
    """
    import re
    copy_pattern = re.compile(
        r"^\s*COPY\s+([A-Za-z0-9][-A-Za-z0-9]*)",
        re.IGNORECASE | re.MULTILINE
    )

    copybook_lines: Dict[str, int] = {}
    for line_num, line in enumerate(original_source.splitlines(), start=1):
        match = copy_pattern.match(line.lstrip())
        if match:
            copybook_name = match.group(1).upper()
            # Store the first occurrence (in case of multiple COPY of same copybook)
            if copybook_name not in copybook_lines:
                copybook_lines[copybook_name] = line_num

    return copybook_lines


def _transform_data_item(
    item: "DataItem",
    options: TreeOptions,
    memory_regions: Dict[str, "MemoryRegion"],
    line_mapping: Optional[Dict[str, Dict[str, Any]]],
    copybook_line_map: Optional[Dict[str, int]] = None,
    defined_in_record: Optional[str] = None,
) -> Optional[DataItemNode]:
    """Transform a DataItem AST node to a DataItemNode tree node.

    Args:
        item: The DataItem AST node to transform
        options: Tree generation options
        memory_regions: Memory region lookup from DataStructureAnalyzer
        line_mapping: Line mapping from COPY expansion
        copybook_line_map: Map of copybook names to their COPY statement line numbers
        defined_in_record: Name of the Level 01 record containing this item.
            For Level 01 and 77-level items, this should be the item's own name.

    Returns:
        DataItemNode if the item should be included, None otherwise
    """
    # Skip FILLER if not included
    if item.is_filler and not options.include_filler:
        return None

    # Skip 88-levels if not included
    if item.level == 88 and not options.include_88_levels:
        return None

    # Get position from memory regions
    position = None
    region = memory_regions.get(item.name.upper())
    if region and region.size > 0:
        position = {
            "start": region.start_offset + 1,  # 1-indexed for COBOL compatibility
            "end": region.start_offset + region.size,
            "size": region.size,
        }

    # Determine copybook source and original line number in root file
    copybook_source = None
    original_line_number = item.line_number  # Default to expanded line number

    if item.line_number and line_mapping:
        mapping = line_mapping.get(str(item.line_number))
        if mapping:
            if mapping.get("is_copybook"):
                # Item is from a copybook
                copybook_source = mapping.get("source_file")
                # Use the COPY statement's line number in the root file
                # First, try to look up the copybook in our map
                if copybook_line_map and copybook_source:
                    copy_line = copybook_line_map.get(copybook_source.upper())
                    if copy_line:
                        original_line_number = copy_line
                    else:
                        # Fallback: use the original_line from mapping
                        # This is typically the COPY statement line for copybook content
                        original_line_number = mapping.get("original_line", item.line_number)
            else:
                # Item is from the main source - use the original line number
                original_line_number = mapping.get("original_line", item.line_number)

    # Recursively transform children
    children: List[DataItemNode] = []
    for child in item.children:
        transformed = _transform_data_item(
            child, options, memory_regions, line_mapping, copybook_line_map,
            defined_in_record
        )
        if transformed:
            children.append(transformed)

    # For FILLER items that don't come from a copybook themselves,
    # check if their children come from a copybook and set the copybook property
    copybook = None
    if item.is_filler and copybook_source is None and children:
        # Find the first non-88-level child that has a copybook_source
        for transformed_child in children:
            if transformed_child.level != 88 and transformed_child.copybook_source:
                copybook = transformed_child.copybook_source
                break

    return DataItemNode(
        name=item.name,
        level=item.level,
        picture=item.picture,
        usage=item.usage,
        value=item.value,
        occurs=item.occurs,
        occurs_depending_on=item.occurs_depending_on,
        redefines=item.redefines,
        is_group=item.is_group,
        is_filler=item.is_filler,
        line_number=original_line_number,
        copybook_source=copybook_source,
        copybook=copybook,
        position=position,
        defined_in_record=defined_in_record,
        children=children,
    )


def _count_items(node: DataItemNode) -> Tuple[int, int, int, int, int]:
    """Count items in a tree node recursively.

    Args:
        node: The root node to count from

    Returns:
        Tuple of (total, groups, elementary, filler, level_88)
    """
    total = 1
    groups = 1 if node.is_group else 0
    elementary = 0 if node.is_group else 1
    filler = 1 if node.is_filler else 0
    level_88 = 1 if node.level == 88 else 0

    for child in node.children:
        child_counts = _count_items(child)
        total += child_counts[0]
        groups += child_counts[1]
        elementary += child_counts[2]
        filler += child_counts[3]
        level_88 += child_counts[4]

    return (total, groups, elementary, filler, level_88)


def _compute_summary(all_records: List[DataItemNode]) -> Dict[str, Any]:
    """Compute summary statistics for the tree.

    Args:
        all_records: List of all Level 01 record nodes

    Returns:
        Dictionary with summary statistics
    """
    total_items = 0
    total_groups = 0
    total_elementary = 0
    total_filler = 0
    total_88_levels = 0

    for record in all_records:
        counts = _count_items(record)
        total_items += counts[0]
        total_groups += counts[1]
        total_elementary += counts[2]
        total_filler += counts[3]
        total_88_levels += counts[4]

    return {
        "total_records": len(all_records),
        "total_items": total_items,
        "group_items": total_groups,
        "elementary_items": total_elementary,
        "filler_items": total_filler,
        "level_88_items": total_88_levels,
    }


def get_data_division_tree(
    source_path: Path,
    options: Optional[TreeOptions] = None,
) -> DataDivisionTree:
    """Get a hierarchical tree view of COBOL DATA DIVISION variables.

    This function parses a COBOL source file and returns a tree structure
    representing all data items in the DATA DIVISION, organized by section.

    Args:
        source_path: Path to the COBOL source file
        options: Tree generation options (uses defaults if not provided)

    Returns:
        DataDivisionTree containing the hierarchical structure of all data items

    Raises:
        FileNotFoundError: If source file doesn't exist
        ParseError: If COBOL source cannot be parsed
        AnalysisError: If analysis fails for other reasons

    Example:
        >>> from cobol_ast import get_data_division_tree, TreeOptions
        >>> from pathlib import Path
        >>>
        >>> # Simple usage with defaults
        >>> tree = get_data_division_tree(Path("myprogram.cob"))
        >>> print(tree.program_name)
        >>> print(tree.summary)
        >>>
        >>> # With custom options
        >>> options = TreeOptions(
        ...     copybook_paths=[Path("./copybooks")],
        ...     include_filler=False,
        ...     include_88_levels=False,
        ... )
        >>> tree = get_data_division_tree(Path("myprogram.cob"), options)
        >>> print(json.dumps(tree.to_dict(), indent=2))
    """
    # Import here to avoid circular imports and keep module lightweight
    import time
    from preprocessor import detect_format, normalize_source
    from parser import CobolParser, ParseError
    from . import ASTBuilder
    from analyzers import DataStructureAnalyzer

    # Use defaults if no options provided
    if options is None:
        options = TreeOptions()

    # Validate input
    if not source_path.exists():
        raise FileNotFoundError(f"Source file not found: {source_path}")

    if not source_path.is_file():
        raise FileNotFoundError(f"Source path is not a file: {source_path}")

    start_time = time.perf_counter()

    try:
        # Read source file
        source = source_path.read_text(encoding="utf-8", errors="replace")
        original_source = source  # Keep original for copybook line mapping
        source_lines_count = len(source.splitlines())

        # Detect format
        source_lines = source.splitlines()
        source_format = detect_format(source_lines)

        # Build copybook line map from original source (before COPY resolution)
        copybook_line_map = _build_copybook_line_map(original_source)

        # Resolve COPY statements
        line_mapping = None
        warnings: List[str] = []
        if options.resolve_copies:
            resolution = _resolve_copies(source, source_path, options.copybook_paths)
            source = resolution.resolved_source
            line_mapping = {
                str(k): {
                    "original_line": v.original_line,
                    "source_file": v.source_file,
                    "is_copybook": v.is_copybook,
                }
                for k, v in resolution.line_mapping.items()
            }
            warnings = resolution.warnings

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

        # Run DataStructureAnalyzer for memory positions
        data_analyzer = DataStructureAnalyzer(program)
        data_analyzer.analyze()

        # Build memory regions lookup
        memory_regions = data_analyzer._memory_regions

        # Transform record descriptions to tree nodes
        all_records: List[DataItemNode] = []
        sections_dict: Dict[str, List[DataItemNode]] = {}

        for record_name, record in program.record_descriptions.items():
            # Use the root_item.name as defined_in_record (this is the Level 01 record name)
            record_defined_in = record.root_item.name
            transformed = _transform_data_item(
                record.root_item, options, memory_regions, line_mapping, copybook_line_map,
                defined_in_record=record_defined_in
            )
            if transformed:
                all_records.append(transformed)

                # Group by section
                section_name = record.section
                if section_name not in sections_dict:
                    sections_dict[section_name] = []
                sections_dict[section_name].append(transformed)

        # Build section list in standard order
        section_order = ["FILE", "WORKING-STORAGE", "LOCAL-STORAGE", "LINKAGE"]
        sections: List[DataDivisionSection] = []

        # Add known sections in order
        for section_name in section_order:
            if section_name in sections_dict:
                sections.append(DataDivisionSection(
                    name=section_name,
                    records=sections_dict[section_name]
                ))

        # Add any other sections not in the standard order
        for section_name, records in sections_dict.items():
            if section_name not in section_order:
                sections.append(DataDivisionSection(
                    name=section_name,
                    records=records
                ))

        # Compute summary
        summary = _compute_summary(all_records)

        # Calculate total execution time
        end_time = time.perf_counter()
        total_execution_time = end_time - start_time

        # Build source info if requested
        source_info = None
        if options.include_source_info:
            source_info = {
                "file_path": str(source_path.absolute()),
                "file_name": source_path.name,
                "source_format": source_format.value,
                "lines_count": source_lines_count,
            }

        return DataDivisionTree(
            program_name=program.name,
            sections=sections,
            all_records=all_records,
            summary=summary,
            execution_time_seconds=round(total_execution_time, 4),
            source_info=source_info,
            warnings=warnings,
        )

    except ParseError:
        raise  # Re-raise ParseError as-is
    except FileNotFoundError:
        raise  # Re-raise FileNotFoundError as-is
    except Exception as e:
        raise AnalysisError(f"Tree generation failed: {e}") from e


def analyze_with_tree(
    source_path: Path,
    options: Optional[CombinedOptions] = None,
) -> CombinedResult:
    """Analyze a COBOL source file and generate both analysis result and data division tree.

    This function performs both paragraph-variables analysis and data division tree
    generation in a single pass, avoiding duplicate preprocessing and parsing.
    Use this when you need both outputs for approximately 40% efficiency gain.

    Args:
        source_path: Path to the COBOL source file
        options: Combined options (uses defaults if not provided)

    Returns:
        CombinedResult containing:
        - data_division_tree: Hierarchical tree view of DATA DIVISION
        - analysis_result: Paragraph variables analysis result with variable_index

    Raises:
        FileNotFoundError: If source file doesn't exist
        ParseError: If COBOL source cannot be parsed
        AnalysisError: If analysis fails for other reasons

    Example:
        >>> from cobol_ast import analyze_with_tree, CombinedOptions
        >>> from pathlib import Path
        >>>
        >>> # Simple usage with defaults
        >>> result = analyze_with_tree(Path("myprogram.cob"))
        >>> print(result.program_name)
        >>> print(result.data_division_tree.summary)
        >>> print(result.analysis_result.paragraph_variables)
        >>>
        >>> # With custom options
        >>> options = CombinedOptions(
        ...     copybook_paths=[Path("./copybooks")],
        ...     include_filler=False,
        ...     include_redefines=False,
        ... )
        >>> result = analyze_with_tree(Path("myprogram.cob"), options)
    """
    # Import here to avoid circular imports and keep module lightweight
    import time
    from preprocessor import detect_format, normalize_source
    from parser import CobolParser, ParseError
    from . import ASTBuilder
    from analyzers import ImpactAnalyzer
    from output import ParagraphVariablesMapper

    # Use defaults if no options provided
    if options is None:
        options = CombinedOptions()

    # Convert to component options for filtering/features
    tree_options = options.to_tree_options()
    analysis_options = options.to_analysis_options()

    # Validate input
    if not source_path.exists():
        raise FileNotFoundError(f"Source file not found: {source_path}")

    if not source_path.is_file():
        raise FileNotFoundError(f"Source path is not a file: {source_path}")

    start_time = time.perf_counter()

    try:
        # ===== SHARED PREPROCESSING (done once) =====

        # Read source file
        source = source_path.read_text(encoding="utf-8", errors="replace")
        original_source = source  # Keep original for copybook line mapping
        source_lines_count = len(source.splitlines())

        # Detect format
        source_lines = source.splitlines()
        source_format = detect_format(source_lines)

        # Build copybook line map from original source (before COPY resolution)
        copybook_line_map = _build_copybook_line_map(original_source)

        # Resolve COPY statements
        line_mapping = None
        line_mapping_dict = None
        original_line_count = source_lines_count
        warnings: List[str] = []
        if options.resolve_copies:
            resolution = _resolve_copies(source, source_path, options.copybook_paths)
            source = resolution.resolved_source
            line_mapping = resolution.line_mapping
            original_line_count = resolution.original_line_count
            warnings = resolution.warnings
            # Also build dict version for tree generation
            line_mapping_dict = {
                str(k): {
                    "original_line": v.original_line,
                    "source_file": v.source_file,
                    "is_copybook": v.is_copybook,
                }
                for k, v in resolution.line_mapping.items()
            }

        # Normalize source
        source = normalize_source(source, source_format)

        # Parse (done once)
        parser = CobolParser(use_generated=False)
        try:
            parse_tree = parser.parse(source)
        except ParseError:
            raise  # Re-raise ParseError as-is

        # Build AST (done once)
        builder = ASTBuilder()
        program = builder.build(parse_tree)

        # ===== ANALYSIS (uses ImpactAnalyzer which contains DataStructureAnalyzer) =====

        analyzer = ImpactAnalyzer(program)
        analyzer.analyze()

        # Generate analysis output
        analysis_output = analyzer.generate_output()

        # Convert expanded line numbers to original line numbers
        if line_mapping:
            _apply_line_mapping_to_analysis(
                analysis_output, line_mapping, original_line_count
            )

        # Build source info (shared between outputs)
        source_info = None
        if options.include_source_info:
            source_info = {
                "file_path": str(source_path.absolute()),
                "file_name": source_path.name,
                "source_format": source_format.value,
                "lines_count": source_lines_count,
            }
            analysis_output["source_info"] = source_info

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
            include_redefines=analysis_options.include_redefines,
            include_ancestor_mods=analysis_options.include_ancestor_mods
        )

        # Add source_info to paragraph_variables output if requested
        if options.include_source_info and source_info:
            paragraph_variables_output["source_info"] = source_info

        # Build variable index for DataDivisionTree linking
        variable_index = _build_variable_index(paragraph_variables_output)

        # ===== DATA DIVISION TREE (reuses analyzer.data_analyzer._memory_regions) =====

        # Reuse memory regions from the already-run DataStructureAnalyzer
        memory_regions = analyzer.data_analyzer._memory_regions

        # Transform record descriptions to tree nodes
        all_records: List[DataItemNode] = []
        sections_dict: Dict[str, List[DataItemNode]] = {}

        for record_name, record in program.record_descriptions.items():
            # Use the root_item.name as defined_in_record (this is the Level 01 record name)
            record_defined_in = record.root_item.name
            transformed = _transform_data_item(
                record.root_item, tree_options, memory_regions, line_mapping_dict,
                copybook_line_map, defined_in_record=record_defined_in
            )
            if transformed:
                all_records.append(transformed)

                # Group by section
                section_name = record.section
                if section_name not in sections_dict:
                    sections_dict[section_name] = []
                sections_dict[section_name].append(transformed)

        # Build section list in standard order
        section_order = ["FILE", "WORKING-STORAGE", "LOCAL-STORAGE", "LINKAGE"]
        sections: List[DataDivisionSection] = []

        # Add known sections in order
        for section_name in section_order:
            if section_name in sections_dict:
                sections.append(DataDivisionSection(
                    name=section_name,
                    records=sections_dict[section_name]
                ))

        # Add any other sections not in the standard order
        for section_name, records in sections_dict.items():
            if section_name not in section_order:
                sections.append(DataDivisionSection(
                    name=section_name,
                    records=records
                ))

        # Compute summary
        summary = _compute_summary(all_records)

        # ===== BUILD RESULTS =====

        # Calculate total execution time
        end_time = time.perf_counter()
        total_execution_time = end_time - start_time

        # Add execution time to analysis output
        analysis_output["execution_time_seconds"] = round(total_execution_time, 4)

        # Build DataDivisionTree
        data_division_tree = DataDivisionTree(
            program_name=program.name,
            sections=sections,
            all_records=all_records,
            summary=summary,
            execution_time_seconds=round(total_execution_time, 4),
            source_info=source_info,
            warnings=warnings,
        )

        # Build AnalysisResult
        analysis_result = AnalysisResult(
            program_name=program.name,
            analysis=analysis_output,
            paragraph_variables=paragraph_variables_output,
            variable_index=variable_index,
            execution_time_seconds=round(total_execution_time, 4),
            source_info=source_info,
            warnings=warnings,
        )

        return CombinedResult(
            program_name=program.name,
            data_division_tree=data_division_tree,
            analysis_result=analysis_result,
            execution_time_seconds=round(total_execution_time, 4),
            warnings=warnings,
        )

    except ParseError:
        raise  # Re-raise ParseError as-is
    except FileNotFoundError:
        raise  # Re-raise FileNotFoundError as-is
    except Exception as e:
        raise AnalysisError(f"Combined analysis failed: {e}") from e
