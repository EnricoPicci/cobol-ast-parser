"""JSON output writer for COBOL analysis results.

This module provides functionality to write analysis results
to JSON format with various options for formatting and filtering.
"""

import json
from typing import Any, Dict, Optional, TextIO
from pathlib import Path
from datetime import datetime


class JSONWriter:
    """Writes analysis results to JSON format.

    Supports various output options including:
    - Pretty printing with configurable indentation
    - Output to file or string
    - Filtering options for the output
    """

    def __init__(
        self,
        pretty_print: bool = True,
        indent: int = 2,
        sort_keys: bool = True,
        include_line_numbers: bool = True,
        include_modification_types: bool = True,
    ):
        """Initialize the JSON writer.

        Args:
            pretty_print: Whether to format JSON with indentation
            indent: Number of spaces for indentation
            sort_keys: Whether to sort dictionary keys
            include_line_numbers: Whether to include line numbers in output
            include_modification_types: Whether to include modification types
        """
        self.pretty_print = pretty_print
        self.indent = indent if pretty_print else None
        self.sort_keys = sort_keys
        self.include_line_numbers = include_line_numbers
        self.include_modification_types = include_modification_types

    def write(self, data: Dict[str, Any], output_path: Optional[Path] = None) -> str:
        """Write analysis results to JSON.

        Args:
            data: Analysis results dictionary
            output_path: Optional path to write file (if None, returns string)

        Returns:
            JSON string
        """
        # Filter data based on options
        filtered_data = self._filter_data(data)

        # Convert to JSON
        json_str = json.dumps(
            filtered_data,
            indent=self.indent,
            sort_keys=self.sort_keys,
            ensure_ascii=False,
            default=self._json_serializer,
        )

        # Write to file if path provided
        if output_path:
            output_path.write_text(json_str, encoding="utf-8")

        return json_str

    def write_to_stream(self, data: Dict[str, Any], stream: TextIO) -> None:
        """Write analysis results to a stream.

        Args:
            data: Analysis results dictionary
            stream: Output stream (file object)
        """
        filtered_data = self._filter_data(data)
        json.dump(
            filtered_data,
            stream,
            indent=self.indent,
            sort_keys=self.sort_keys,
            ensure_ascii=False,
            default=self._json_serializer,
        )

    def _filter_data(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Filter data based on writer options.

        Args:
            data: Original data dictionary

        Returns:
            Filtered data dictionary
        """
        if self.include_line_numbers and self.include_modification_types:
            return data

        # Deep copy and filter
        import copy
        filtered = copy.deepcopy(data)

        # Filter sections_and_paragraphs
        if "sections_and_paragraphs" in filtered:
            for name, modifications in filtered["sections_and_paragraphs"].items():
                if isinstance(modifications, list):
                    for mod in modifications:
                        if not self.include_line_numbers and "line_number" in mod:
                            del mod["line_number"]
                        if not self.include_modification_types and "modification_type" in mod:
                            del mod["modification_type"]

        return filtered

    def _json_serializer(self, obj: Any) -> Any:
        """Custom JSON serializer for special types.

        Args:
            obj: Object to serialize

        Returns:
            JSON-serializable representation
        """
        if isinstance(obj, datetime):
            return obj.isoformat()
        if isinstance(obj, set):
            return sorted(list(obj))
        if isinstance(obj, Path):
            return str(obj)
        if hasattr(obj, "__dict__"):
            return obj.__dict__

        raise TypeError(f"Object of type {type(obj)} is not JSON serializable")

    def format_compact(self, data: Dict[str, Any]) -> str:
        """Format data in compact single-line JSON.

        Args:
            data: Analysis results dictionary

        Returns:
            Compact JSON string
        """
        return json.dumps(
            self._filter_data(data),
            separators=(",", ":"),
            sort_keys=self.sort_keys,
            ensure_ascii=False,
            default=self._json_serializer,
        )


def create_output_report(
    analysis_output: Dict[str, Any],
    include_summary: bool = True,
    include_details: bool = True,
) -> Dict[str, Any]:
    """Create a formatted output report from analysis results.

    Args:
        analysis_output: Raw analysis output from ImpactAnalyzer
        include_summary: Whether to include summary section
        include_details: Whether to include detailed modifications

    Returns:
        Formatted report dictionary
    """
    report = {
        "program_name": analysis_output.get("program_name", "UNKNOWN"),
        "analysis_date": analysis_output.get("analysis_date", datetime.now().isoformat()),
    }

    if include_summary and "summary" in analysis_output:
        report["summary"] = analysis_output["summary"]

    if include_details and "sections_and_paragraphs" in analysis_output:
        report["sections_and_paragraphs"] = analysis_output["sections_and_paragraphs"]

    return report


def write_analysis_report(
    analysis_output: Dict[str, Any],
    output_path: Path,
    pretty_print: bool = True,
) -> None:
    """Convenience function to write analysis report to file.

    Args:
        analysis_output: Analysis results from ImpactAnalyzer
        output_path: Path to write the report
        pretty_print: Whether to format with indentation
    """
    writer = JSONWriter(pretty_print=pretty_print)
    report = create_output_report(analysis_output)
    writer.write(report, output_path)
