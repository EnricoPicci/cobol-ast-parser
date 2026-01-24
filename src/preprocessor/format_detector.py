"""Source format detection for COBOL programs.

COBOL has two primary source formats:
- Fixed format (traditional): Column-based with specific areas
- Free format (modern): No column restrictions

This module detects which format a COBOL source file uses.
"""

from enum import Enum
from typing import List, Optional


class SourceFormat(Enum):
    """COBOL source format types."""

    FIXED = "fixed"
    FREE = "free"


def detect_format(source_lines: List[str]) -> SourceFormat:
    """Detect the source format of COBOL code.

    Detection heuristics:
    1. Check for >>SOURCE FREE directive
    2. Check for column 7 indicators (*, -, D, /)
    3. Check for sequence numbers in columns 1-6
    4. Check line length patterns

    Args:
        source_lines: List of source code lines

    Returns:
        SourceFormat.FIXED or SourceFormat.FREE
    """
    if not source_lines:
        return SourceFormat.FIXED

    # Check for explicit format directives
    for line in source_lines[:20]:  # Check first 20 lines
        stripped = line.strip().upper()
        if stripped.startswith(">>SOURCE"):
            if "FREE" in stripped:
                return SourceFormat.FREE
            elif "FIXED" in stripped:
                return SourceFormat.FIXED

    # Count indicators in column 7 and sequence numbers
    fixed_indicators = 0
    free_indicators = 0
    lines_checked = 0

    for line in source_lines:
        if not line or line.isspace():
            continue

        lines_checked += 1
        if lines_checked > 100:  # Sample first 100 non-empty lines
            break

        # Check for free format comment indicator
        if "*>" in line:
            free_indicators += 1
            continue

        # For fixed format, line should be at least 7 characters
        # and have meaningful content in column 7
        if len(line) >= 7:
            col7 = line[6] if len(line) > 6 else " "

            # Check for fixed format indicators in column 7
            if col7 in ("*", "-", "/", "D", "d"):
                fixed_indicators += 1
                continue

            # Check for sequence numbers (digits in columns 1-6)
            cols_1_6 = line[:6]
            if cols_1_6.strip() and cols_1_6.strip().isdigit():
                fixed_indicators += 1
                continue

    # Decision based on indicators
    if free_indicators > fixed_indicators:
        return SourceFormat.FREE

    # Default to fixed format (most common)
    return SourceFormat.FIXED


def is_comment_line(line: str, format: SourceFormat) -> bool:
    """Check if a line is a comment.

    Args:
        line: The source line to check
        format: The source format (FIXED or FREE)

    Returns:
        True if the line is a comment, False otherwise
    """
    if not line:
        return False

    if format == SourceFormat.FREE:
        stripped = line.strip()
        return stripped.startswith("*>") or stripped.startswith("*")

    # Fixed format: column 7 is the indicator
    if len(line) >= 7:
        indicator = line[6]
        return indicator in ("*", "/")

    return False


def is_continuation_line(line: str, format: SourceFormat) -> bool:
    """Check if a line is a continuation line.

    Args:
        line: The source line to check
        format: The source format (FIXED or FREE)

    Returns:
        True if the line is a continuation, False otherwise
    """
    if not line:
        return False

    if format == SourceFormat.FREE:
        # Free format continuation is handled differently
        # (previous line ends with hyphen)
        return False

    # Fixed format: hyphen in column 7
    if len(line) >= 7:
        return line[6] == "-"

    return False


def is_debug_line(line: str, format: SourceFormat) -> bool:
    """Check if a line is a debug line.

    Args:
        line: The source line to check
        format: The source format (FIXED or FREE)

    Returns:
        True if the line is a debug line, False otherwise
    """
    if not line:
        return False

    if format == SourceFormat.FREE:
        stripped = line.strip().upper()
        return stripped.startswith(">>D")

    # Fixed format: D or d in column 7
    if len(line) >= 7:
        return line[6] in ("D", "d")

    return False


def get_content_area(line: str, format: SourceFormat) -> str:
    """Extract the content area from a COBOL line.

    For fixed format: columns 8-72 (Area A + Area B)
    For free format: entire line (minus comments)

    Args:
        line: The source line
        format: The source format

    Returns:
        The content portion of the line
    """
    if not line:
        return ""

    if format == SourceFormat.FREE:
        # Remove inline comments
        comment_pos = line.find("*>")
        if comment_pos >= 0:
            return line[:comment_pos].rstrip()
        return line.rstrip()

    # Fixed format: columns 8-72 (indices 7-71)
    if len(line) < 7:
        return ""

    # Skip if it's a comment or debug line
    if line[6] in ("*", "/", "D", "d"):
        return ""

    # Extract columns 8-72
    content = line[7:72] if len(line) > 7 else ""
    return content.rstrip()
