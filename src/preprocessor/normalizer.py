"""Source normalizer for COBOL programs.

This module normalizes COBOL source code by:
- Handling line continuations
- Removing/preserving comments
- Normalizing column positions for fixed format
- Converting to a clean format for parsing
"""

from typing import List, Optional, Tuple
from .format_detector import SourceFormat, is_comment_line, is_continuation_line, is_debug_line


def normalize_source(
    source: str,
    format: Optional[SourceFormat] = None,
    remove_comments: bool = True,
    remove_debug_lines: bool = True,
    preserve_line_numbers: bool = True,
) -> str:
    """Normalize COBOL source code for parsing.

    Args:
        source: Raw COBOL source code
        format: Source format (auto-detected if None)
        remove_comments: Whether to remove comment lines
        remove_debug_lines: Whether to remove debug lines
        preserve_line_numbers: Whether to preserve line numbers (blank lines for removed)

    Returns:
        Normalized source code
    """
    from .format_detector import detect_format

    lines = source.splitlines()

    if format is None:
        format = detect_format(lines)

    normalized_lines = []
    i = 0

    while i < len(lines):
        line = lines[i]

        # Handle empty lines
        if not line or line.isspace():
            if preserve_line_numbers:
                normalized_lines.append("")
            i += 1
            continue

        # Handle comment lines
        if is_comment_line(line, format):
            if remove_comments:
                if preserve_line_numbers:
                    normalized_lines.append("")
            else:
                normalized_lines.append(line)
            i += 1
            continue

        # Handle debug lines
        if is_debug_line(line, format):
            if remove_debug_lines:
                if preserve_line_numbers:
                    normalized_lines.append("")
            else:
                normalized_lines.append(line)
            i += 1
            continue

        # Handle continuation lines
        if format == SourceFormat.FIXED:
            content, lines_consumed = _handle_fixed_format_line(
                lines, i, remove_comments, preserve_line_numbers
            )
            normalized_lines.append(content)
            # Add blank lines for consumed continuation lines if preserving
            if preserve_line_numbers and lines_consumed > 1:
                normalized_lines.extend([""] * (lines_consumed - 1))
            i += lines_consumed
        else:
            content = _handle_free_format_line(line, remove_comments)
            normalized_lines.append(content)
            i += 1

    return "\n".join(normalized_lines)


def _handle_fixed_format_line(
    lines: List[str],
    start_index: int,
    remove_comments: bool,
    preserve_line_numbers: bool,
) -> Tuple[str, int]:
    """Handle a fixed format line, including continuations.

    Args:
        lines: All source lines
        start_index: Index of current line
        remove_comments: Whether to strip comments
        preserve_line_numbers: Whether to preserve line numbers

    Returns:
        Tuple of (normalized content, number of lines consumed)
    """
    line = lines[start_index]
    content_parts = []
    lines_consumed = 1

    # Extract content from Area A and B (columns 8-72)
    content = _extract_fixed_content(line)
    content_parts.append(content)

    # Check for continuation lines
    next_index = start_index + 1
    while next_index < len(lines):
        next_line = lines[next_index]

        if is_continuation_line(next_line, SourceFormat.FIXED):
            # Continuation line: column 7 is hyphen
            # Content starts at column 12 (index 11) typically
            # Remove trailing spaces from previous, add continuation content
            if content_parts:
                content_parts[-1] = content_parts[-1].rstrip()

            cont_content = _extract_continuation_content(next_line)
            content_parts.append(cont_content)
            lines_consumed += 1
            next_index += 1
        else:
            break

    return " ".join(content_parts), lines_consumed


def _extract_fixed_content(line: str) -> str:
    """Extract content from a fixed format line (columns 8-72).

    Args:
        line: Source line

    Returns:
        Content portion
    """
    if len(line) < 7:
        return ""

    # Check indicator column
    indicator = line[6] if len(line) > 6 else " "
    if indicator in ("*", "/"):
        return ""  # Comment line

    # Extract columns 8-72 (indices 7-71)
    if len(line) <= 7:
        return ""

    end_col = min(len(line), 72)
    content = line[7:end_col]

    return content


def _extract_continuation_content(line: str) -> str:
    """Extract content from a continuation line.

    For continuation lines, content typically starts at column 12,
    but can vary. The hyphen in column 7 indicates continuation.

    Args:
        line: Continuation line

    Returns:
        Content portion
    """
    if len(line) < 12:
        return line[7:] if len(line) > 7 else ""

    # Standard continuation starts at column 12 (index 11)
    end_col = min(len(line), 72)
    content = line[11:end_col]

    return content


def _handle_free_format_line(line: str, remove_comments: bool) -> str:
    """Handle a free format line.

    Args:
        line: Source line
        remove_comments: Whether to remove inline comments

    Returns:
        Processed content
    """
    content = line

    if remove_comments:
        # Remove inline comments (*>)
        comment_pos = content.find("*>")
        if comment_pos >= 0:
            content = content[:comment_pos]

    return content.rstrip()


def extract_sequence_number(line: str, format: SourceFormat) -> Optional[str]:
    """Extract the sequence number from a line (fixed format only).

    Args:
        line: Source line
        format: Source format

    Returns:
        Sequence number string or None
    """
    if format != SourceFormat.FIXED:
        return None

    if len(line) < 6:
        return None

    seq_area = line[:6].strip()
    return seq_area if seq_area else None


def get_line_content(line: str, format: SourceFormat) -> str:
    """Get the meaningful content portion of a COBOL line.

    Args:
        line: Full source line
        format: Source format

    Returns:
        Content portion only
    """
    if format == SourceFormat.FREE:
        return _handle_free_format_line(line, remove_comments=True)
    else:
        return _extract_fixed_content(line)
