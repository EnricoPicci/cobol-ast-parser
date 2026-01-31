"""Tests for the preprocessor module."""

import pytest
from pathlib import Path

# Path is setup in conftest.py

from preprocessor.format_detector import (
    SourceFormat,
    detect_format,
    is_comment_line,
    is_continuation_line,
    get_content_area,
)
from preprocessor.copy_resolver import (
    CopyResolver,
    CopyStatement,
    CopyResolutionError,
    CircularCopyError,
    CopyNotFoundError,
)
from preprocessor.normalizer import normalize_source


class TestFormatDetector:
    """Tests for source format detection."""

    def test_detect_fixed_format_with_sequence_numbers(self):
        """Test detection of fixed format with sequence numbers."""
        lines = [
            "000100 IDENTIFICATION DIVISION.                                         ",
            "000200 PROGRAM-ID. TEST.                                                 ",
        ]
        assert detect_format(lines) == SourceFormat.FIXED

    def test_detect_fixed_format_with_comments(self):
        """Test detection of fixed format with column 7 comments."""
        lines = [
            "      *THIS IS A COMMENT",
            "       IDENTIFICATION DIVISION.",
        ]
        assert detect_format(lines) == SourceFormat.FIXED

    def test_detect_free_format_directive(self):
        """Test detection of free format via directive."""
        lines = [
            ">>SOURCE FREE",
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST.",
        ]
        assert detect_format(lines) == SourceFormat.FREE

    def test_detect_free_format_comments(self):
        """Test detection of free format with *> comments."""
        lines = [
            "*> This is a free format comment",
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST.",
        ] * 10  # Repeat to get enough samples
        assert detect_format(lines) == SourceFormat.FREE

    def test_is_comment_line_fixed(self):
        """Test comment line detection in fixed format."""
        assert is_comment_line("      *THIS IS A COMMENT", SourceFormat.FIXED)
        assert is_comment_line("      /PAGE BREAK", SourceFormat.FIXED)
        assert not is_comment_line("       MOVE A TO B", SourceFormat.FIXED)

    def test_is_comment_line_free(self):
        """Test comment line detection in free format."""
        assert is_comment_line("*> This is a comment", SourceFormat.FREE)
        assert is_comment_line("  *> Indented comment", SourceFormat.FREE)

    def test_is_continuation_line(self):
        """Test continuation line detection."""
        assert is_continuation_line("      -    CONTINUATION", SourceFormat.FIXED)
        assert not is_continuation_line("       NORMAL LINE", SourceFormat.FIXED)

    def test_get_content_area_fixed(self):
        """Test content extraction from fixed format."""
        line = "000100       MOVE A TO B.                                             "
        content = get_content_area(line, SourceFormat.FIXED)
        assert "MOVE A TO B" in content

    def test_get_content_area_free(self):
        """Test content extraction from free format."""
        line = "MOVE A TO B. *> comment"
        content = get_content_area(line, SourceFormat.FREE)
        assert content == "MOVE A TO B."


class TestCopyResolver:
    """Tests for COPY statement resolution."""

    @pytest.fixture
    def fixtures_path(self):
        """Get path to test fixtures."""
        return Path(__file__).parent / "fixtures"

    @pytest.fixture
    def resolver(self, fixtures_path):
        """Create a CopyResolver with test copybook paths."""
        return CopyResolver([fixtures_path / "copybooks"])

    def test_parse_simple_copy(self):
        """Test parsing a simple COPY statement."""
        source = "       COPY EMPLOYEE-CPY."
        resolver = CopyResolver()
        # Just check the pattern matches
        match = resolver.COPY_PATTERN.search(source)
        assert match is not None
        assert match.group(1).upper() == "EMPLOYEE-CPY"

    def test_parse_copy_with_library(self):
        """Test parsing COPY with library name."""
        source = "       COPY MYFILE OF MYLIB."
        resolver = CopyResolver()
        match = resolver.COPY_PATTERN.search(source)
        assert match is not None
        assert match.group(1).upper() == "MYFILE"
        assert match.group(2).upper() == "MYLIB"

    def test_parse_copy_with_replacing(self):
        """Test parsing COPY with REPLACING clause."""
        source = "       COPY TEMPLATE REPLACING ==:PREFIX:== BY ==WS-==."
        resolver = CopyResolver()
        match = resolver.COPY_PATTERN.search(source)
        assert match is not None
        assert match.group(3) is not None  # REPLACING clause

    def test_resolve_copy(self, resolver, fixtures_path):
        """Test resolving a COPY statement."""
        source = "       COPY EMPLOYEE-CPY."
        resolved = resolver.resolve(source)
        assert "WS-EMPLOYEE" in resolved
        assert "WS-EMP-ID" in resolved

    def test_copy_not_found(self, resolver):
        """Test error when copybook not found."""
        source = "       COPY NONEXISTENT."
        with pytest.raises(CopyNotFoundError):
            resolver.resolve(source)

    def test_line_mapping_with_empty_lines_in_copybook(self, resolver):
        """Test that empty lines in copybook content don't break line mapping.

        Regression test for bug where an empty line in copybook content that
        matched an empty line in the original source would reset the copybook
        tracking, causing subsequent lines to be incorrectly mapped to "COPYBOOK"
        instead of the actual copybook name.
        """
        # Source with COPY followed by empty line (which could match empty line in copybook)
        source = """       01  WS-RECORD.
           COPY EMPTYLINE-CPY.

       01  WS-OTHER.
           03  WS-FIELD            PIC X(10)."""

        resolved = resolver.resolve(source, "TEST-MAIN.cbl")

        # Check line mapping - all lines from the copybook should have
        # source_file set to "EMPTYLINE-CPY", not "COPYBOOK"
        for line_num, mapping in resolver.line_mapping.items():
            if mapping.is_copybook:
                assert mapping.source_file == "EMPTYLINE-CPY", (
                    f"Line {line_num} has source_file='{mapping.source_file}' "
                    f"but should be 'EMPTYLINE-CPY'. "
                    f"Empty lines in copybook should not reset copybook tracking."
                )


class TestNormalizer:
    """Tests for source normalization."""

    def test_normalize_removes_comments(self):
        """Test that normalization removes comments."""
        source = """      *THIS IS A COMMENT
       MOVE A TO B.
      *ANOTHER COMMENT
       MOVE C TO D."""

        normalized = normalize_source(source, SourceFormat.FIXED)
        assert "*THIS IS A COMMENT" not in normalized
        assert "MOVE A TO B" in normalized

    def test_normalize_handles_continuations(self):
        """Test that normalization handles continuation lines."""
        source = """       MOVE "THIS IS A VERY LONG STRING THAT CONTI
      -    "NUES ON THE NEXT LINE" TO WS-FIELD."""

        normalized = normalize_source(source, SourceFormat.FIXED)
        # Content should be joined
        assert "MOVE" in normalized

    def test_normalize_preserves_line_numbers(self):
        """Test that normalization preserves line numbers when requested."""
        source = """      *COMMENT
       MOVE A TO B.
      *COMMENT
       MOVE C TO D."""

        normalized = normalize_source(
            source, SourceFormat.FIXED, preserve_line_numbers=True
        )
        lines = normalized.splitlines()
        # Should have same number of lines
        assert len(lines) == len(source.splitlines())
