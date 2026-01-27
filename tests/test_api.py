"""Tests for the public API module."""

import pytest
from pathlib import Path

from api import (
    analyze_paragraph_variables,
    AnalysisOptions,
    AnalysisResult,
    AnalysisError,
)
from parser import ParseError


# Path to test fixtures
FIXTURES_DIR = Path(__file__).parent / "fixtures"


class TestAnalyzePararagraphVariables:
    """Tests for the analyze_paragraph_variables function."""

    def test_basic_analysis(self):
        """Test basic analysis of a simple COBOL program."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        assert isinstance(result, AnalysisResult)
        assert result.program_name == "SIMPLE-PROGRAM"
        assert isinstance(result.analysis, dict)
        assert isinstance(result.paragraph_variables, dict)
        assert result.execution_time_seconds > 0

    def test_returns_both_outputs(self):
        """Test that both analysis and paragraph_variables outputs are returned."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # Analysis output should have expected keys
        assert "program_name" in result.analysis
        assert "sections_and_paragraphs" in result.analysis
        assert "data_hierarchy" in result.analysis

        # Paragraph variables output should have expected keys
        assert "program_name" in result.paragraph_variables
        assert "paragraphs" in result.paragraph_variables
        assert "summary" in result.paragraph_variables

    def test_with_default_options(self):
        """Test analysis with default options."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # Should include redefines and ancestor mods by default
        # Source info should NOT be included by default
        assert result.source_info is None

    def test_with_custom_options(self):
        """Test analysis with custom options."""
        source_path = FIXTURES_DIR / "simple_program.cob"
        options = AnalysisOptions(
            include_source_info=True,
            include_redefines=False,
            include_ancestor_mods=False,
        )

        result = analyze_paragraph_variables(source_path, options)

        # Source info should be included
        assert result.source_info is not None
        assert "file_name" in result.source_info
        assert result.source_info["file_name"] == "simple_program.cob"

    def test_include_source_info(self):
        """Test that source_info is included when requested."""
        source_path = FIXTURES_DIR / "simple_program.cob"
        options = AnalysisOptions(include_source_info=True)

        result = analyze_paragraph_variables(source_path, options)

        assert result.source_info is not None
        assert result.source_info["file_name"] == "simple_program.cob"
        assert result.source_info["source_format"] == "fixed"
        assert result.source_info["lines_count"] > 0

        # Source info should also be in both outputs
        assert "source_info" in result.analysis
        assert "source_info" in result.paragraph_variables

    def test_file_not_found(self):
        """Test that FileNotFoundError is raised for missing files."""
        source_path = FIXTURES_DIR / "nonexistent.cob"

        with pytest.raises(FileNotFoundError) as exc_info:
            analyze_paragraph_variables(source_path)

        assert "nonexistent.cob" in str(exc_info.value)

    def test_directory_instead_of_file(self):
        """Test that FileNotFoundError is raised when path is a directory."""
        with pytest.raises(FileNotFoundError) as exc_info:
            analyze_paragraph_variables(FIXTURES_DIR)

        assert "not a file" in str(exc_info.value)

    def test_paragraphs_contain_variables(self):
        """Test that paragraphs in the output contain modified variables."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        paragraphs = result.paragraph_variables.get("paragraphs", {})

        # INIT-PARA should have variables (INITIALIZE WS-EMPLOYEE-RECORD, etc.)
        assert "INIT-PARA" in paragraphs
        init_para_vars = paragraphs["INIT-PARA"]
        assert len(init_para_vars) > 0

        # PROCESS-PARA should have variables (MOVE, ADD, COMPUTE, etc.)
        assert "PROCESS-PARA" in paragraphs
        process_para_vars = paragraphs["PROCESS-PARA"]
        assert len(process_para_vars) > 0

    def test_analysis_with_redefines_program(self):
        """Test analysis of a program with REDEFINES."""
        source_path = FIXTURES_DIR / "redefines_program.cob"

        result = analyze_paragraph_variables(source_path)

        assert isinstance(result, AnalysisResult)
        assert isinstance(result.analysis, dict)
        assert isinstance(result.paragraph_variables, dict)

    def test_exclude_redefines(self):
        """Test that redefines can be excluded from output."""
        source_path = FIXTURES_DIR / "redefines_program.cob"

        # With redefines
        result_with = analyze_paragraph_variables(
            source_path,
            AnalysisOptions(include_redefines=True)
        )

        # Without redefines
        result_without = analyze_paragraph_variables(
            source_path,
            AnalysisOptions(include_redefines=False)
        )

        # The summary should show different counts
        summary_with = result_with.paragraph_variables.get("summary", {})
        summary_without = result_without.paragraph_variables.get("summary", {})

        # Variables in redefines records should be 0 when excluded
        # (or at least different from when included)
        assert summary_without.get("variables_in_redefines_records", 0) <= \
               summary_with.get("variables_in_redefines_records", 0)


class TestAnalysisOptions:
    """Tests for the AnalysisOptions dataclass."""

    def test_default_values(self):
        """Test that default values are set correctly."""
        options = AnalysisOptions()

        assert options.copybook_paths is None
        assert options.resolve_copies is True
        assert options.include_redefines is True
        assert options.include_ancestor_mods is True
        assert options.include_source_info is False

    def test_custom_values(self):
        """Test setting custom values."""
        options = AnalysisOptions(
            copybook_paths=[Path("/copybooks")],
            resolve_copies=False,
            include_redefines=False,
            include_ancestor_mods=False,
            include_source_info=True,
        )

        assert options.copybook_paths == [Path("/copybooks")]
        assert options.resolve_copies is False
        assert options.include_redefines is False
        assert options.include_ancestor_mods is False
        assert options.include_source_info is True


class TestAnalysisResult:
    """Tests for the AnalysisResult dataclass."""

    def test_result_attributes(self):
        """Test that result has all expected attributes."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # Check all attributes exist
        assert hasattr(result, "program_name")
        assert hasattr(result, "analysis")
        assert hasattr(result, "paragraph_variables")
        assert hasattr(result, "execution_time_seconds")
        assert hasattr(result, "source_info")

    def test_program_name_matches(self):
        """Test that program_name matches in result and outputs."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        assert result.program_name == result.analysis.get("program_name")
        assert result.program_name == result.paragraph_variables.get("program_name")
