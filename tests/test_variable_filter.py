"""Tests for the variable filter module."""

import pytest
import json
import tempfile
from pathlib import Path
from datetime import datetime

# Path is setup in conftest.py
from output.variable_filter import VariableFilter


class TestVariableFilter:
    """Tests for VariableFilter class."""

    @pytest.fixture
    def sample_analysis_data(self):
        """Create sample analysis data for testing."""
        return {
            "program_name": "TEST-PROGRAM",
            "analysis_date": "2026-01-25T10:00:00Z",
            "execution_time_seconds": 0.1234,
            "sections_and_paragraphs": {
                "INIT-SECTION": [
                    {
                        "variable": "WS-TOTAL",
                        "modification_type": "INITIALIZE",
                        "line_number": 100,
                        "affected_records": ["WS-TOTALS"]
                    },
                    {
                        "variable": "WS-COUNT",
                        "modification_type": "MOVE",
                        "line_number": 101,
                        "affected_records": ["WS-COUNTERS"]
                    }
                ],
                "PROCESS-PARA": [
                    {
                        "variable": "WS-TOTAL",
                        "modification_type": "ADD",
                        "line_number": 200,
                        "affected_records": ["WS-TOTALS"]
                    },
                    {
                        "variable": "CUST-BALANCE",
                        "modification_type": "COMPUTE",
                        "line_number": 201,
                        "affected_records": ["CUSTOMER-RECORD"]
                    }
                ],
                "OVERLAY-PARA": [
                    {
                        "variable": "CUST-OVERLAY-AMT",
                        "modification_type": "MOVE",
                        "line_number": 300,
                        "affected_records": ["CUSTOMER-OVERLAY"],
                        "affected_variables": [
                            {
                                "name": "CUST-BALANCE",
                                "overlap_type": "full",
                                "redefines_chain": "CUSTOMER-OVERLAY REDEFINES CUSTOMER-RECORD",
                                "redefines_level": 1
                            },
                            {
                                "name": "CUST-NAME",
                                "overlap_type": "partial",
                                "redefines_chain": "CUSTOMER-OVERLAY REDEFINES CUSTOMER-RECORD",
                                "redefines_level": 1
                            }
                        ]
                    }
                ]
            },
            "summary": {
                "total_modifications": 5
            }
        }

    def test_single_variable_direct_match(self, sample_analysis_data):
        """Test filtering for a single variable with direct modifications."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["WS-TOTAL"])

        assert result["program_name"] == "TEST-PROGRAM"
        assert "WS-TOTAL" in result["variables"]
        assert len(result["variables"]["WS-TOTAL"]["direct_modifications"]) == 2
        assert result["variables"]["WS-TOTAL"]["redefines_modifications"] == []
        assert result["summary"]["variables_found"] == 1
        assert result["summary"]["variables_not_found"] == []

    def test_multiple_variables(self, sample_analysis_data):
        """Test filtering for multiple variables."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["WS-TOTAL", "CUST-BALANCE"])

        assert len(result["filter_variables"]) == 2
        assert "WS-TOTAL" in result["variables"]
        assert "CUST-BALANCE" in result["variables"]
        assert result["summary"]["variables_found"] == 2

    def test_redefines_related_match(self, sample_analysis_data):
        """Test that REDEFINES-related modifications are captured."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["CUST-BALANCE"])

        assert "CUST-BALANCE" in result["variables"]
        var_data = result["variables"]["CUST-BALANCE"]

        # Should have direct modification from PROCESS-PARA
        assert len(var_data["direct_modifications"]) == 1
        assert var_data["direct_modifications"][0]["section_or_paragraph"] == "PROCESS-PARA"

        # Should have REDEFINES modification from OVERLAY-PARA
        assert len(var_data["redefines_modifications"]) == 1
        redef_mod = var_data["redefines_modifications"][0]
        assert redef_mod["section_or_paragraph"] == "OVERLAY-PARA"
        assert redef_mod["modified_variable"] == "CUST-OVERLAY-AMT"
        assert redef_mod["overlap_type"] == "full"
        assert "REDEFINES" in redef_mod["redefines_chain"]

    def test_variable_not_found(self, sample_analysis_data):
        """Test handling of variables not found in analysis."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["NONEXISTENT-VAR"])

        assert result["summary"]["variables_found"] == 0
        assert "NONEXISTENT-VAR" in result["summary"]["variables_not_found"]
        assert "NONEXISTENT-VAR" not in result["variables"]

    def test_mixed_found_and_not_found(self, sample_analysis_data):
        """Test filtering with some found and some not found variables."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["WS-TOTAL", "MISSING-VAR"])

        assert result["summary"]["variables_found"] == 1
        assert result["summary"]["variables_not_found"] == ["MISSING-VAR"]
        assert "WS-TOTAL" in result["variables"]
        assert "MISSING-VAR" not in result["variables"]

    def test_case_insensitivity(self, sample_analysis_data):
        """Test that variable matching is case-insensitive."""
        var_filter = VariableFilter(sample_analysis_data)

        # Test lowercase
        result = var_filter.filter(["ws-total"])
        assert "WS-TOTAL" in result["variables"]

        # Test mixed case
        result = var_filter.filter(["Ws-Total"])
        assert "WS-TOTAL" in result["variables"]

    def test_no_redefines_flag(self, sample_analysis_data):
        """Test filtering with REDEFINES excluded."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["CUST-BALANCE"], include_redefines=False)

        var_data = result["variables"]["CUST-BALANCE"]
        # Should have direct modifications
        assert len(var_data["direct_modifications"]) == 1
        # Should NOT have REDEFINES modifications
        assert len(var_data["redefines_modifications"]) == 0

    def test_redefines_only_match(self, sample_analysis_data):
        """Test variable that only appears in REDEFINES (affected_variables)."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["CUST-NAME"])

        assert "CUST-NAME" in result["variables"]
        var_data = result["variables"]["CUST-NAME"]
        # No direct modifications
        assert len(var_data["direct_modifications"]) == 0
        # Only REDEFINES modifications
        assert len(var_data["redefines_modifications"]) == 1

    def test_output_structure(self, sample_analysis_data):
        """Test that output has correct structure."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["WS-TOTAL"])

        # Check top-level fields
        assert "program_name" in result
        assert "analysis_date" in result
        assert "execution_time_seconds" in result
        assert "filter_variables" in result
        assert "variables" in result
        assert "summary" in result

        # Check summary fields
        summary = result["summary"]
        assert "variables_requested" in summary
        assert "variables_found" in summary
        assert "variables_not_found" in summary
        assert "total_direct_modifications" in summary
        assert "total_redefines_modifications" in summary

    def test_modification_details_preserved(self, sample_analysis_data):
        """Test that modification details are correctly preserved."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["WS-TOTAL"])

        mods = result["variables"]["WS-TOTAL"]["direct_modifications"]
        # Find INIT-SECTION modification
        init_mod = next(m for m in mods if m["section_or_paragraph"] == "INIT-SECTION")

        assert init_mod["modification_type"] == "INITIALIZE"
        assert init_mod["line_number"] == 100
        assert "WS-TOTALS" in init_mod["affected_records"]

    def test_summary_counts(self, sample_analysis_data):
        """Test that summary counts are accurate."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["WS-TOTAL", "CUST-BALANCE"])

        summary = result["summary"]
        assert summary["variables_requested"] == 2
        assert summary["variables_found"] == 2
        # WS-TOTAL: 2 direct, CUST-BALANCE: 1 direct
        assert summary["total_direct_modifications"] == 3
        # CUST-BALANCE: 1 redefines
        assert summary["total_redefines_modifications"] == 1

    def test_empty_sections(self):
        """Test handling of analysis with empty sections."""
        analysis_data = {
            "program_name": "EMPTY-TEST",
            "analysis_date": datetime.now().isoformat(),
            "sections_and_paragraphs": {}
        }
        var_filter = VariableFilter(analysis_data)
        result = var_filter.filter(["ANY-VAR"])

        assert result["summary"]["variables_found"] == 0
        assert "ANY-VAR" in result["summary"]["variables_not_found"]

    def test_filter_variables_echoed(self, sample_analysis_data):
        """Test that requested variables are echoed in output."""
        var_filter = VariableFilter(sample_analysis_data)
        result = var_filter.filter(["ws-total", "Cust-Balance"])

        # Should be normalized to uppercase
        assert "WS-TOTAL" in result["filter_variables"]
        assert "CUST-BALANCE" in result["filter_variables"]


class TestVariableFilterCLIIntegration:
    """Integration tests for filter-by-variable CLI command."""

    @pytest.fixture
    def temp_analysis_file(self, tmp_path):
        """Create a temporary analysis JSON file."""
        analysis_data = {
            "program_name": "CLI-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "execution_time_seconds": 0.05,
            "sections_and_paragraphs": {
                "MAIN-PARA": [
                    {
                        "variable": "WS-FIELD-A",
                        "modification_type": "MOVE",
                        "line_number": 50,
                        "affected_records": ["WS-RECORD"]
                    },
                    {
                        "variable": "WS-FIELD-B",
                        "modification_type": "ADD",
                        "line_number": 51,
                        "affected_records": ["WS-RECORD"]
                    }
                ]
            },
            "summary": {"total_modifications": 2}
        }

        json_file = tmp_path / "test-analysis.json"
        with open(json_file, "w") as f:
            json.dump(analysis_data, f)

        return json_file

    @pytest.fixture
    def temp_variables_file(self, tmp_path):
        """Create a temporary variables file."""
        vars_file = tmp_path / "variables.txt"
        vars_file.write_text("WS-FIELD-A\nWS-FIELD-B\n")
        return vars_file

    def test_filter_from_json_file(self, temp_analysis_file):
        """Test filtering from a JSON file."""
        with open(temp_analysis_file, "r") as f:
            analysis_data = json.load(f)

        var_filter = VariableFilter(analysis_data)
        result = var_filter.filter(["WS-FIELD-A"])

        assert result["program_name"] == "CLI-TEST"
        assert "WS-FIELD-A" in result["variables"]

    def test_filter_with_variables_file(self, temp_analysis_file, temp_variables_file):
        """Test filtering using variables from a file."""
        with open(temp_analysis_file, "r") as f:
            analysis_data = json.load(f)

        with open(temp_variables_file, "r") as f:
            variable_names = [line.strip() for line in f if line.strip()]

        var_filter = VariableFilter(analysis_data)
        result = var_filter.filter(variable_names)

        assert result["summary"]["variables_found"] == 2
        assert "WS-FIELD-A" in result["variables"]
        assert "WS-FIELD-B" in result["variables"]


class TestVariableFilterEdgeCases:
    """Edge case tests for VariableFilter."""

    def test_missing_affected_variables_field(self):
        """Test handling when affected_variables field is missing."""
        analysis_data = {
            "program_name": "NO-AFFECTED",
            "analysis_date": datetime.now().isoformat(),
            "sections_and_paragraphs": {
                "PARA-1": [
                    {
                        "variable": "VAR-A",
                        "modification_type": "MOVE",
                        "line_number": 10,
                        "affected_records": []
                        # No affected_variables field
                    }
                ]
            }
        }

        var_filter = VariableFilter(analysis_data)
        # Should not crash when looking for REDEFINES matches
        result = var_filter.filter(["VAR-B"])

        assert result["summary"]["variables_found"] == 0

    def test_same_variable_multiple_sections(self):
        """Test variable modified in multiple sections."""
        analysis_data = {
            "program_name": "MULTI-SECTION",
            "analysis_date": datetime.now().isoformat(),
            "sections_and_paragraphs": {
                "SECTION-A": [
                    {"variable": "COMMON-VAR", "modification_type": "MOVE", "line_number": 10, "affected_records": []}
                ],
                "SECTION-B": [
                    {"variable": "COMMON-VAR", "modification_type": "ADD", "line_number": 20, "affected_records": []}
                ],
                "SECTION-C": [
                    {"variable": "COMMON-VAR", "modification_type": "COMPUTE", "line_number": 30, "affected_records": []}
                ]
            }
        }

        var_filter = VariableFilter(analysis_data)
        result = var_filter.filter(["COMMON-VAR"])

        mods = result["variables"]["COMMON-VAR"]["direct_modifications"]
        assert len(mods) == 3
        sections = {m["section_or_paragraph"] for m in mods}
        assert sections == {"SECTION-A", "SECTION-B", "SECTION-C"}

    def test_variable_in_both_direct_and_redefines(self):
        """Test variable that appears both directly and via REDEFINES."""
        analysis_data = {
            "program_name": "DUAL-MATCH",
            "analysis_date": datetime.now().isoformat(),
            "sections_and_paragraphs": {
                "DIRECT-PARA": [
                    {"variable": "DUAL-VAR", "modification_type": "MOVE", "line_number": 10, "affected_records": []}
                ],
                "INDIRECT-PARA": [
                    {
                        "variable": "OTHER-VAR",
                        "modification_type": "MOVE",
                        "line_number": 20,
                        "affected_records": [],
                        "affected_variables": [
                            {"name": "DUAL-VAR", "overlap_type": "full", "redefines_chain": "X REDEFINES Y"}
                        ]
                    }
                ]
            }
        }

        var_filter = VariableFilter(analysis_data)
        result = var_filter.filter(["DUAL-VAR"])

        var_data = result["variables"]["DUAL-VAR"]
        assert len(var_data["direct_modifications"]) == 1
        assert len(var_data["redefines_modifications"]) == 1

    def test_empty_variable_names_list(self):
        """Test handling of empty variable names list."""
        analysis_data = {
            "program_name": "TEST",
            "analysis_date": datetime.now().isoformat(),
            "sections_and_paragraphs": {}
        }

        var_filter = VariableFilter(analysis_data)
        result = var_filter.filter([])

        assert result["summary"]["variables_requested"] == 0
        assert result["summary"]["variables_found"] == 0
        assert result["variables"] == {}

    def test_missing_optional_fields(self):
        """Test handling when optional fields are missing in modifications."""
        analysis_data = {
            "program_name": "MINIMAL",
            "sections_and_paragraphs": {
                "PARA-1": [
                    {
                        "variable": "VAR-A"
                        # Missing modification_type, line_number, affected_records
                    }
                ]
            }
        }

        var_filter = VariableFilter(analysis_data)
        result = var_filter.filter(["VAR-A"])

        # Should handle gracefully with None values
        mod = result["variables"]["VAR-A"]["direct_modifications"][0]
        assert mod["modification_type"] is None
        assert mod["line_number"] is None
        assert mod["affected_records"] == []


class TestAnalyzeAndFilterCommand:
    """Tests for the analyze-and-filter CLI command."""

    @pytest.fixture
    def fixtures_path(self):
        """Return path to test fixtures."""
        return Path(__file__).parent / "fixtures"

    @pytest.fixture
    def simple_program_path(self, fixtures_path):
        """Return path to simple_program.cob fixture."""
        return fixtures_path / "simple_program.cob"

    @pytest.fixture
    def redefines_program_path(self, fixtures_path):
        """Return path to redefines_program.cob fixture."""
        return fixtures_path / "redefines_program.cob"

    def test_analyze_and_filter_produces_both_files(self, simple_program_path, tmp_path):
        """Test that analyze-and-filter produces both output files."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(simple_program_path), "-v", "WS-LOOP-CTR", "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # Check both files exist
        analysis_file = tmp_path / "SIMPLE-PROGRAM-analysis.json"
        filter_file = tmp_path / "SIMPLE-PROGRAM-variable-filter.json"

        assert analysis_file.exists(), "Analysis file not created"
        assert filter_file.exists(), "Filter file not created"

        # Verify analysis file structure
        with open(analysis_file, "r") as f:
            analysis_data = json.load(f)
        assert "program_name" in analysis_data
        assert "sections_and_paragraphs" in analysis_data
        assert analysis_data["program_name"] == "SIMPLE-PROGRAM"

        # Verify filter file structure
        with open(filter_file, "r") as f:
            filter_data = json.load(f)
        assert "variables" in filter_data
        assert "WS-LOOP-CTR" in filter_data["variables"]
        assert filter_data["summary"]["variables_found"] == 1

    def test_analyze_and_filter_multiple_variables(self, simple_program_path, tmp_path):
        """Test filtering for multiple variables."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(simple_program_path), "-v", "WS-LOOP-CTR", "WS-EMP-SALARY", "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        filter_file = tmp_path / "SIMPLE-PROGRAM-variable-filter.json"
        with open(filter_file, "r") as f:
            filter_data = json.load(f)

        assert filter_data["summary"]["variables_requested"] == 2
        assert filter_data["summary"]["variables_found"] == 2
        assert "WS-LOOP-CTR" in filter_data["variables"]
        assert "WS-EMP-SALARY" in filter_data["variables"]

    def test_analyze_and_filter_with_variables_file(self, simple_program_path, tmp_path):
        """Test using --variables-file option."""
        import subprocess
        import sys

        # Create variables file
        vars_file = tmp_path / "variables.txt"
        vars_file.write_text("WS-LOOP-CTR\nWS-TOTAL-CTR\n")

        output_dir = tmp_path / "output"

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(simple_program_path), "--variables-file", str(vars_file),
             "-o", str(output_dir), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        filter_file = output_dir / "SIMPLE-PROGRAM-variable-filter.json"
        with open(filter_file, "r") as f:
            filter_data = json.load(f)

        assert filter_data["summary"]["variables_requested"] == 2
        assert "WS-LOOP-CTR" in filter_data["variables"]
        assert "WS-TOTAL-CTR" in filter_data["variables"]

    def test_analyze_and_filter_no_redefines(self, redefines_program_path, tmp_path):
        """Test --no-redefines flag excludes REDEFINES modifications."""
        import subprocess
        import sys

        # First run without --no-redefines
        result1 = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(redefines_program_path), "-v", "INPUT-TYPE", "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )
        assert result1.returncode == 0

        filter_file = tmp_path / "REDEFINES-EXAMPLE-variable-filter.json"
        with open(filter_file, "r") as f:
            with_redefines = json.load(f)

        # Run with --no-redefines
        output_dir2 = tmp_path / "no-redefines"
        result2 = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(redefines_program_path), "-v", "INPUT-TYPE", "--no-redefines",
             "-o", str(output_dir2), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )
        assert result2.returncode == 0

        filter_file2 = output_dir2 / "REDEFINES-EXAMPLE-variable-filter.json"
        with open(filter_file2, "r") as f:
            without_redefines = json.load(f)

        # Without --no-redefines should have more modifications
        assert with_redefines["summary"]["total_redefines_modifications"] > 0
        assert without_redefines["summary"]["total_redefines_modifications"] == 0

    def test_analyze_and_filter_stdout_output(self, simple_program_path):
        """Test output to stdout when no -o is specified."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(simple_program_path), "-v", "WS-LOOP-CTR", "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # stdout should be valid JSON (filter output)
        output_data = json.loads(result.stdout)
        assert "variables" in output_data
        assert "WS-LOOP-CTR" in output_data["variables"]

    def test_analyze_and_filter_variable_not_found(self, simple_program_path, tmp_path):
        """Test handling when variable is not found."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(simple_program_path), "-v", "NONEXISTENT-VAR", "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0  # Should still succeed

        filter_file = tmp_path / "SIMPLE-PROGRAM-variable-filter.json"
        with open(filter_file, "r") as f:
            filter_data = json.load(f)

        assert filter_data["summary"]["variables_found"] == 0
        assert "NONEXISTENT-VAR" in filter_data["summary"]["variables_not_found"]

    def test_analyze_and_filter_missing_source(self, tmp_path):
        """Test error handling when source file doesn't exist."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             "/nonexistent/path/program.cob", "-v", "WS-VAR", "-o", str(tmp_path)],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 1

    def test_analyze_and_filter_custom_filenames(self, simple_program_path, tmp_path):
        """Test custom output filename patterns."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(simple_program_path), "-v", "WS-LOOP-CTR", "-o", str(tmp_path),
             "--analysis-filename", "{program_name}-full.json",
             "--filter-filename", "{program_name}-vars.json", "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # Check custom filenames
        assert (tmp_path / "SIMPLE-PROGRAM-full.json").exists()
        assert (tmp_path / "SIMPLE-PROGRAM-vars.json").exists()

    def test_analyze_and_filter_case_insensitive_variables(self, simple_program_path, tmp_path):
        """Test that variable matching is case-insensitive."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze-and-filter",
             str(simple_program_path), "-v", "ws-loop-ctr", "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        filter_file = tmp_path / "SIMPLE-PROGRAM-variable-filter.json"
        with open(filter_file, "r") as f:
            filter_data = json.load(f)

        # Should find the variable despite lowercase input
        assert filter_data["summary"]["variables_found"] == 1
        assert "WS-LOOP-CTR" in filter_data["variables"]


class TestCLIBackwardsCompatibility:
    """Tests for CLI backwards compatibility."""

    @pytest.fixture
    def fixtures_path(self):
        """Return path to test fixtures."""
        return Path(__file__).parent / "fixtures"

    @pytest.fixture
    def simple_program_path(self, fixtures_path):
        """Return path to simple_program.cob fixture."""
        return fixtures_path / "simple_program.cob"

    def test_analyze_without_subcommand(self, simple_program_path, tmp_path):
        """Test that analyze works without explicit 'analyze' subcommand."""
        import subprocess
        import sys

        # Without 'analyze' subcommand (backwards compatible)
        result = subprocess.run(
            [sys.executable, "-m", "src",
             str(simple_program_path), "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0
        assert (tmp_path / "SIMPLE-PROGRAM-analysis.json").exists()

    def test_analyze_with_subcommand(self, simple_program_path, tmp_path):
        """Test that analyze works with explicit 'analyze' subcommand."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "analyze",
             str(simple_program_path), "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0
        assert (tmp_path / "SIMPLE-PROGRAM-analysis.json").exists()

    def test_version_flag(self):
        """Test --version flag."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "--version"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0
        assert "1.2.0" in result.stdout

    def test_help_shows_all_commands(self):
        """Test that help shows all available commands."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "--help"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0
        assert "analyze" in result.stdout
        assert "filter-by-variable" in result.stdout
        assert "analyze-and-filter" in result.stdout
