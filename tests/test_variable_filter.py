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
