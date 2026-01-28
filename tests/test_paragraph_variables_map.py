"""Tests for the paragraph variables map module."""

import pytest
import json
from pathlib import Path
from datetime import datetime

# Path is setup in conftest.py
from output.paragraph_variables_map import ParagraphVariablesMapper


class TestParagraphVariablesMapper:
    """Tests for ParagraphVariablesMapper class."""

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
                ]
            },
            "data_hierarchy": {
                "WS-TOTALS": ["WS-TOTALS"],
                "WS-TOTAL": ["WS-TOTALS", "WS-TOTAL"],
                "WS-COUNTERS": ["WS-COUNTERS"],
                "WS-COUNT": ["WS-COUNTERS", "WS-COUNT"],
                "CUSTOMER-RECORD": ["CUSTOMER-RECORD"],
                "CUST-BALANCE": ["CUSTOMER-RECORD", "CUST-FINANCIAL", "CUST-BALANCE"]
            },
            "summary": {
                "total_modifications": 4
            }
        }

    def test_basic_mapping(self, sample_analysis_data):
        """Test basic variable mapping functionality."""
        mapper = ParagraphVariablesMapper(sample_analysis_data)
        result = mapper.map()

        assert result["program_name"] == "TEST-PROGRAM"
        assert "INIT-SECTION" in result["paragraphs"]
        assert "PROCESS-PARA" in result["paragraphs"]
        assert "WS-TOTAL" in result["paragraphs"]["INIT-SECTION"]
        assert "WS-COUNT" in result["paragraphs"]["INIT-SECTION"]

    def test_direct_modifications_appear_in_output(self, sample_analysis_data):
        """Test that directly modified variables appear in output."""
        mapper = ParagraphVariablesMapper(sample_analysis_data)
        result = mapper.map()

        # WS-TOTAL should appear in both INIT-SECTION and PROCESS-PARA
        assert "WS-TOTAL" in result["paragraphs"]["INIT-SECTION"]
        assert "WS-TOTAL" in result["paragraphs"]["PROCESS-PARA"]

        # CUST-BALANCE should only be in PROCESS-PARA
        assert "CUST-BALANCE" in result["paragraphs"]["PROCESS-PARA"]
        assert "CUST-BALANCE" not in result["paragraphs"]["INIT-SECTION"]

    def test_variable_record_info(self, sample_analysis_data):
        """Test that variable record information is correct."""
        mapper = ParagraphVariablesMapper(sample_analysis_data)
        result = mapper.map()

        # WS-TOTAL is in WS-TOTALS record
        ws_total = result["paragraphs"]["INIT-SECTION"]["WS-TOTAL"]
        assert ws_total["defined_in_record"] == "WS-TOTALS"
        assert ws_total["base_record"] == "WS-TOTALS"

    def test_summary_counts(self, sample_analysis_data):
        """Test that summary statistics are correct."""
        mapper = ParagraphVariablesMapper(sample_analysis_data)
        result = mapper.map()

        summary = result["summary"]
        assert summary["total_paragraphs_with_changes"] == 2
        # WS-TOTAL, WS-COUNT, CUST-BALANCE = 3 unique vars (without ancestor tracking)
        assert summary["total_unique_variables"] >= 3

    def test_output_structure(self, sample_analysis_data):
        """Test that output has correct top-level structure."""
        mapper = ParagraphVariablesMapper(sample_analysis_data)
        result = mapper.map()

        # Check top-level fields
        assert "program_name" in result
        assert "analysis_date" in result
        assert "execution_time_seconds" in result
        assert "paragraphs" in result
        assert "summary" in result

        # Check summary fields
        summary = result["summary"]
        assert "total_paragraphs_with_changes" in summary
        assert "total_unique_variables" in summary
        assert "variables_in_redefines_records" in summary
        assert "variables_via_ancestor_modification" in summary
        assert "level_77_variables" in summary


class TestRedefinesResolution:
    """Tests for REDEFINES chain resolution."""

    @pytest.fixture
    def redefines_analysis_data(self):
        """Create analysis data with REDEFINES relationships."""
        return {
            "program_name": "REDEFINES-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "MODIFY-PARA": [
                    {
                        "variable": "TRAN-CUSTOMER-ID",
                        "modification_type": "MOVE",
                        "line_number": 300,
                        "affected_records": ["PAYMENT-DETAIL", "TRANSACTION-RECORD"],
                        "affected_variables": [
                            {
                                "name": "PAY-CASH",
                                "overlap_type": "direct_redefines_group",
                                "redefines_chain": "PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD",
                                "redefines_level": 1
                            }
                        ]
                    }
                ]
            },
            "data_hierarchy": {
                "TRANSACTION-RECORD": ["TRANSACTION-RECORD"],
                "TRAN-CUSTOMER-ID": ["TRANSACTION-RECORD", "TRAN-CUSTOMER-ID"],
                "PAYMENT-DETAIL": ["PAYMENT-DETAIL"],
                "PAY-CASH": ["PAYMENT-DETAIL", "PAY-CASH"]
            }
        }

    def test_redefines_variables_included(self, redefines_analysis_data):
        """Test that REDEFINES-affected variables are included."""
        mapper = ParagraphVariablesMapper(redefines_analysis_data)
        result = mapper.map()

        para_vars = result["paragraphs"]["MODIFY-PARA"]

        # Direct modification
        assert "TRAN-CUSTOMER-ID" in para_vars

        # REDEFINES-affected variable
        assert "PAY-CASH" in para_vars

    def test_redefines_base_record_resolution(self, redefines_analysis_data):
        """Test that base_record follows REDEFINES chain."""
        mapper = ParagraphVariablesMapper(redefines_analysis_data)
        result = mapper.map()

        pay_cash = result["paragraphs"]["MODIFY-PARA"]["PAY-CASH"]

        # PAY-CASH is defined in PAYMENT-DETAIL
        assert pay_cash["defined_in_record"] == "PAYMENT-DETAIL"

        # But PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD
        assert pay_cash["base_record"] == "TRANSACTION-RECORD"

    def test_no_redefines_flag(self, redefines_analysis_data):
        """Test --no-redefines excludes REDEFINES-affected variables."""
        mapper = ParagraphVariablesMapper(redefines_analysis_data)
        result = mapper.map(include_redefines=False)

        para_vars = result["paragraphs"]["MODIFY-PARA"]

        # Direct modification should still be included
        assert "TRAN-CUSTOMER-ID" in para_vars

        # REDEFINES-affected should be excluded
        assert "PAY-CASH" not in para_vars


class TestRedefinesChain:
    """Tests for multi-level REDEFINES chain resolution."""

    @pytest.fixture
    def chained_redefines_data(self):
        """Create analysis data with REDEFINES chain (A REDEFINES B REDEFINES C)."""
        return {
            "program_name": "CHAIN-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "CHAIN-PARA": [
                    {
                        "variable": "VAR-A",
                        "modification_type": "MOVE",
                        "line_number": 100,
                        "affected_records": ["RECORD-A"],
                        "affected_variables": [
                            {
                                "name": "VAR-B",
                                "overlap_type": "full",
                                "redefines_chain": "RECORD-B REDEFINES RECORD-A",
                                "redefines_level": 1
                            },
                            {
                                "name": "VAR-C",
                                "overlap_type": "full",
                                "redefines_chain": "RECORD-C REDEFINES RECORD-B",
                                "redefines_level": 2
                            }
                        ]
                    }
                ]
            },
            "data_hierarchy": {
                "RECORD-A": ["RECORD-A"],
                "VAR-A": ["RECORD-A", "VAR-A"],
                "RECORD-B": ["RECORD-B"],
                "VAR-B": ["RECORD-B", "VAR-B"],
                "RECORD-C": ["RECORD-C"],
                "VAR-C": ["RECORD-C", "VAR-C"]
            }
        }

    def test_chain_resolution_to_root(self, chained_redefines_data):
        """Test that REDEFINES chain resolves to ultimate root."""
        mapper = ParagraphVariablesMapper(chained_redefines_data)
        result = mapper.map()

        para_vars = result["paragraphs"]["CHAIN-PARA"]

        # VAR-A is in RECORD-A (no redefines)
        var_a = para_vars["VAR-A"]
        assert var_a["defined_in_record"] == "RECORD-A"
        assert var_a["base_record"] == "RECORD-A"

        # VAR-B is in RECORD-B which REDEFINES RECORD-A
        var_b = para_vars["VAR-B"]
        assert var_b["defined_in_record"] == "RECORD-B"
        assert var_b["base_record"] == "RECORD-A"

        # VAR-C is in RECORD-C which REDEFINES RECORD-B which REDEFINES RECORD-A
        var_c = para_vars["VAR-C"]
        assert var_c["defined_in_record"] == "RECORD-C"
        assert var_c["base_record"] == "RECORD-A"


class Test77LevelVariables:
    """Tests for 77-level variable detection."""

    @pytest.fixture
    def analysis_with_77_level(self):
        """Create analysis data with 77-level variables."""
        return {
            "program_name": "LEVEL77-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "WORK-PARA": [
                    {
                        "variable": "WS-STANDALONE",
                        "modification_type": "MOVE",
                        "line_number": 100,
                        "affected_records": ["WS-STANDALONE"]
                    },
                    {
                        "variable": "WS-NESTED-VAR",
                        "modification_type": "MOVE",
                        "line_number": 101,
                        "affected_records": ["WS-RECORD"]
                    }
                ]
            },
            "data_hierarchy": {
                # 77-level: hierarchy only contains itself
                "WS-STANDALONE": ["WS-STANDALONE"],
                # Nested variable: has parent in hierarchy
                "WS-RECORD": ["WS-RECORD"],
                "WS-NESTED-VAR": ["WS-RECORD", "WS-NESTED-VAR"]
            },
            "memory_regions": {
                "WS-STANDALONE": {
                    "start_offset": 0,
                    "size": 10,
                    "record_name": "WS-STANDALONE",
                    "level": 77  # This marks it as a 77-level variable
                },
                "WS-RECORD": {
                    "start_offset": 0,
                    "size": 50,
                    "record_name": "WS-RECORD",
                    "level": 1
                },
                "WS-NESTED-VAR": {
                    "start_offset": 10,
                    "size": 20,
                    "record_name": "WS-RECORD",
                    "level": 5
                }
            }
        }

    def test_77_level_flag_present(self, analysis_with_77_level):
        """Test that 77-level variables have 77-level-var flag."""
        mapper = ParagraphVariablesMapper(analysis_with_77_level)
        result = mapper.map()

        para_vars = result["paragraphs"]["WORK-PARA"]

        # WS-STANDALONE is 77-level
        standalone = para_vars["WS-STANDALONE"]
        assert standalone.get("77-level-var") is True
        assert standalone["defined_in_record"] == "WS-STANDALONE"
        assert standalone["base_record"] == "WS-STANDALONE"

        # WS-NESTED-VAR is NOT 77-level
        nested = para_vars["WS-NESTED-VAR"]
        assert "77-level-var" not in nested

    def test_77_level_count_in_summary(self, analysis_with_77_level):
        """Test that summary includes correct 77-level count."""
        mapper = ParagraphVariablesMapper(analysis_with_77_level)
        result = mapper.map()

        assert result["summary"]["level_77_variables"] == 1


class TestAncestorModifications:
    """Tests for ancestor modification tracking."""

    @pytest.fixture
    def hierarchical_analysis_data(self):
        """Create analysis data with nested data hierarchy."""
        return {
            "program_name": "ANCESTOR-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "INIT-PARA": [
                    {
                        "variable": "CUST-HEADER",
                        "modification_type": "INITIALIZE",
                        "line_number": 100,
                        "affected_records": ["CUSTOMER-RECORD"]
                    }
                ],
                "UPDATE-PARA": [
                    {
                        "variable": "CUST-ID",
                        "modification_type": "MOVE",
                        "line_number": 150,
                        "affected_records": ["CUSTOMER-RECORD"]
                    }
                ]
            },
            "data_hierarchy": {
                "CUSTOMER-RECORD": ["CUSTOMER-RECORD"],
                "CUST-HEADER": ["CUSTOMER-RECORD", "CUST-HEADER"],
                "CUST-ID": ["CUSTOMER-RECORD", "CUST-HEADER", "CUST-ID"],
                "CUST-NAME": ["CUSTOMER-RECORD", "CUST-HEADER", "CUST-NAME"]
            }
        }

    def test_children_included_when_group_modified(self, hierarchical_analysis_data):
        """Test that children of modified group appear in output."""
        mapper = ParagraphVariablesMapper(hierarchical_analysis_data)
        result = mapper.map()

        init_para_vars = result["paragraphs"]["INIT-PARA"]

        # CUST-HEADER was directly modified
        assert "CUST-HEADER" in init_para_vars

        # CUST-ID and CUST-NAME are children of CUST-HEADER, so should be included
        assert "CUST-ID" in init_para_vars
        assert "CUST-NAME" in init_para_vars

    def test_no_ancestor_mods_flag(self, hierarchical_analysis_data):
        """Test --no-ancestor-mods excludes children of modified groups."""
        mapper = ParagraphVariablesMapper(hierarchical_analysis_data)
        result = mapper.map(include_ancestor_mods=False)

        init_para_vars = result["paragraphs"]["INIT-PARA"]

        # Direct modification still included
        assert "CUST-HEADER" in init_para_vars

        # Children should NOT be included
        assert "CUST-ID" not in init_para_vars
        assert "CUST-NAME" not in init_para_vars

    def test_ancestor_mod_count_in_summary(self, hierarchical_analysis_data):
        """Test that summary tracks ancestor modification count."""
        mapper = ParagraphVariablesMapper(hierarchical_analysis_data)
        result = mapper.map()

        # CUST-ID and CUST-NAME are included via ancestor modification
        assert result["summary"]["variables_via_ancestor_modification"] >= 2


class TestEdgeCases:
    """Tests for edge cases and error handling."""

    def test_empty_paragraphs_excluded(self):
        """Test that paragraphs with no changes are excluded from output."""
        analysis_data = {
            "program_name": "EMPTY-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "HAS-MODS": [
                    {"variable": "VAR-A", "modification_type": "MOVE", "line_number": 10, "affected_records": []}
                ],
                "EMPTY-PARA": []
            },
            "data_hierarchy": {
                "VAR-A": ["VAR-A"]
            }
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        result = mapper.map()

        assert "HAS-MODS" in result["paragraphs"]
        assert "EMPTY-PARA" not in result["paragraphs"]
        assert result["summary"]["total_paragraphs_with_changes"] == 1

    def test_case_insensitivity(self):
        """Test that variable names are handled case-insensitively."""
        analysis_data = {
            "program_name": "CASE-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PARA-1": [
                    {"variable": "my-variable", "modification_type": "MOVE", "line_number": 10, "affected_records": []}
                ]
            },
            "data_hierarchy": {
                "MY-VARIABLE": ["MY-VARIABLE"]
            }
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        result = mapper.map()

        # Variable should be normalized to uppercase
        assert "MY-VARIABLE" in result["paragraphs"]["PARA-1"]

    def test_missing_data_hierarchy(self):
        """Test handling when data_hierarchy is missing."""
        analysis_data = {
            "program_name": "NO-HIERARCHY",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "SOME-PARA": [
                    {"variable": "MY-VAR", "modification_type": "MOVE", "line_number": 10, "affected_records": []}
                ]
            }
            # No data_hierarchy key
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        result = mapper.map()

        # Should work without crashing
        assert "SOME-PARA" in result["paragraphs"]
        var_info = result["paragraphs"]["SOME-PARA"]["MY-VAR"]
        # Without hierarchy, variable is its own record
        assert var_info["defined_in_record"] == "MY-VAR"
        assert var_info["base_record"] == "MY-VAR"

    def test_variable_not_in_hierarchy(self):
        """Test handling when variable is not found in data_hierarchy."""
        analysis_data = {
            "program_name": "MISSING-VAR",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PARA-1": [
                    {"variable": "UNKNOWN-VAR", "modification_type": "MOVE", "line_number": 10, "affected_records": []}
                ]
            },
            "data_hierarchy": {
                "OTHER-VAR": ["SOME-RECORD", "OTHER-VAR"]
            }
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        result = mapper.map()

        # Variable should still appear, defaulting to itself as record
        var_info = result["paragraphs"]["PARA-1"]["UNKNOWN-VAR"]
        assert var_info["defined_in_record"] == "UNKNOWN-VAR"
        assert var_info["base_record"] == "UNKNOWN-VAR"

    def test_same_variable_multiple_paragraphs(self):
        """Test that same variable appears in multiple paragraphs."""
        analysis_data = {
            "program_name": "MULTI-PARA",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PARA-A": [
                    {"variable": "COMMON-VAR", "modification_type": "MOVE", "line_number": 10, "affected_records": []}
                ],
                "PARA-B": [
                    {"variable": "COMMON-VAR", "modification_type": "ADD", "line_number": 20, "affected_records": []}
                ]
            },
            "data_hierarchy": {
                "COMMON-VAR": ["COMMON-VAR"]
            }
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        result = mapper.map()

        # Variable should appear in both paragraphs
        assert "COMMON-VAR" in result["paragraphs"]["PARA-A"]
        assert "COMMON-VAR" in result["paragraphs"]["PARA-B"]

        # But unique count should be 1
        assert result["summary"]["total_unique_variables"] == 1

    def test_empty_sections_and_paragraphs(self):
        """Test handling when sections_and_paragraphs is empty."""
        analysis_data = {
            "program_name": "EMPTY-PROGRAM",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {}
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        result = mapper.map()

        assert result["paragraphs"] == {}
        assert result["summary"]["total_paragraphs_with_changes"] == 0
        assert result["summary"]["total_unique_variables"] == 0

    def test_circular_redefines_detection(self):
        """Test that circular REDEFINES chains don't cause infinite loop."""
        analysis_data = {
            "program_name": "CIRCULAR-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PARA-1": [
                    {
                        "variable": "VAR-A",
                        "modification_type": "MOVE",
                        "line_number": 10,
                        "affected_records": ["RECORD-A"],
                        "affected_variables": [
                            {
                                "name": "VAR-B",
                                "overlap_type": "full",
                                "redefines_chain": "RECORD-B REDEFINES RECORD-A",
                                "redefines_level": 1
                            },
                            {
                                "name": "VAR-C",
                                "overlap_type": "full",
                                # Circular: A -> B -> C -> A
                                "redefines_chain": "RECORD-A REDEFINES RECORD-C",
                                "redefines_level": 1
                            }
                        ]
                    }
                ]
            },
            "data_hierarchy": {
                "RECORD-A": ["RECORD-A"],
                "VAR-A": ["RECORD-A", "VAR-A"],
                "RECORD-B": ["RECORD-B"],
                "VAR-B": ["RECORD-B", "VAR-B"],
                "RECORD-C": ["RECORD-C"],
                "VAR-C": ["RECORD-C", "VAR-C"]
            }
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        # Should not hang or crash
        result = mapper.map()

        assert "PARA-1" in result["paragraphs"]


class TestCLIIntegration:
    """Integration tests for paragraph-variables-map CLI command."""

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

    def test_basic_command(self, simple_program_path):
        """Test basic command execution with COBOL source."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # Output should be valid JSON (paragraph-variables map)
        output_data = json.loads(result.stdout)
        assert "program_name" in output_data
        assert "paragraphs" in output_data
        assert output_data["program_name"] == "SIMPLE-PROGRAM"

    def test_output_to_directory(self, simple_program_path, tmp_path):
        """Test that both analysis and map files are written to output directory."""
        import subprocess
        import sys

        output_dir = tmp_path / "output"

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "-o", str(output_dir), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # Check both files exist
        analysis_file = output_dir / "SIMPLE-PROGRAM-analysis.json"
        map_file = output_dir / "SIMPLE-PROGRAM-paragraph-variables.json"

        assert analysis_file.exists(), "Analysis file not created"
        assert map_file.exists(), "Paragraph variables map file not created"

        # Verify analysis file structure
        with open(analysis_file, "r") as f:
            analysis_data = json.load(f)
        assert "program_name" in analysis_data
        assert "sections_and_paragraphs" in analysis_data
        assert analysis_data["program_name"] == "SIMPLE-PROGRAM"

        # Verify map file structure
        with open(map_file, "r") as f:
            map_data = json.load(f)
        assert "paragraphs" in map_data
        assert "summary" in map_data
        assert map_data["program_name"] == "SIMPLE-PROGRAM"

    def test_no_redefines_flag(self, redefines_program_path, tmp_path):
        """Test --no-redefines flag from command line."""
        import subprocess
        import sys

        # Without --no-redefines
        result1 = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(redefines_program_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )
        assert result1.returncode == 0
        output1 = json.loads(result1.stdout)
        # Should have variables_in_redefines_records > 0
        with_redefines_count = output1["summary"]["variables_in_redefines_records"]

        # With --no-redefines
        result2 = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(redefines_program_path), "--no-redefines", "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )
        assert result2.returncode == 0
        output2 = json.loads(result2.stdout)

        # Without --no-redefines should have more (or equal) variables
        assert output1["summary"]["total_unique_variables"] >= output2["summary"]["total_unique_variables"]

    def test_no_ancestor_mods_flag(self, simple_program_path):
        """Test --no-ancestor-mods flag from command line."""
        import subprocess
        import sys

        # Without --no-ancestor-mods
        result1 = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )
        assert result1.returncode == 0
        output1 = json.loads(result1.stdout)

        # With --no-ancestor-mods
        result2 = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "--no-ancestor-mods", "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )
        assert result2.returncode == 0
        output2 = json.loads(result2.stdout)

        # With ancestor mods should have more variables
        assert output1["summary"]["total_unique_variables"] >= output2["summary"]["total_unique_variables"]
        assert output2["summary"]["variables_via_ancestor_modification"] == 0

    def test_missing_source_file(self, tmp_path):
        """Test proper error handling for missing source file."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(tmp_path / "nonexistent.cob")],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 1

    def test_custom_output_filenames(self, simple_program_path, tmp_path):
        """Test custom output filename patterns."""
        import subprocess
        import sys

        output_dir = tmp_path / "output"

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "-o", str(output_dir),
             "--analysis-filename", "{program_name}-full.json",
             "--output-filename", "{program_name}-vars.json", "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # Check custom filenames
        assert (output_dir / "SIMPLE-PROGRAM-full.json").exists()
        assert (output_dir / "SIMPLE-PROGRAM-vars.json").exists()

    def test_help_shows_command(self):
        """Test that help shows the paragraph-variables-map command."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "--help"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0
        assert "paragraph-variables-map" in result.stdout

    def test_command_help(self):
        """Test paragraph-variables-map --help."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map", "--help"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0
        assert "--no-redefines" in result.stdout
        assert "--no-ancestor-mods" in result.stdout
        assert "--output-filename" in result.stdout
        assert "--analysis-filename" in result.stdout
        assert "--copybook-path" in result.stdout or "-c" in result.stdout
        assert "source" in result.stdout.lower()

    def test_stdout_output(self, simple_program_path):
        """Test output to stdout when no -o is specified."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # stdout should be valid JSON (map output)
        output_data = json.loads(result.stdout)
        assert "paragraphs" in output_data
        assert output_data["program_name"] == "SIMPLE-PROGRAM"

    def test_include_source_info(self, simple_program_path, tmp_path):
        """Test --include-source-info flag."""
        import subprocess
        import sys

        output_dir = tmp_path / "output"

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "-o", str(output_dir),
             "--include-source-info", "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # Check analysis file has source_info
        analysis_file = output_dir / "SIMPLE-PROGRAM-analysis.json"
        with open(analysis_file, "r") as f:
            analysis_data = json.load(f)

        assert "source_info" in analysis_data
        assert "file_name" in analysis_data["source_info"]


class TestOrphanModifications:
    """Tests for orphan modifications (statements outside paragraphs/sections)."""

    @pytest.fixture
    def orphan_analysis_data(self):
        """Create analysis data with PROCEDURE DIVISION orphan modifications."""
        return {
            "program_name": "ORPHAN-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PROCEDURE DIVISION": [
                    {
                        "variable": "INIT-FLAG",
                        "modification_type": "MOVE",
                        "line_number": 24,
                        "affected_records": ["WS-FLAGS"]
                    },
                    {
                        "variable": "STARTUP-COUNTER",
                        "modification_type": "MOVE",
                        "line_number": 25,
                        "affected_records": ["WS-COUNTERS"]
                    }
                ],
                "MAIN-PARA": [
                    {
                        "variable": "PROCESS-COUNT",
                        "modification_type": "ADD",
                        "line_number": 30,
                        "affected_records": ["WS-COUNTERS"]
                    }
                ]
            },
            "data_hierarchy": {
                "WS-FLAGS": ["WS-FLAGS"],
                "INIT-FLAG": ["WS-FLAGS", "INIT-FLAG"],
                "WS-COUNTERS": ["WS-COUNTERS"],
                "STARTUP-COUNTER": ["WS-COUNTERS", "STARTUP-COUNTER"],
                "PROCESS-COUNT": ["WS-COUNTERS", "PROCESS-COUNT"]
            }
        }

    def test_procedure_division_key_present(self, orphan_analysis_data):
        """Test that PROCEDURE DIVISION key appears in output."""
        mapper = ParagraphVariablesMapper(orphan_analysis_data)
        result = mapper.map()

        assert "PROCEDURE DIVISION" in result["paragraphs"]
        assert "MAIN-PARA" in result["paragraphs"]

    def test_orphan_variables_captured(self, orphan_analysis_data):
        """Test that variables modified outside paragraphs are captured."""
        mapper = ParagraphVariablesMapper(orphan_analysis_data)
        result = mapper.map()

        proc_div_vars = result["paragraphs"]["PROCEDURE DIVISION"]
        assert "INIT-FLAG" in proc_div_vars
        assert "STARTUP-COUNTER" in proc_div_vars

    def test_orphan_variable_details(self, orphan_analysis_data):
        """Test that orphan variable details are correct."""
        mapper = ParagraphVariablesMapper(orphan_analysis_data)
        result = mapper.map()

        init_flag = result["paragraphs"]["PROCEDURE DIVISION"]["INIT-FLAG"]
        assert init_flag["defined_in_record"] == "WS-FLAGS"
        assert init_flag["base_record"] == "WS-FLAGS"
        assert "line 24" in init_flag["explanation"]

    def test_orphan_included_in_summary(self, orphan_analysis_data):
        """Test that PROCEDURE DIVISION is counted in summary."""
        mapper = ParagraphVariablesMapper(orphan_analysis_data)
        result = mapper.map()

        # Should count PROCEDURE DIVISION + MAIN-PARA = 2
        assert result["summary"]["total_paragraphs_with_changes"] == 2
        # Should count unique variables: INIT-FLAG, STARTUP-COUNTER, PROCESS-COUNT = 3
        assert result["summary"]["total_unique_variables"] == 3


class TestWithRealFixtures:
    """Additional integration tests using real test fixtures."""

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

    def test_single_step_analysis_and_map(self, simple_program_path, tmp_path):
        """Test that single command produces both analysis and map."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        # Verify both files created
        analysis_file = tmp_path / "SIMPLE-PROGRAM-analysis.json"
        map_file = tmp_path / "SIMPLE-PROGRAM-paragraph-variables.json"

        assert analysis_file.exists()
        assert map_file.exists()

        # Verify content consistency
        with open(analysis_file, "r") as f:
            analysis_data = json.load(f)
        with open(map_file, "r") as f:
            map_data = json.load(f)

        assert analysis_data["program_name"] == map_data["program_name"]
        assert len(map_data["paragraphs"]) > 0
        assert map_data["summary"]["total_paragraphs_with_changes"] > 0

    def test_with_redefines_program(self, redefines_program_path, tmp_path):
        """Test with a program that has REDEFINES."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(redefines_program_path), "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        map_file = tmp_path / "REDEFINES-EXAMPLE-paragraph-variables.json"
        assert map_file.exists()

        with open(map_file, "r") as f:
            map_data = json.load(f)

        assert map_data["program_name"] == "REDEFINES-EXAMPLE"
        assert "paragraphs" in map_data

    def test_execution_time_reported(self, simple_program_path, tmp_path):
        """Test that execution times are included in output."""
        import subprocess
        import sys

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(simple_program_path), "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        map_file = tmp_path / "SIMPLE-PROGRAM-paragraph-variables.json"
        with open(map_file, "r") as f:
            map_data = json.load(f)

        assert "execution_time_seconds" in map_data
        assert map_data["execution_time_seconds"] >= 0

    def test_orphan_modifications_with_real_program(self, tmp_path):
        """Test that orphan modifications are captured with a real COBOL program."""
        import subprocess
        import sys

        # Use the ORDERMGMT.cbl program which has orphan modifications
        program_path = Path(__file__).parent.parent / "complex-cobol-source" / "ORDERMGMT.cbl"
        copybook_path = Path(__file__).parent.parent / "complex-cobol-source" / "copybooks"

        if not program_path.exists():
            pytest.skip("ORDERMGMT.cbl test fixture not available")

        result = subprocess.run(
            [sys.executable, "-m", "src", "paragraph-variables-map",
             str(program_path), "-c", str(copybook_path), "-o", str(tmp_path), "-q"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )

        assert result.returncode == 0

        map_file = tmp_path / "ORDERMGMT-paragraph-variables.json"
        assert map_file.exists()

        with open(map_file, "r") as f:
            map_data = json.load(f)

        # Should have PROCEDURE DIVISION key with orphan modifications
        assert "PROCEDURE DIVISION" in map_data["paragraphs"], \
            "PROCEDURE DIVISION key should be present for orphan modifications"

        # CUSTOMER-STATUS is modified at line 24 outside any paragraph
        assert "CUSTOMER-STATUS" in map_data["paragraphs"]["PROCEDURE DIVISION"], \
            "CUSTOMER-STATUS should be captured as orphan modification"

        # Regular paragraphs should also be present
        assert "INIT-CUSTOMER" in map_data["paragraphs"]
        assert "PROCESS-CONTRACT" in map_data["paragraphs"]
        assert "UPDATE-PAYMENT" in map_data["paragraphs"]


class TestFillerRedefinesPattern:
    """Tests for FILLER REDEFINES pattern handling."""

    @pytest.fixture
    def filler_redefines_with_copybook_data(self):
        """Create analysis data with FILLER REDEFINES pattern from copybook."""
        return {
            "program_name": "FILLER-COPYBOOK-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PROCESS-ORDER": [
                    {
                        "variable": "CUSTOMER-ID",
                        "modification_type": "MOVE",
                        "line_number": 27,
                        "affected_records": ["ORDER-BUFFER", "FILLER$1"]
                    }
                ]
            },
            "data_hierarchy": {
                "ORDER-BUFFER": ["ORDER-BUFFER"],
                "FILLER$1": ["FILLER$1"],
                "CUSTOMER-ID": ["FILLER$1", "CUSTOMER-ID"]
            },
            "memory_regions": {
                "CUSTOMER-ID": {
                    "start_offset": 0,
                    "size": 10,
                    "record_name": "FILLER$1",
                    "definition_line": 150  # Line in expanded source (copybook)
                },
                "ORDER-BUFFER": {
                    "start_offset": 0,
                    "size": 100,
                    "record_name": "ORDER-BUFFER"
                },
                "FILLER$1": {
                    "start_offset": 0,
                    "size": 100,
                    "record_name": "FILLER$1"
                }
            },
            "_line_mapping": {
                "150": {
                    "original_line": 15,
                    "is_copybook": True,
                    "source_file": "CUSTINFO"
                }
            },
            "_original_line_count": 50
        }

    def test_filler_with_copybook_shows_copybook_source(self, filler_redefines_with_copybook_data):
        """Test that FILLER from copybook shows copybook name in defined_in_record."""
        # Add REDEFINES relationship
        filler_redefines_with_copybook_data["sections_and_paragraphs"]["PROCESS-ORDER"][0]["affected_variables"] = [
            {
                "name": "CUSTOMER-ID",
                "overlap_type": "full",
                "redefines_chain": "FILLER$1 REDEFINES ORDER-BUFFER",
                "redefines_level": 1
            }
        ]

        mapper = ParagraphVariablesMapper(filler_redefines_with_copybook_data)
        result = mapper.map()

        customer_id = result["paragraphs"]["PROCESS-ORDER"]["CUSTOMER-ID"]

        # Should show copybook source in defined_in_record
        assert customer_id["defined_in_record"] == "FILLER (CUSTINFO copybook)"
        assert customer_id["base_record"] == "ORDER-BUFFER"

    def test_filler_redefines_includes_position(self, filler_redefines_with_copybook_data):
        """Test that FILLER REDEFINES includes position info."""
        # Add REDEFINES relationship
        filler_redefines_with_copybook_data["sections_and_paragraphs"]["PROCESS-ORDER"][0]["affected_variables"] = [
            {
                "name": "CUSTOMER-ID",
                "overlap_type": "full",
                "redefines_chain": "FILLER$1 REDEFINES ORDER-BUFFER",
                "redefines_level": 1
            }
        ]

        mapper = ParagraphVariablesMapper(filler_redefines_with_copybook_data)
        result = mapper.map()

        customer_id = result["paragraphs"]["PROCESS-ORDER"]["CUSTOMER-ID"]

        # Should have position info
        assert "position" in customer_id
        assert customer_id["position"]["start"] == 1  # 1-indexed
        assert customer_id["position"]["end"] == 10   # 1-indexed, inclusive


class TestNormalRedefinesUnchanged:
    """Tests that normal (non-FILLER) REDEFINES behavior is unchanged."""

    @pytest.fixture
    def normal_redefines_data(self):
        """Create analysis data with normal named REDEFINES."""
        return {
            "program_name": "NORMAL-REDEFINES-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "MODIFY-PARA": [
                    {
                        "variable": "TRAN-CUSTOMER-ID",
                        "modification_type": "MOVE",
                        "line_number": 300,
                        "affected_records": ["PAYMENT-DETAIL", "TRANSACTION-RECORD"],
                        "affected_variables": [
                            {
                                "name": "PAY-CASH",
                                "overlap_type": "direct_redefines_group",
                                "redefines_chain": "PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD",
                                "redefines_level": 1
                            }
                        ]
                    }
                ]
            },
            "data_hierarchy": {
                "TRANSACTION-RECORD": ["TRANSACTION-RECORD"],
                "TRAN-CUSTOMER-ID": ["TRANSACTION-RECORD", "TRAN-CUSTOMER-ID"],
                "PAYMENT-DETAIL": ["PAYMENT-DETAIL"],
                "PAY-CASH": ["PAYMENT-DETAIL", "PAY-CASH"]
            },
            "memory_regions": {
                "PAY-CASH": {
                    "start_offset": 10,
                    "size": 8,
                    "record_name": "PAYMENT-DETAIL"
                }
            }
        }

    def test_normal_redefines_has_position(self, normal_redefines_data):
        """Test that normal named REDEFINES also gets position info."""
        mapper = ParagraphVariablesMapper(normal_redefines_data)
        result = mapper.map()

        pay_cash = result["paragraphs"]["MODIFY-PARA"]["PAY-CASH"]

        # Should have position (all variables get position)
        assert "position" in pay_cash
        assert pay_cash["position"]["start"] == 11  # 1-indexed (offset 10 + 1)
        assert pay_cash["position"]["end"] == 18    # 1-indexed, inclusive

        # defined_in_record should be unchanged (not formatted as FILLER)
        assert pay_cash["defined_in_record"] == "PAYMENT-DETAIL"
        assert pay_cash["base_record"] == "TRANSACTION-RECORD"

    def test_normal_redefines_still_works(self, normal_redefines_data):
        """Test that normal REDEFINES resolution still works correctly."""
        mapper = ParagraphVariablesMapper(normal_redefines_data)
        result = mapper.map()

        para_vars = result["paragraphs"]["MODIFY-PARA"]

        # Direct modification
        assert "TRAN-CUSTOMER-ID" in para_vars

        # REDEFINES-affected variable
        assert "PAY-CASH" in para_vars


class TestFillerWithoutCopybook:
    """Tests for FILLER patterns from main source (not copybook)."""

    @pytest.fixture
    def filler_main_source_data(self):
        """Create analysis data with FILLER REDEFINES from main source."""
        return {
            "program_name": "FILLER-MAIN-TEST",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PROCESS-DATA": [
                    {
                        "variable": "FIELD-A",
                        "modification_type": "MOVE",
                        "line_number": 100,
                        "affected_records": ["MAIN-BUFFER", "FILLER$2"],
                        "affected_variables": [
                            {
                                "name": "FIELD-A",
                                "overlap_type": "full",
                                "redefines_chain": "FILLER$2 REDEFINES MAIN-BUFFER",
                                "redefines_level": 1
                            }
                        ]
                    }
                ]
            },
            "data_hierarchy": {
                "MAIN-BUFFER": ["MAIN-BUFFER"],
                "FILLER$2": ["FILLER$2"],
                "FIELD-A": ["FILLER$2", "FIELD-A"]
            },
            "memory_regions": {
                "FIELD-A": {
                    "start_offset": 5,
                    "size": 20,
                    "record_name": "FILLER$2",
                    "definition_line": 30  # Line in main source
                }
            },
            "_line_mapping": {
                "30": {
                    "original_line": 30,
                    "is_copybook": False,
                    "source_file": "MAIN"
                }
            },
            "_original_line_count": 100
        }

    def test_filler_without_copybook_shows_filler_only(self, filler_main_source_data):
        """Test that FILLER from main source shows just 'FILLER'."""
        mapper = ParagraphVariablesMapper(filler_main_source_data)
        result = mapper.map()

        field_a = result["paragraphs"]["PROCESS-DATA"]["FIELD-A"]

        # Should show just "FILLER" without copybook suffix
        assert field_a["defined_in_record"] == "FILLER"
        assert field_a["base_record"] == "MAIN-BUFFER"

    def test_filler_without_copybook_still_has_position(self, filler_main_source_data):
        """Test that FILLER from main source still includes position."""
        mapper = ParagraphVariablesMapper(filler_main_source_data)
        result = mapper.map()

        field_a = result["paragraphs"]["PROCESS-DATA"]["FIELD-A"]

        # Should still have position info
        assert "position" in field_a
        assert field_a["position"]["start"] == 6   # 1-indexed (offset 5 + 1)
        assert field_a["position"]["end"] == 25    # 1-indexed, inclusive (5 + 20)


class TestFillerEdgeCases:
    """Tests for edge cases with FILLER handling."""

    def test_filler_without_line_mapping(self):
        """Test FILLER when no line mapping exists."""
        analysis_data = {
            "program_name": "FILLER-NO-MAPPING",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PARA-1": [
                    {
                        "variable": "VAR-X",
                        "modification_type": "MOVE",
                        "line_number": 50,
                        "affected_records": ["BUFFER", "FILLER$3"],
                        "affected_variables": [
                            {
                                "name": "VAR-X",
                                "overlap_type": "full",
                                "redefines_chain": "FILLER$3 REDEFINES BUFFER",
                                "redefines_level": 1
                            }
                        ]
                    }
                ]
            },
            "data_hierarchy": {
                "BUFFER": ["BUFFER"],
                "FILLER$3": ["FILLER$3"],
                "VAR-X": ["FILLER$3", "VAR-X"]
            },
            "memory_regions": {
                "VAR-X": {
                    "start_offset": 0,
                    "size": 15,
                    "record_name": "FILLER$3"
                    # No definition_line
                }
            }
            # No _line_mapping
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        result = mapper.map()

        var_x = result["paragraphs"]["PARA-1"]["VAR-X"]

        # Should fallback to just "FILLER" without copybook info
        assert var_x["defined_in_record"] == "FILLER"
        assert var_x["base_record"] == "BUFFER"

    def test_filler_same_as_base_still_has_position(self):
        """Test that FILLER without REDEFINES relationship still gets position."""
        analysis_data = {
            "program_name": "FILLER-NO-REDEFINES",
            "analysis_date": "2026-01-25T10:00:00Z",
            "sections_and_paragraphs": {
                "PARA-1": [
                    {
                        "variable": "VAR-Y",
                        "modification_type": "MOVE",
                        "line_number": 60,
                        "affected_records": ["FILLER$4"]
                        # No affected_variables - no REDEFINES relationship
                    }
                ]
            },
            "data_hierarchy": {
                "FILLER$4": ["FILLER$4"],
                "VAR-Y": ["FILLER$4", "VAR-Y"]
            },
            "memory_regions": {
                "VAR-Y": {
                    "start_offset": 0,
                    "size": 10,
                    "record_name": "FILLER$4"
                }
            }
        }

        mapper = ParagraphVariablesMapper(analysis_data)
        result = mapper.map()

        var_y = result["paragraphs"]["PARA-1"]["VAR-Y"]

        # All variables get position info now
        assert "position" in var_y
        assert var_y["position"]["start"] == 1   # 1-indexed
        assert var_y["position"]["end"] == 10    # 1-indexed, inclusive

        # Should still format FILLER name
        assert var_y["defined_in_record"] == "FILLER"
