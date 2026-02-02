"""Tests for the public API module."""

import pytest
from pathlib import Path

from cobol_ast.api import (
    analyze_paragraph_variables,
    AnalysisOptions,
    AnalysisResult,
    AnalysisError,
    get_data_division_tree,
    TreeOptions,
    DataDivisionTree,
    DataItemNode,
    DataDivisionSection,
    _build_copybook_line_map,
    _build_variable_index,
    analyze_with_tree,
    CombinedOptions,
    CombinedResult,
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

        # Should include redefines, ancestor mods, and source info by default
        assert result.source_info is not None
        assert result.source_info["file_name"] == "simple_program.cob"

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

    def test_line_numbers_are_original_not_expanded(self):
        """Test that line numbers refer to original source, not expanded source."""
        source_path = FIXTURES_DIR / "copybook_main.cob"
        copybook_path = FIXTURES_DIR / "copybooks"

        options = AnalysisOptions(
            copybook_paths=[copybook_path],
        )
        result = analyze_paragraph_variables(source_path, options)

        # Get the original source line count
        original_line_count = len(source_path.read_text().splitlines())

        # Check line_number in sections_and_paragraphs
        for section_name, modifications in result.analysis.get("sections_and_paragraphs", {}).items():
            for mod in modifications:
                line_num = mod.get("line_number")
                assert line_num is not None, f"Missing line_number in {section_name}"
                # Line number should be within original source file range
                assert line_num <= original_line_count, (
                    f"Line number {line_num} exceeds original source line count "
                    f"({original_line_count}) for {mod.get('variable')} in {section_name}"
                )

        # Check definition_line in memory_regions
        for var_name, region_info in result.analysis.get("memory_regions", {}).items():
            if "definition_line" in region_info:
                line_num = region_info["definition_line"]
                # Definition line should be within original source file range
                assert line_num <= original_line_count, (
                    f"Definition line {line_num} exceeds original source line count "
                    f"({original_line_count}) for variable {var_name}"
                )

    def test_exact_line_numbers_for_paragraph_modifications(self):
        """Test that line numbers for modifications match exact source locations.

        Regression test to ensure line_number in sections_and_paragraphs
        correctly maps to the original source file line where the modification
        statement appears.

        Uses all_modifications.cob which has known line numbers:
        - Line 34: MOVE 100 TO WS-NUM-A (in TEST-MOVE-PARA)
        - Line 39: COMPUTE WS-NUM-C = WS-NUM-A + WS-NUM-B (in TEST-COMPUTE-PARA)
        - Line 43: ADD 10 TO WS-NUM-A (in TEST-ADD-PARA)
        """
        source_path = FIXTURES_DIR / "all_modifications.cob"
        result = analyze_paragraph_variables(source_path)

        sections_and_paragraphs = result.analysis.get("sections_and_paragraphs", {})

        # Find TEST-MOVE-PARA modifications
        test_move_mods = sections_and_paragraphs.get("TEST-MOVE-PARA", [])
        move_to_num_a = next(
            (m for m in test_move_mods
             if m["variable"] == "WS-NUM-A" and m["modification_type"] == "MOVE"),
            None
        )
        assert move_to_num_a is not None, "MOVE to WS-NUM-A not found in TEST-MOVE-PARA"
        assert move_to_num_a["line_number"] == 34, (
            f"MOVE to WS-NUM-A should be at line 34, got {move_to_num_a['line_number']}"
        )

        # Find TEST-COMPUTE-PARA modifications
        test_compute_mods = sections_and_paragraphs.get("TEST-COMPUTE-PARA", [])
        compute_num_c = next(
            (m for m in test_compute_mods
             if m["variable"] == "WS-NUM-C" and m["modification_type"] == "COMPUTE"),
            None
        )
        assert compute_num_c is not None, "COMPUTE to WS-NUM-C not found in TEST-COMPUTE-PARA"
        assert compute_num_c["line_number"] == 39, (
            f"COMPUTE to WS-NUM-C should be at line 39, got {compute_num_c['line_number']}"
        )

        # Find TEST-ADD-PARA modifications
        test_add_mods = sections_and_paragraphs.get("TEST-ADD-PARA", [])
        add_to_num_a = next(
            (m for m in test_add_mods
             if m["variable"] == "WS-NUM-A" and m["modification_type"] == "ADD"),
            None
        )
        assert add_to_num_a is not None, "ADD to WS-NUM-A not found in TEST-ADD-PARA"
        assert add_to_num_a["line_number"] == 43, (
            f"ADD to WS-NUM-A should be at line 43, got {add_to_num_a['line_number']}"
        )


class TestAnalysisOptions:
    """Tests for the AnalysisOptions dataclass."""

    def test_default_values(self):
        """Test that default values are set correctly."""
        options = AnalysisOptions()

        assert options.copybook_paths is None  # Source dir is added automatically
        assert options.resolve_copies is True
        assert options.include_redefines is True
        assert options.include_ancestor_mods is True
        assert options.include_source_info is True

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
        assert hasattr(result, "variable_index")
        assert hasattr(result, "execution_time_seconds")
        assert hasattr(result, "source_info")

    def test_program_name_matches(self):
        """Test that program_name matches in result and outputs."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        assert result.program_name == result.analysis.get("program_name")
        assert result.program_name == result.paragraph_variables.get("program_name")

    def test_variable_index_structure(self):
        """Test that variable_index has the expected nested structure."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # variable_index should be a dict
        assert isinstance(result.variable_index, dict)

        # Each key should be a record name (defined_in_record)
        for record_name, positions in result.variable_index.items():
            assert isinstance(record_name, str)
            assert isinstance(positions, dict)

            # Each position key should be "start:end"
            for pos_key, entry in positions.items():
                assert isinstance(pos_key, str)
                assert ":" in pos_key  # Format is "start:end"

                # Entry should have variable_name, modifying_paragraphs, and accessing_paragraphs
                assert "variable_name" in entry
                assert "modifying_paragraphs" in entry
                assert "accessing_paragraphs" in entry
                assert isinstance(entry["variable_name"], str)
                assert isinstance(entry["modifying_paragraphs"], list)
                assert isinstance(entry["accessing_paragraphs"], list)

    def test_variable_index_contains_modified_variables(self):
        """Test that variable_index contains variables from paragraph_variables."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # Get all variables from paragraph_variables
        all_vars_in_paragraphs = set()
        for para_vars in result.paragraph_variables.get("paragraphs", {}).values():
            all_vars_in_paragraphs.update(para_vars.keys())

        # Get all variables from variable_index
        all_vars_in_index = set()
        for positions in result.variable_index.values():
            for entry in positions.values():
                all_vars_in_index.add(entry["variable_name"])

        # All variables with positions in paragraph_variables should be in index
        # (Some may not have positions and thus not be in the index)
        assert len(all_vars_in_index) > 0

    def test_variable_index_lookup_matches_paragraph_variables(self):
        """Test that looking up via variable_index gives correct paragraphs."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # For each entry in variable_index, verify the modifying paragraphs match
        for record_name, positions in result.variable_index.items():
            for pos_key, entry in positions.items():
                var_name = entry["variable_name"]
                modifying_in_index = set(entry["modifying_paragraphs"])

                # Find all paragraphs that modify this variable
                modifying_from_original = set()
                for para_name, para_vars in result.paragraph_variables.get("paragraphs", {}).items():
                    if var_name in para_vars:
                        var_info = para_vars[var_name]
                        # Check if the record and position match
                        if var_info.get("defined_in_record") == record_name:
                            pos = var_info.get("position", {})
                            if f"{pos.get('start')}:{pos.get('end')}" == pos_key:
                                # Only count as modifying if it has modification_lines
                                if var_info.get("modification_lines"):
                                    modifying_from_original.add(para_name)

                # The sets should match
                assert modifying_in_index == modifying_from_original, (
                    f"Mismatch for {var_name} at {record_name}[{pos_key}]: "
                    f"index={modifying_in_index}, original={modifying_from_original}"
                )


class TestVariableIndexLinking:
    """Tests for linking DataDivisionTree nodes to paragraphs via variable_index."""

    def test_link_data_division_tree_to_paragraphs(self):
        """Test the complete workflow: select node from tree, find modifying paragraphs."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        # Get both outputs
        tree = get_data_division_tree(source_path)
        result = analyze_paragraph_variables(source_path)

        # Find a node in the tree that has modifications
        # Look for WS-EMP-ID in WS-EMPLOYEE-RECORD
        emp_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE-RECORD":
                emp_record = record
                break

        assert emp_record is not None

        # Find WS-EMP-ID child
        emp_id_node = None
        for child in emp_record.children:
            if child.name == "WS-EMP-ID":
                emp_id_node = child
                break

        # If we found the node and it has position info, try the lookup
        if emp_id_node and emp_id_node.position:
            # Use the defined_in_record and position to look up
            record_key = emp_id_node.defined_in_record
            pos_key = f"{emp_id_node.position['start']}:{emp_id_node.position['end']}"

            # Look up in variable_index
            entry = result.variable_index.get(record_key, {}).get(pos_key)

            # If there's an entry, verify structure
            if entry:
                assert "variable_name" in entry
                assert "modifying_paragraphs" in entry
                assert "accessing_paragraphs" in entry
                assert isinstance(entry["modifying_paragraphs"], list)
                assert isinstance(entry["accessing_paragraphs"], list)

    def test_link_returns_all_modifying_paragraphs(self):
        """Test that linking returns all paragraphs that modify a variable."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)
        result = analyze_paragraph_variables(source_path)

        # Helper to find modifying paragraphs for a node
        def find_paragraphs_for_node(node):
            if not node.defined_in_record or not node.position:
                return None
            pos_key = f"{node.position['start']}:{node.position['end']}"
            entry = result.variable_index.get(node.defined_in_record, {}).get(pos_key)
            return entry["modifying_paragraphs"] if entry else []

        # Traverse tree and check each node
        modified_vars_found = []
        for record in tree.all_records:
            paragraphs = find_paragraphs_for_node(record)
            if paragraphs:
                modified_vars_found.append((record.name, paragraphs))

            # Check children recursively
            def check_children(node):
                for child in node.children:
                    paragraphs = find_paragraphs_for_node(child)
                    if paragraphs:
                        modified_vars_found.append((child.name, paragraphs))
                    check_children(child)

            check_children(record)

        # We should find some modified variables
        assert len(modified_vars_found) > 0

    def test_redefines_records_have_separate_entries(self):
        """Test that REDEFINES records have their own entries in variable_index."""
        source_path = FIXTURES_DIR / "redefines_program.cob"

        result = analyze_paragraph_variables(source_path)

        # Check that there are entries for EMPLOYEE-RECORD (which REDEFINES INPUT-RECORD)
        # and potentially for INPUT-RECORD itself
        record_names_in_index = set(result.variable_index.keys())

        # We should have at least some records in the index
        assert len(record_names_in_index) > 0


class TestBuildVariableIndex:
    """Tests for the _build_variable_index helper function."""

    def test_builds_index_from_paragraph_variables(self):
        """Test that index is built correctly from paragraph_variables structure."""
        paragraph_variables = {
            "paragraphs": {
                "PARA-1": {
                    "VAR-A": {
                        "defined_in_record": "RECORD-1",
                        "base_record": "RECORD-1",
                        "position": {"start": 1, "end": 10},
                        "modification_lines": [10],
                        "explanation": "direct modification"
                    },
                    "VAR-B": {
                        "defined_in_record": "RECORD-1",
                        "base_record": "RECORD-1",
                        "position": {"start": 11, "end": 20},
                        "modification_lines": [15],
                        "explanation": "direct modification"
                    }
                },
                "PARA-2": {
                    "VAR-A": {
                        "defined_in_record": "RECORD-1",
                        "base_record": "RECORD-1",
                        "position": {"start": 1, "end": 10},
                        "modification_lines": [20],
                        "explanation": "direct modification"
                    }
                }
            }
        }

        index = _build_variable_index(paragraph_variables)

        # Should have RECORD-1
        assert "RECORD-1" in index

        # Should have two position keys
        assert "1:10" in index["RECORD-1"]
        assert "11:20" in index["RECORD-1"]

        # VAR-A at 1:10 should have both paragraphs as modifying
        assert index["RECORD-1"]["1:10"]["variable_name"] == "VAR-A"
        assert set(index["RECORD-1"]["1:10"]["modifying_paragraphs"]) == {"PARA-1", "PARA-2"}
        assert index["RECORD-1"]["1:10"]["accessing_paragraphs"] == []

        # VAR-B at 11:20 should have only PARA-1 as modifying
        assert index["RECORD-1"]["11:20"]["variable_name"] == "VAR-B"
        assert index["RECORD-1"]["11:20"]["modifying_paragraphs"] == ["PARA-1"]
        assert index["RECORD-1"]["11:20"]["accessing_paragraphs"] == []

    def test_handles_multiple_records(self):
        """Test that index correctly separates different records."""
        paragraph_variables = {
            "paragraphs": {
                "PARA-1": {
                    "VAR-X": {
                        "defined_in_record": "REC-A",
                        "base_record": "REC-A",
                        "position": {"start": 1, "end": 5},
                        "modification_lines": [10],
                        "explanation": "direct"
                    },
                    "VAR-Y": {
                        "defined_in_record": "REC-B",
                        "base_record": "REC-B",
                        "position": {"start": 1, "end": 5},
                        "modification_lines": [15],
                        "explanation": "direct"
                    }
                }
            }
        }

        index = _build_variable_index(paragraph_variables)

        # Should have separate entries for each record
        assert "REC-A" in index
        assert "REC-B" in index

        # Same position key but in different records
        assert "1:5" in index["REC-A"]
        assert "1:5" in index["REC-B"]

        assert index["REC-A"]["1:5"]["variable_name"] == "VAR-X"
        assert index["REC-B"]["1:5"]["variable_name"] == "VAR-Y"

    def test_skips_variables_without_position(self):
        """Test that variables without position info are skipped."""
        paragraph_variables = {
            "paragraphs": {
                "PARA-1": {
                    "VAR-WITH-POS": {
                        "defined_in_record": "REC-1",
                        "base_record": "REC-1",
                        "position": {"start": 1, "end": 10},
                        "modification_lines": [10],
                        "explanation": "direct"
                    },
                    "VAR-NO-POS": {
                        "defined_in_record": "REC-1",
                        "base_record": "REC-1",
                        "modification_lines": [15],
                        "explanation": "direct"
                        # No position key
                    }
                }
            }
        }

        index = _build_variable_index(paragraph_variables)

        # Only the variable with position should be in index
        assert "REC-1" in index
        assert "1:10" in index["REC-1"]
        assert index["REC-1"]["1:10"]["variable_name"] == "VAR-WITH-POS"

        # No other entries for REC-1
        assert len(index["REC-1"]) == 1

    def test_empty_paragraphs(self):
        """Test handling of empty paragraph_variables."""
        paragraph_variables = {"paragraphs": {}}

        index = _build_variable_index(paragraph_variables)

        assert index == {}

    def test_no_duplicate_paragraphs(self):
        """Test that same paragraph is not added twice for same variable."""
        # This tests the deduplication logic
        paragraph_variables = {
            "paragraphs": {
                "PARA-1": {
                    "VAR-A": {
                        "defined_in_record": "REC-1",
                        "base_record": "REC-1",
                        "position": {"start": 1, "end": 10},
                        "modification_lines": [10],
                        "explanation": "direct"
                    }
                }
            }
        }

        index = _build_variable_index(paragraph_variables)

        # Should have exactly one paragraph entry in modifying_paragraphs
        assert index["REC-1"]["1:10"]["modifying_paragraphs"] == ["PARA-1"]
        assert index["REC-1"]["1:10"]["accessing_paragraphs"] == []

    def test_uses_raw_filler_format(self):
        """Test that FILLER records use raw format (FILLER$n) in index."""
        paragraph_variables = {
            "paragraphs": {
                "PARA-1": {
                    "VAR-IN-FILLER": {
                        "defined_in_record": "FILLER$1",  # Raw format
                        "base_record": "MAIN-REC",
                        "position": {"start": 1, "end": 10},
                        "modification_lines": [10],
                        "explanation": "direct"
                    },
                    "VAR-NORMAL": {
                        "defined_in_record": "NORMAL-REC",
                        "base_record": "NORMAL-REC",
                        "position": {"start": 1, "end": 5},
                        "modification_lines": [15],
                        "explanation": "direct"
                    }
                }
            }
        }

        index = _build_variable_index(paragraph_variables)

        # FILLER variable should use raw FILLER$n format as key
        assert "FILLER$1" in index
        assert index["FILLER$1"]["1:10"]["variable_name"] == "VAR-IN-FILLER"

        # Normal variable should use defined_in_record as key
        assert "NORMAL-REC" in index
        assert index["NORMAL-REC"]["1:5"]["variable_name"] == "VAR-NORMAL"


class TestGetDataDivisionTree:
    """Tests for the get_data_division_tree function."""

    def test_basic_tree_structure(self):
        """Test basic tree structure with simple program."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        assert isinstance(tree, DataDivisionTree)
        assert tree.program_name == "SIMPLE-PROGRAM"
        assert tree.execution_time_seconds > 0
        assert len(tree.all_records) > 0
        assert len(tree.sections) > 0

    def test_returns_expected_records(self):
        """Test that the tree contains expected records."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # Get record names
        record_names = [r.name for r in tree.all_records]

        # simple_program.cob has these Level 01 records
        assert "WS-EMPLOYEE-RECORD" in record_names
        assert "WS-COUNTERS" in record_names
        assert "WS-FLAGS" in record_names

    def test_tree_hierarchy(self):
        """Test that tree preserves hierarchical structure."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # Find WS-EMPLOYEE-RECORD
        emp_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE-RECORD":
                emp_record = record
                break

        assert emp_record is not None
        assert emp_record.level == 1
        assert emp_record.is_group is True
        assert len(emp_record.children) > 0

        # Check children exist
        child_names = [c.name for c in emp_record.children]
        assert "WS-EMP-ID" in child_names
        assert "WS-EMP-NAME" in child_names

    def test_88_level_inclusion(self):
        """Test that 88-level items are included by default."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # Find WS-FLAGS record which has 88-level items
        flags_record = None
        for record in tree.all_records:
            if record.name == "WS-FLAGS":
                flags_record = record
                break

        assert flags_record is not None

        # WS-EOF-FLAG should have 88-level children
        eof_flag = None
        for child in flags_record.children:
            if child.name == "WS-EOF-FLAG":
                eof_flag = child
                break

        assert eof_flag is not None
        assert len(eof_flag.children) > 0

        # Check for 88-level items
        has_88_level = any(c.level == 88 for c in eof_flag.children)
        assert has_88_level

    def test_88_level_exclusion(self):
        """Test that 88-level items can be excluded."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(include_88_levels=False)
        )

        # Find WS-FLAGS record
        flags_record = None
        for record in tree.all_records:
            if record.name == "WS-FLAGS":
                flags_record = record
                break

        assert flags_record is not None

        # WS-EOF-FLAG should have no 88-level children
        eof_flag = None
        for child in flags_record.children:
            if child.name == "WS-EOF-FLAG":
                eof_flag = child
                break

        assert eof_flag is not None
        # Should have no children (88-levels were excluded)
        assert len(eof_flag.children) == 0

    def test_filler_inclusion(self):
        """Test that FILLER items are included by default."""
        source_path = FIXTURES_DIR / "redefines_program.cob"

        tree = get_data_division_tree(source_path)

        # Look for FILLER in any record
        def has_filler(node):
            if node.is_filler:
                return True
            for child in node.children:
                if has_filler(child):
                    return True
            return False

        found_filler = any(has_filler(r) for r in tree.all_records)
        assert found_filler

    def test_filler_exclusion(self):
        """Test that FILLER items can be excluded."""
        source_path = FIXTURES_DIR / "redefines_program.cob"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(include_filler=False)
        )

        # Look for FILLER in any record
        def has_filler(node):
            if node.is_filler:
                return True
            for child in node.children:
                if has_filler(child):
                    return True
            return False

        found_filler = any(has_filler(r) for r in tree.all_records)
        assert not found_filler

    def test_position_calculation(self):
        """Test that memory positions are calculated."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # Find WS-EMPLOYEE-RECORD
        emp_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE-RECORD":
                emp_record = record
                break

        assert emp_record is not None
        assert emp_record.position is not None
        assert "start" in emp_record.position
        assert "end" in emp_record.position
        assert "size" in emp_record.position
        assert emp_record.position["size"] > 0

    def test_section_grouping(self):
        """Test that records are grouped by section."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # Should have at least WORKING-STORAGE section
        section_names = [s.name for s in tree.sections]
        assert "WORKING-STORAGE" in section_names

        # WORKING-STORAGE should contain records
        ws_section = None
        for section in tree.sections:
            if section.name == "WORKING-STORAGE":
                ws_section = section
                break

        assert ws_section is not None
        assert len(ws_section.records) > 0

    def test_redefines_handling(self):
        """Test that REDEFINES information is preserved."""
        source_path = FIXTURES_DIR / "redefines_program.cob"

        tree = get_data_division_tree(source_path)

        # Find EMPLOYEE-RECORD which redefines INPUT-RECORD
        emp_record = None
        for record in tree.all_records:
            if record.name == "EMPLOYEE-RECORD":
                emp_record = record
                break

        assert emp_record is not None
        assert emp_record.redefines == "INPUT-RECORD"

    def test_occurs_handling(self):
        """Test that OCCURS information is preserved."""
        # simple_program.cob doesn't have OCCURS, so we'll just verify
        # the field exists and is None when not present
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # Check first record - should have occurs as None
        record = tree.all_records[0]
        assert record.occurs is None

    def test_source_info_included(self):
        """Test that source_info is included by default."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        assert tree.source_info is not None
        assert tree.source_info["file_name"] == "simple_program.cob"
        assert tree.source_info["source_format"] == "fixed"
        assert tree.source_info["lines_count"] > 0

    def test_source_info_excluded(self):
        """Test that source_info can be excluded."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(include_source_info=False)
        )

        assert tree.source_info is None

    def test_summary_statistics(self):
        """Test that summary statistics are computed."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        assert "total_records" in tree.summary
        assert "total_items" in tree.summary
        assert "group_items" in tree.summary
        assert "elementary_items" in tree.summary
        assert "filler_items" in tree.summary
        assert "level_88_items" in tree.summary

        # Values should be reasonable
        assert tree.summary["total_records"] == len(tree.all_records)
        assert tree.summary["total_items"] > 0
        assert tree.summary["elementary_items"] > 0

    def test_to_dict_serialization(self):
        """Test that to_dict produces valid JSON-serializable structure."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        tree_dict = tree.to_dict()

        assert isinstance(tree_dict, dict)
        assert "program_name" in tree_dict
        assert "sections" in tree_dict
        assert "all_records" in tree_dict
        assert "summary" in tree_dict
        assert "execution_time_seconds" in tree_dict

        # Should be JSON serializable
        import json
        json_str = json.dumps(tree_dict)
        assert len(json_str) > 0

    def test_file_not_found(self):
        """Test that FileNotFoundError is raised for missing files."""
        source_path = FIXTURES_DIR / "nonexistent.cob"

        with pytest.raises(FileNotFoundError) as exc_info:
            get_data_division_tree(source_path)

        assert "nonexistent.cob" in str(exc_info.value)

    def test_directory_instead_of_file(self):
        """Test that FileNotFoundError is raised when path is a directory."""
        with pytest.raises(FileNotFoundError) as exc_info:
            get_data_division_tree(FIXTURES_DIR)

        assert "not a file" in str(exc_info.value)

    def test_copybook_source_tracking(self):
        """Test that copybook source is tracked for items from copybooks."""
        source_path = FIXTURES_DIR / "copybook_main.cob"
        copybook_path = FIXTURES_DIR / "copybooks"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(copybook_paths=[copybook_path])
        )

        # Find WS-EMPLOYEE record (from EMPLOYEE-CPY copybook)
        emp_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE":
                emp_record = record
                break

        assert emp_record is not None
        assert emp_record.copybook_source == "EMPLOYEE-CPY"

        # Children should also have copybook source
        for child in emp_record.children:
            assert child.copybook_source == "EMPLOYEE-CPY"

    def test_line_numbers_refer_to_root_file(self):
        """Test that line numbers refer to the root file, not expanded source."""
        source_path = FIXTURES_DIR / "copybook_main.cob"
        copybook_path = FIXTURES_DIR / "copybooks"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(copybook_paths=[copybook_path])
        )

        # Find WS-EMPLOYEE record (from EMPLOYEE-CPY copybook)
        # COPY EMPLOYEE-CPY is on line 7 in copybook_main.cob
        emp_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE":
                emp_record = record
                break

        assert emp_record is not None
        # Line number should be the COPY statement line in root file (line 7)
        assert emp_record.line_number == 7

        # Children should also have line 7 (the COPY statement line)
        for child in emp_record.children:
            assert child.line_number == 7

    def test_line_numbers_for_multiple_copybooks(self):
        """Test that items from different copybooks have correct COPY statement lines."""
        source_path = FIXTURES_DIR / "copybook_main.cob"
        copybook_path = FIXTURES_DIR / "copybooks"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(copybook_paths=[copybook_path])
        )

        # copybook_main.cob has:
        # Line 7: COPY EMPLOYEE-CPY.
        # Line 8: COPY COUNTERS-CPY.

        emp_record = None
        ctr_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE":
                emp_record = record
            elif record.name == "WS-COUNTERS":
                ctr_record = record

        # WS-EMPLOYEE should have line 7 (COPY EMPLOYEE-CPY)
        if emp_record:
            assert emp_record.line_number == 7

        # WS-COUNTERS should have line 8 (COPY COUNTERS-CPY)
        if ctr_record:
            assert ctr_record.line_number == 8

    def test_line_numbers_for_multiline_copy_with_replacing(self):
        """Test that multi-line COPY statements with REPLACING map to correct line.

        Regression test for bug where COPY statements spanning multiple lines
        (e.g., COPY ... REPLACING ... on subsequent lines) were not being
        detected because the regex required a trailing period on the same line.
        Items from such copybooks should map to the line where COPY starts.
        """
        source_path = FIXTURES_DIR / "multiline_copy_program.cob"
        copybook_path = FIXTURES_DIR / "copybooks"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(copybook_paths=[copybook_path])
        )

        # multiline_copy_program.cob has:
        # Line 6: 01 WS-SIMPLE-ITEM (main source)
        # Line 7: COPY MULTILINE-CPY
        # Line 8:     REPLACING ==ORIGINAL-RECORD==
        # Line 9:            BY ==REPLACED-RECORD==.
        # Line 10: 01 WS-ANOTHER-ITEM (main source)

        replaced_record = None
        simple_item = None
        another_item = None
        for record in tree.all_records:
            if record.name == "REPLACED-RECORD":
                replaced_record = record
            elif record.name == "WS-SIMPLE-ITEM":
                simple_item = record
            elif record.name == "WS-ANOTHER-ITEM":
                another_item = record

        # REPLACED-RECORD comes from copybook, should have line 7 (COPY statement)
        assert replaced_record is not None, "REPLACED-RECORD not found in tree"
        assert replaced_record.line_number == 7, (
            f"REPLACED-RECORD should be at line 7 (COPY statement), "
            f"got {replaced_record.line_number}"
        )

        # Children of REPLACED-RECORD should also have line 7
        for child in replaced_record.children:
            assert child.line_number == 7, (
                f"Child {child.name} should be at line 7, got {child.line_number}"
            )

        # WS-SIMPLE-ITEM is from main source, should be at line 6
        assert simple_item is not None
        assert simple_item.line_number == 6

        # WS-ANOTHER-ITEM is from main source, should be at line 10
        assert another_item is not None
        assert another_item.line_number == 10

    def test_line_numbers_for_main_source_items(self):
        """Test that items from main source have their original line numbers."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # Find WS-EMPLOYEE-RECORD which is defined in the main source
        emp_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE-RECORD":
                emp_record = record
                break

        assert emp_record is not None
        # Should have its original line number from the source file
        assert emp_record.line_number > 0
        # Line number should be reasonable (not an expanded line number in thousands)
        assert emp_record.line_number < 100

    def test_defined_in_record_for_level_01(self):
        """Test that Level 01 records have defined_in_record equal to their own name."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # All Level 01 records should have defined_in_record equal to their name
        for record in tree.all_records:
            assert record.defined_in_record == record.name, (
                f"Level 01 record {record.name} should have defined_in_record={record.name}, "
                f"got {record.defined_in_record}"
            )

    def test_defined_in_record_for_nested_items(self):
        """Test that nested items have defined_in_record equal to their parent Level 01."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)

        # Find WS-EMPLOYEE-RECORD and check its children
        emp_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE-RECORD":
                emp_record = record
                break

        assert emp_record is not None

        # All children should have defined_in_record = "WS-EMPLOYEE-RECORD"
        def check_children(node, expected_record):
            for child in node.children:
                assert child.defined_in_record == expected_record, (
                    f"Child {child.name} should have defined_in_record={expected_record}, "
                    f"got {child.defined_in_record}"
                )
                check_children(child, expected_record)

        check_children(emp_record, "WS-EMPLOYEE-RECORD")

    def test_defined_in_record_for_redefines_items(self):
        """Test that items in REDEFINES records have correct defined_in_record.

        Items in a record that REDEFINEs another should have defined_in_record
        equal to the REDEFINING record name, not the base record.
        """
        source_path = FIXTURES_DIR / "redefines_program.cob"

        tree = get_data_division_tree(source_path)

        # Find EMPLOYEE-RECORD which redefines INPUT-RECORD
        emp_record = None
        input_record = None
        for record in tree.all_records:
            if record.name == "EMPLOYEE-RECORD":
                emp_record = record
            elif record.name == "INPUT-RECORD":
                input_record = record

        assert emp_record is not None
        assert input_record is not None
        assert emp_record.redefines == "INPUT-RECORD"

        # EMPLOYEE-RECORD itself should have defined_in_record = "EMPLOYEE-RECORD"
        assert emp_record.defined_in_record == "EMPLOYEE-RECORD"

        # Children of EMPLOYEE-RECORD should have defined_in_record = "EMPLOYEE-RECORD"
        # NOT "INPUT-RECORD" (the base record)
        def check_children(node, expected_record):
            for child in node.children:
                assert child.defined_in_record == expected_record, (
                    f"Child {child.name} of REDEFINES record should have "
                    f"defined_in_record={expected_record}, got {child.defined_in_record}"
                )
                check_children(child, expected_record)

        check_children(emp_record, "EMPLOYEE-RECORD")

    def test_defined_in_record_with_copybooks(self):
        """Test that defined_in_record is correctly set for items from copybooks."""
        source_path = FIXTURES_DIR / "copybook_main.cob"
        copybook_path = FIXTURES_DIR / "copybooks"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(copybook_paths=[copybook_path])
        )

        # Find WS-EMPLOYEE record (from EMPLOYEE-CPY copybook)
        emp_record = None
        for record in tree.all_records:
            if record.name == "WS-EMPLOYEE":
                emp_record = record
                break

        assert emp_record is not None
        assert emp_record.copybook_source == "EMPLOYEE-CPY"
        assert emp_record.defined_in_record == "WS-EMPLOYEE"

        # All children should also have defined_in_record = "WS-EMPLOYEE"
        for child in emp_record.children:
            assert child.defined_in_record == "WS-EMPLOYEE", (
                f"Child {child.name} from copybook should have "
                f"defined_in_record=WS-EMPLOYEE, got {child.defined_in_record}"
            )

    def test_defined_in_record_in_serialized_output(self):
        """Test that defined_in_record appears in to_dict() serialized output."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        tree = get_data_division_tree(source_path)
        tree_dict = tree.to_dict()

        # Check that defined_in_record is in all_records
        for record_dict in tree_dict["all_records"]:
            assert "defined_in_record" in record_dict, (
                f"Record {record_dict['name']} should have defined_in_record in serialized output"
            )
            # For Level 01, defined_in_record should equal name
            assert record_dict["defined_in_record"] == record_dict["name"]

            # Check children recursively
            def check_children_dict(parent_dict, expected_record):
                for child_dict in parent_dict.get("children", []):
                    assert "defined_in_record" in child_dict, (
                        f"Child {child_dict['name']} should have defined_in_record in serialized output"
                    )
                    assert child_dict["defined_in_record"] == expected_record
                    check_children_dict(child_dict, expected_record)

            check_children_dict(record_dict, record_dict["name"])


class TestTreeOptions:
    """Tests for the TreeOptions dataclass."""

    def test_default_values(self):
        """Test that default values are set correctly."""
        options = TreeOptions()

        assert options.copybook_paths is None
        assert options.resolve_copies is True
        assert options.include_filler is True
        assert options.include_88_levels is True
        assert options.include_source_info is True

    def test_custom_values(self):
        """Test setting custom values."""
        options = TreeOptions(
            copybook_paths=[Path("/copybooks")],
            resolve_copies=False,
            include_filler=False,
            include_88_levels=False,
            include_source_info=False,
        )

        assert options.copybook_paths == [Path("/copybooks")]
        assert options.resolve_copies is False
        assert options.include_filler is False
        assert options.include_88_levels is False
        assert options.include_source_info is False


class TestDataItemNode:
    """Tests for the DataItemNode dataclass."""

    def test_to_dict_minimal(self):
        """Test to_dict with minimal fields."""
        node = DataItemNode(name="TEST-ITEM", level=5)

        d = node.to_dict()

        assert d["name"] == "TEST-ITEM"
        assert d["level"] == 5
        # Optional fields should not be present
        assert "picture" not in d
        assert "usage" not in d
        assert "is_group" not in d
        assert "children" not in d
        assert "defined_in_record" not in d

    def test_to_dict_with_optional_fields(self):
        """Test to_dict with optional fields populated."""
        node = DataItemNode(
            name="TEST-ITEM",
            level=5,
            picture="X(10)",
            is_group=True,
            redefines="OTHER-ITEM",
            position={"start": 1, "end": 10, "size": 10},
            defined_in_record="PARENT-RECORD",
            children=[DataItemNode(name="CHILD", level=10, defined_in_record="PARENT-RECORD")],
        )

        d = node.to_dict()

        assert d["name"] == "TEST-ITEM"
        assert d["level"] == 5
        assert d["picture"] == "X(10)"
        assert d["is_group"] is True
        assert d["redefines"] == "OTHER-ITEM"
        assert d["position"] == {"start": 1, "end": 10, "size": 10}
        assert d["defined_in_record"] == "PARENT-RECORD"
        assert len(d["children"]) == 1
        assert d["children"][0]["name"] == "CHILD"
        assert d["children"][0]["defined_in_record"] == "PARENT-RECORD"

    def test_defined_in_record_included_when_set(self):
        """Test that defined_in_record is included in to_dict when set."""
        node = DataItemNode(
            name="TEST-ITEM",
            level=5,
            defined_in_record="MY-RECORD",
        )

        d = node.to_dict()

        assert "defined_in_record" in d
        assert d["defined_in_record"] == "MY-RECORD"

    def test_defined_in_record_not_included_when_none(self):
        """Test that defined_in_record is not included in to_dict when None."""
        node = DataItemNode(
            name="TEST-ITEM",
            level=5,
            defined_in_record=None,
        )

        d = node.to_dict()

        assert "defined_in_record" not in d


class TestDataDivisionSection:
    """Tests for the DataDivisionSection dataclass."""

    def test_to_dict(self):
        """Test to_dict serialization."""
        section = DataDivisionSection(
            name="WORKING-STORAGE",
            records=[
                DataItemNode(name="RECORD-1", level=1),
                DataItemNode(name="RECORD-2", level=1),
            ]
        )

        d = section.to_dict()

        assert d["name"] == "WORKING-STORAGE"
        assert len(d["records"]) == 2
        assert d["records"][0]["name"] == "RECORD-1"
        assert d["records"][1]["name"] == "RECORD-2"


class TestFillerCopybookProperty:
    """Tests for the copybook property on FILLER items."""

    def test_filler_copybook_property_in_to_dict(self):
        """Test that copybook property appears in to_dict output."""
        node = DataItemNode(
            name="FILLER$1",
            level=2,
            is_filler=True,
            copybook="EMPLOYEE-CPY",
        )

        d = node.to_dict()

        assert d["is_filler"] is True
        assert d["copybook"] == "EMPLOYEE-CPY"

    def test_filler_copybook_property_not_included_when_none(self):
        """Test that copybook property is not included when None."""
        node = DataItemNode(
            name="FILLER$1",
            level=2,
            is_filler=True,
        )

        d = node.to_dict()

        assert d["is_filler"] is True
        assert "copybook" not in d

    def test_filler_with_copybook_content(self):
        """Test FILLER item that contains content from a copybook."""
        source_path = FIXTURES_DIR / "copybook_main.cob"
        copybook_path = FIXTURES_DIR / "copybooks"

        tree = get_data_division_tree(
            source_path,
            TreeOptions(copybook_paths=[copybook_path])
        )

        # Look for FILLER items that have copybook property set
        def find_filler_with_copybook(node):
            if node.is_filler and node.copybook:
                return node
            for child in node.children:
                result = find_filler_with_copybook(child)
                if result:
                    return result
            return None

        # Check if any FILLER has copybook property
        # (depends on the fixture having such a structure)
        filler = None
        for record in tree.all_records:
            filler = find_filler_with_copybook(record)
            if filler:
                break

        # If we found a FILLER with copybook, verify structure
        if filler:
            assert filler.is_filler is True
            assert filler.copybook is not None
            # The FILLER's children should have copybook_source matching
            if filler.children:
                for child in filler.children:
                    if child.level != 88 and child.copybook_source:
                        assert child.copybook_source == filler.copybook


class TestBuildCopybookLineMap:
    """Tests for the _build_copybook_line_map helper function."""

    def test_extracts_copy_statements(self):
        """Test that COPY statements are correctly extracted."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY COPYBOOK-A.
       COPY COPYBOOK-B.
       PROCEDURE DIVISION.
       STOP RUN.
"""
        result = _build_copybook_line_map(source)

        assert "COPYBOOK-A" in result
        assert "COPYBOOK-B" in result
        assert result["COPYBOOK-A"] == 5
        assert result["COPYBOOK-B"] == 6

    def test_handles_multiple_copies_of_same_copybook(self):
        """Test that first occurrence is used for duplicate COPY statements."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY MY-COPY.
       01 WS-VAR PIC X.
       COPY MY-COPY.
       PROCEDURE DIVISION.
       STOP RUN.
"""
        result = _build_copybook_line_map(source)

        # Should use line 5 (first occurrence), not line 7
        assert result["MY-COPY"] == 5

    def test_case_insensitive(self):
        """Test that copybook names are normalized to uppercase."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY my-copybook.
       PROCEDURE DIVISION.
       STOP RUN.
"""
        result = _build_copybook_line_map(source)

        assert "MY-COPYBOOK" in result
        assert result["MY-COPYBOOK"] == 5

    def test_no_copy_statements(self):
        """Test handling of source with no COPY statements."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC X.
       PROCEDURE DIVISION.
       STOP RUN.
"""
        result = _build_copybook_line_map(source)

        assert len(result) == 0


class TestAnalyzeWithTree:
    """Tests for the analyze_with_tree combined API function."""

    def test_returns_combined_result(self):
        """Test that analyze_with_tree returns a CombinedResult."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_with_tree(source_path)

        assert isinstance(result, CombinedResult)
        assert result.program_name == "SIMPLE-PROGRAM"
        assert isinstance(result.data_division_tree, DataDivisionTree)
        assert isinstance(result.analysis_result, AnalysisResult)
        assert result.execution_time_seconds > 0

    def test_data_division_tree_matches_separate_call(self):
        """Test that DataDivisionTree from combined API matches separate API."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        # Get tree from combined API
        combined = analyze_with_tree(source_path)
        combined_tree = combined.data_division_tree

        # Get tree from separate API
        separate_tree = get_data_division_tree(source_path)

        # Compare key attributes (excluding execution time which varies)
        assert combined_tree.program_name == separate_tree.program_name
        assert combined_tree.summary == separate_tree.summary
        assert len(combined_tree.all_records) == len(separate_tree.all_records)
        assert len(combined_tree.sections) == len(separate_tree.sections)

        # Compare record names
        combined_names = {r.name for r in combined_tree.all_records}
        separate_names = {r.name for r in separate_tree.all_records}
        assert combined_names == separate_names

    def test_analysis_result_matches_separate_call(self):
        """Test that AnalysisResult from combined API matches separate API."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        # Get result from combined API
        combined = analyze_with_tree(source_path)
        combined_analysis = combined.analysis_result

        # Get result from separate API
        separate_analysis = analyze_paragraph_variables(source_path)

        # Compare key attributes (excluding execution time and analysis_date)
        assert combined_analysis.program_name == separate_analysis.program_name

        # Compare paragraph_variables structure (excluding volatile fields)
        combined_paras = combined_analysis.paragraph_variables.get("paragraphs", {})
        separate_paras = separate_analysis.paragraph_variables.get("paragraphs", {})
        assert set(combined_paras.keys()) == set(separate_paras.keys())

        # Compare variable_index
        assert combined_analysis.variable_index == separate_analysis.variable_index

    def test_variable_index_is_populated(self):
        """Test that variable_index is properly populated in combined result."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_with_tree(source_path)

        # variable_index should have entries
        assert result.analysis_result.variable_index
        assert isinstance(result.analysis_result.variable_index, dict)

    def test_with_default_options(self):
        """Test combined API with default options."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_with_tree(source_path)

        # Source info should be included by default
        assert result.data_division_tree.source_info is not None
        assert result.analysis_result.source_info is not None

    def test_with_custom_options(self):
        """Test combined API with custom options."""
        source_path = FIXTURES_DIR / "simple_program.cob"
        options = CombinedOptions(
            include_source_info=False,
            include_filler=False,
            include_88_levels=False,
            include_redefines=False,
        )

        result = analyze_with_tree(source_path, options)

        # Source info should be excluded
        assert result.data_division_tree.source_info is None
        assert result.analysis_result.source_info is None

        # Check no FILLER items in tree
        def has_filler(node):
            if node.is_filler:
                return True
            for child in node.children:
                if has_filler(child):
                    return True
            return False

        for record in result.data_division_tree.all_records:
            assert not has_filler(record)

        # Check no 88-level items in tree
        def has_88_level(node):
            if node.level == 88:
                return True
            for child in node.children:
                if has_88_level(child):
                    return True
            return False

        for record in result.data_division_tree.all_records:
            assert not has_88_level(record)

    def test_file_not_found(self):
        """Test that FileNotFoundError is raised for missing files."""
        source_path = FIXTURES_DIR / "nonexistent.cob"

        with pytest.raises(FileNotFoundError) as exc_info:
            analyze_with_tree(source_path)

        assert "nonexistent.cob" in str(exc_info.value)

    def test_directory_instead_of_file(self):
        """Test that FileNotFoundError is raised when path is a directory."""
        with pytest.raises(FileNotFoundError) as exc_info:
            analyze_with_tree(FIXTURES_DIR)

        assert "not a file" in str(exc_info.value)

    def test_with_copybooks(self):
        """Test combined API with copybook resolution."""
        source_path = FIXTURES_DIR / "copybook_main.cob"
        copybook_path = FIXTURES_DIR / "copybooks"
        options = CombinedOptions(copybook_paths=[copybook_path])

        result = analyze_with_tree(source_path, options)

        # Tree should contain items from copybooks
        record_names = {r.name for r in result.data_division_tree.all_records}
        assert "WS-EMPLOYEE" in record_names

        # Analysis should include copybook content
        assert result.analysis_result.program_name is not None

    def test_with_redefines_program(self):
        """Test combined API with REDEFINES program."""
        source_path = FIXTURES_DIR / "redefines_program.cob"

        result = analyze_with_tree(source_path)

        # Find EMPLOYEE-RECORD which redefines INPUT-RECORD
        emp_record = None
        for record in result.data_division_tree.all_records:
            if record.name == "EMPLOYEE-RECORD":
                emp_record = record
                break

        assert emp_record is not None
        assert emp_record.redefines == "INPUT-RECORD"


class TestCombinedOptions:
    """Tests for the CombinedOptions dataclass."""

    def test_default_values(self):
        """Test that default values are set correctly."""
        options = CombinedOptions()

        assert options.copybook_paths is None
        assert options.resolve_copies is True
        assert options.include_redefines is True
        assert options.include_ancestor_mods is True
        assert options.include_source_info is True
        assert options.include_filler is True
        assert options.include_88_levels is True

    def test_custom_values(self):
        """Test setting custom values."""
        options = CombinedOptions(
            copybook_paths=[Path("/copybooks")],
            resolve_copies=False,
            include_redefines=False,
            include_ancestor_mods=False,
            include_source_info=False,
            include_filler=False,
            include_88_levels=False,
        )

        assert options.copybook_paths == [Path("/copybooks")]
        assert options.resolve_copies is False
        assert options.include_redefines is False
        assert options.include_ancestor_mods is False
        assert options.include_source_info is False
        assert options.include_filler is False
        assert options.include_88_levels is False

    def test_to_analysis_options(self):
        """Test conversion to AnalysisOptions."""
        options = CombinedOptions(
            copybook_paths=[Path("/copybooks")],
            resolve_copies=False,
            include_redefines=False,
            include_ancestor_mods=False,
            include_source_info=False,
        )

        analysis_options = options.to_analysis_options()

        assert analysis_options.copybook_paths == [Path("/copybooks")]
        assert analysis_options.resolve_copies is False
        assert analysis_options.include_redefines is False
        assert analysis_options.include_ancestor_mods is False
        assert analysis_options.include_source_info is False

    def test_to_tree_options(self):
        """Test conversion to TreeOptions."""
        options = CombinedOptions(
            copybook_paths=[Path("/copybooks")],
            resolve_copies=False,
            include_filler=False,
            include_88_levels=False,
            include_source_info=False,
        )

        tree_options = options.to_tree_options()

        assert tree_options.copybook_paths == [Path("/copybooks")]
        assert tree_options.resolve_copies is False
        assert tree_options.include_filler is False
        assert tree_options.include_88_levels is False
        assert tree_options.include_source_info is False


class TestCombinedResult:
    """Tests for the CombinedResult dataclass."""

    def test_result_attributes(self):
        """Test that CombinedResult has expected attributes."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_with_tree(source_path)

        assert hasattr(result, "program_name")
        assert hasattr(result, "data_division_tree")
        assert hasattr(result, "analysis_result")
        assert hasattr(result, "execution_time_seconds")

    def test_program_name_consistency(self):
        """Test that program_name is consistent across all components."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_with_tree(source_path)

        assert result.program_name == result.data_division_tree.program_name
        assert result.program_name == result.analysis_result.program_name


class TestVariableAccessTracking:
    """Tests for variable access (read) tracking."""

    def test_access_lines_in_paragraph_variables(self):
        """Test that access_lines are tracked in paragraph_variables output."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)
        paragraphs = result.paragraph_variables.get("paragraphs", {})

        # CLEANUP-PARA has DISPLAY WS-TOTAL-CTR which should be tracked as access
        cleanup_vars = paragraphs.get("CLEANUP-PARA", {})
        assert "WS-TOTAL-CTR" in cleanup_vars
        ws_total = cleanup_vars["WS-TOTAL-CTR"]
        assert "access_lines" in ws_total
        assert len(ws_total["access_lines"]) > 0

    def test_access_lines_from_if_condition(self):
        """Test that variables in IF conditions are tracked as accesses."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)
        paragraphs = result.paragraph_variables.get("paragraphs", {})

        # PROCESS-PARA has IF WS-LOOP-CTR > 100
        process_vars = paragraphs.get("PROCESS-PARA", {})
        assert "WS-LOOP-CTR" in process_vars
        ws_loop = process_vars["WS-LOOP-CTR"]
        # Should have both modification_lines (ADD 1 TO) and access_lines (IF condition)
        assert "modification_lines" in ws_loop
        assert "access_lines" in ws_loop

    def test_access_lines_from_compute_expression(self):
        """Test that source variables in COMPUTE are tracked as accesses."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)
        paragraphs = result.paragraph_variables.get("paragraphs", {})

        # PROCESS-PARA has COMPUTE WS-EMP-SALARY = WS-EMP-SALARY * 1.05
        # WS-EMP-SALARY is both modified (target) and accessed (source)
        process_vars = paragraphs.get("PROCESS-PARA", {})
        assert "WS-EMP-SALARY" in process_vars
        ws_salary = process_vars["WS-EMP-SALARY"]
        assert "modification_lines" in ws_salary
        assert "access_lines" in ws_salary

    def test_variable_index_has_accessing_paragraphs(self):
        """Test that variable_index includes accessing_paragraphs."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # Check structure
        for record_name, positions in result.variable_index.items():
            for pos_key, entry in positions.items():
                assert "modifying_paragraphs" in entry
                assert "accessing_paragraphs" in entry
                assert isinstance(entry["modifying_paragraphs"], list)
                assert isinstance(entry["accessing_paragraphs"], list)

    def test_accessing_paragraphs_contains_reader(self):
        """Test that accessing_paragraphs contains paragraphs that read the variable."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # Find WS-TOTAL-CTR in the index - should have CLEANUP-PARA as an accessor
        for record_name, positions in result.variable_index.items():
            for pos_key, entry in positions.items():
                if entry["variable_name"] == "WS-TOTAL-CTR":
                    # CLEANUP-PARA displays this variable
                    assert "CLEANUP-PARA" in entry["accessing_paragraphs"]
                    return

        pytest.fail("WS-TOTAL-CTR not found in variable_index")

    def test_summary_includes_access_counts(self):
        """Test that summary statistics include access counts."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)
        summary = result.paragraph_variables.get("summary", {})

        # Check new summary fields
        assert "unique_modified_variables" in summary
        assert "unique_accessed_variables" in summary
        assert summary["unique_accessed_variables"] > 0

    def test_analysis_output_includes_accesses(self):
        """Test that analysis output includes sections_and_paragraphs_accesses."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        # Analysis output should have accesses section
        assert "sections_and_paragraphs_accesses" in result.analysis
        accesses = result.analysis["sections_and_paragraphs_accesses"]
        assert isinstance(accesses, dict)

        # Should have at least some access data
        assert len(accesses) > 0

    def test_build_variable_index_with_accesses(self):
        """Test _build_variable_index correctly handles access_lines."""
        paragraph_variables = {
            "paragraphs": {
                "PARA-1": {
                    "VAR-A": {
                        "defined_in_record": "REC-1",
                        "base_record": "REC-1",
                        "position": {"start": 1, "end": 10},
                        "modification_lines": [10],
                        "access_lines": [15, 20],
                        "explanation": "modified and accessed"
                    }
                },
                "PARA-2": {
                    "VAR-A": {
                        "defined_in_record": "REC-1",
                        "base_record": "REC-1",
                        "position": {"start": 1, "end": 10},
                        "access_lines": [25],  # Only accessed, not modified
                        "explanation": "accessed only"
                    }
                }
            }
        }

        index = _build_variable_index(paragraph_variables)

        # Check the entry for VAR-A
        entry = index["REC-1"]["1:10"]
        assert entry["variable_name"] == "VAR-A"

        # PARA-1 modifies and accesses, PARA-2 only accesses
        assert entry["modifying_paragraphs"] == ["PARA-1"]
        assert set(entry["accessing_paragraphs"]) == {"PARA-1", "PARA-2"}
