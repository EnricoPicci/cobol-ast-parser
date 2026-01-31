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
        assert hasattr(result, "execution_time_seconds")
        assert hasattr(result, "source_info")

    def test_program_name_matches(self):
        """Test that program_name matches in result and outputs."""
        source_path = FIXTURES_DIR / "simple_program.cob"

        result = analyze_paragraph_variables(source_path)

        assert result.program_name == result.analysis.get("program_name")
        assert result.program_name == result.paragraph_variables.get("program_name")


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

    def test_to_dict_with_optional_fields(self):
        """Test to_dict with optional fields populated."""
        node = DataItemNode(
            name="TEST-ITEM",
            level=5,
            picture="X(10)",
            is_group=True,
            redefines="OTHER-ITEM",
            position={"start": 1, "end": 10, "size": 10},
            children=[DataItemNode(name="CHILD", level=10)],
        )

        d = node.to_dict()

        assert d["name"] == "TEST-ITEM"
        assert d["level"] == 5
        assert d["picture"] == "X(10)"
        assert d["is_group"] is True
        assert d["redefines"] == "OTHER-ITEM"
        assert d["position"] == {"start": 1, "end": 10, "size": 10}
        assert len(d["children"]) == 1
        assert d["children"][0]["name"] == "CHILD"


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
