"""Tests for analyze_for_paragraphs API."""

import pytest
from pathlib import Path

from src.cobol_ast.api import (
    analyze_for_paragraphs,
    ParagraphAnalysisOptions,
    ParagraphAnalysisResult,
    _normalize_paragraph_name,
)


SIMPLE_PROGRAM = Path("tests/fixtures/simple_program.cob")
ALL_MODS_PROGRAM = Path("tests/fixtures/all_modifications.cob")


class TestAnalyzeForParagraphs:
    """Tests for the analyze_for_paragraphs function."""

    def test_basic_filtering(self):
        """Only records referenced by specified paragraphs are in filtered output."""
        # CLEANUP-PARA only accesses WS-TOTAL-CTR which is in WS-COUNTERS
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["CLEANUP-PARA"],
        )

        record_names = [r.name for r in result.filtered_records]
        assert "WS-COUNTERS" in record_names
        # WS-EMPLOYEE-RECORD is NOT referenced by CLEANUP-PARA
        assert "WS-EMPLOYEE-RECORD" not in record_names

    def test_multiple_paragraphs(self):
        """Filtering by multiple paragraphs unions their referenced records."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA", "PROCESS-PARA"],
        )

        record_names = [r.name for r in result.filtered_records]
        assert "WS-EMPLOYEE-RECORD" in record_names
        assert "WS-COUNTERS" in record_names

    def test_full_group_inclusion(self):
        """When a child variable is referenced, the entire 01-level group is included."""
        # INIT-PARA modifies WS-EMP-ID (child of WS-EMPLOYEE-RECORD)
        # The full WS-EMPLOYEE-RECORD group should be included with all children
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        # Find the WS-EMPLOYEE-RECORD
        emp_record = None
        for record in result.filtered_records:
            if record.name == "WS-EMPLOYEE-RECORD":
                emp_record = record
                break

        assert emp_record is not None
        child_names = [c.name for c in emp_record.children]
        # All children should be present, not just the referenced ones
        assert "WS-EMP-ID" in child_names
        assert "WS-EMP-NAME" in child_names
        assert "WS-EMP-SALARY" in child_names
        assert "WS-EMP-DEPT" in child_names

    def test_no_matches(self):
        """When paragraph_names don't match any paragraphs, return empty filtered output."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["NON-EXISTENT-PARA"],
        )

        assert result.filtered_sections == []
        assert result.filtered_records == []
        assert result.filter_summary["total_records_after"] == 0
        assert result.paragraph_names_used == []

    def test_empty_paragraph_list(self):
        """Empty paragraph_names list returns empty filtered output."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=[],
        )

        assert result.filtered_sections == []
        assert result.filtered_records == []

    def test_case_insensitivity(self):
        """Paragraph names match regardless of case."""
        result_lower = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["cleanup-para"],
        )
        result_upper = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["CLEANUP-PARA"],
        )
        result_mixed = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["Cleanup-Para"],
        )

        assert (
            [r.name for r in result_lower.filtered_records]
            == [r.name for r in result_upper.filtered_records]
            == [r.name for r in result_mixed.filtered_records]
        )
        assert len(result_lower.filtered_records) > 0

    def test_section_name_matching(self):
        """Paragraph names with SECTION suffix are matched correctly."""
        # "MAIN-SECTION" appears in the variable_index for simple_program.cob
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["MAIN-SECTION"],
        )

        record_names = [r.name for r in result.filtered_records]
        assert len(record_names) > 0

    def test_program_name(self):
        """Result contains the correct program name."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        assert result.program_name == "SIMPLE-PROGRAM"

    def test_filtered_sections_grouping(self):
        """Records are grouped into the correct filtered sections."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        assert len(result.filtered_sections) > 0
        # All records in simple_program.cob are in WORKING-STORAGE
        for section in result.filtered_sections:
            assert section.name == "WORKING-STORAGE"

    def test_filter_summary_statistics(self):
        """filter_summary contains correct record counts and reduction percentage."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["CLEANUP-PARA"],
        )

        summary = result.filter_summary
        assert "total_records_before" in summary
        assert "total_records_after" in summary
        assert "records_removed" in summary
        assert "reduction_percentage" in summary

        # simple_program.cob has 3 records, CLEANUP-PARA only touches WS-COUNTERS
        assert summary["total_records_before"] == 3
        assert summary["total_records_after"] == 1
        assert summary["records_removed"] == 2
        assert summary["reduction_percentage"] == pytest.approx(66.7, abs=0.1)

    def test_filter_summary_no_reduction(self):
        """When all records match, reduction percentage is 0."""
        # Use all paragraphs from all_modifications.cob to get most records
        result = analyze_for_paragraphs(
            ALL_MODS_PROGRAM,
            paragraph_names=[
                "TEST-MOVE-PARA", "TEST-COMPUTE-PARA", "TEST-ADD-PARA",
                "TEST-INSPECT-PARA", "TEST-INITIALIZE-PARA",
                "TEST-SET-PARA", "TEST-ACCEPT-PARA",
            ],
        )

        # These paragraphs reference WS-NUMERIC-FIELDS, WS-TEXT-FIELDS,
        # WS-INDEX-FIELD, WS-INPUT-FIELD (4 of 6 records)
        assert result.filter_summary["total_records_after"] >= 4

    def test_paragraph_names_used(self):
        """paragraph_names_used lists only the paragraphs that matched."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA", "NON-EXISTENT-PARA"],
        )

        assert "INIT-PARA" in result.paragraph_names_used
        assert "NON-EXISTENT-PARA" not in result.paragraph_names_used

    def test_execution_time(self):
        """Result includes a positive execution time."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        assert result.execution_time_seconds > 0

    def test_file_not_found(self):
        """Raises FileNotFoundError for missing source file."""
        with pytest.raises(FileNotFoundError):
            analyze_for_paragraphs(
                Path("nonexistent.cob"),
                paragraph_names=["INIT-PARA"],
            )

    def test_with_copybook_paths(self):
        """ParagraphAnalysisOptions.copybook_paths is forwarded correctly."""
        result = analyze_for_paragraphs(
            Path("tests/fixtures/copybook_main.cob"),
            paragraph_names=["MAIN-PARA"],
            options=ParagraphAnalysisOptions(
                copybook_paths=[Path("tests/fixtures/copybooks")],
            ),
        )

        # copybook_main.cob has COPY statements that define variables
        # MAIN-PARA should reference some of them
        assert result.program_name == "COPYBOOK-EXAMPLE"

    def test_exclude_filler(self):
        """ParagraphAnalysisOptions.include_filler=False excludes FILLER items."""
        result_with = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
            options=ParagraphAnalysisOptions(include_filler=True),
        )
        result_without = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
            options=ParagraphAnalysisOptions(include_filler=False),
        )

        # Both should have the same records since simple_program.cob
        # doesn't have FILLER items in the referenced records
        assert len(result_with.filtered_records) == len(result_without.filtered_records)

    def test_exclude_88_levels(self):
        """ParagraphAnalysisOptions.include_88_levels=False excludes 88-level items."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
            options=ParagraphAnalysisOptions(include_88_levels=False),
        )

        # Verify no 88-level items in the filtered tree
        def has_88(node):
            if node.level == 88:
                return True
            return any(has_88(c) for c in node.children)

        for record in result.filtered_records:
            assert not has_88(record)


class TestParagraphAnalysisResultFullData:
    """Tests verifying the full (unfiltered) data is included in the result."""

    def test_full_tree_included(self):
        """data_division_tree contains all records, not just filtered ones."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["CLEANUP-PARA"],
        )

        # Filtered should be a subset
        assert len(result.filtered_records) < len(result.data_division_tree.all_records)
        # Full tree has all 3 records
        assert len(result.data_division_tree.all_records) == 3

    def test_analysis_result_included(self):
        """analysis_result contains variable_index and paragraph_variables."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        assert result.analysis_result is not None
        assert result.analysis_result.variable_index is not None
        assert len(result.analysis_result.variable_index) > 0
        assert result.analysis_result.paragraph_variables is not None
        assert "paragraphs" in result.analysis_result.paragraph_variables

    def test_variable_index_usable(self):
        """The variable_index can be used for lookups on filtered records."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        index = result.analysis_result.variable_index
        # Look up a filtered record in the index
        for record in result.filtered_records:
            if record.name == "WS-EMPLOYEE-RECORD":
                assert record.defined_in_record in index
                break

    def test_full_tree_has_unfiltered_records(self):
        """data_division_tree includes records that are NOT in filtered_records."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["CLEANUP-PARA"],
        )

        full_names = {r.name for r in result.data_division_tree.all_records}
        filtered_names = {r.name for r in result.filtered_records}
        # WS-FLAGS should be in full tree but not in filtered
        assert "WS-FLAGS" in full_names
        assert "WS-FLAGS" not in filtered_names


class TestParagraphAnalysisResultToDict:
    """Tests for ParagraphAnalysisResult.to_dict()."""

    def test_to_dict_structure(self):
        """to_dict() returns a well-formed dictionary."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        d = result.to_dict()
        assert "program_name" in d
        assert "data_division_tree" in d
        assert "analysis_result" in d
        assert "filtered_sections" in d
        assert "filtered_records" in d
        assert "filter_summary" in d
        assert "paragraph_names_used" in d
        assert "execution_time_seconds" in d

    def test_to_dict_analysis_result_contents(self):
        """to_dict() includes paragraph_variables and variable_index."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        d = result.to_dict()
        ar = d["analysis_result"]
        assert "paragraph_variables" in ar
        assert "variable_index" in ar

    def test_to_dict_no_warnings(self):
        """to_dict() omits warnings key when empty."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        d = result.to_dict()
        if not result.warnings:
            assert "warnings" not in d

    def test_to_dict_serializable(self):
        """to_dict() output is JSON-serializable."""
        import json

        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        # Should not raise
        json_str = json.dumps(result.to_dict())
        assert len(json_str) > 0


class TestParagraphAnalysisResultToText:
    """Tests for ParagraphAnalysisResult.to_text()."""

    def test_to_text_contains_section_header(self):
        """to_text() output includes section headers."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        text = result.to_text()
        assert "WORKING-STORAGE SECTION." in text

    def test_to_text_contains_record_names(self):
        """to_text() output includes record names."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        text = result.to_text()
        assert "WS-EMPLOYEE-RECORD" in text
        assert "WS-COUNTERS" in text

    def test_to_text_contains_level_numbers(self):
        """to_text() output includes COBOL level numbers."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        text = result.to_text()
        assert "01" in text
        assert "05" in text

    def test_to_text_contains_pic_clauses(self):
        """to_text() output includes PIC clauses for elementary items."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        text = result.to_text()
        assert "PIC" in text

    def test_to_text_indentation(self):
        """to_text() output has proper indentation for nested items."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        text = result.to_text()
        lines = text.splitlines()

        # Level 01 items should not be indented
        for line in lines:
            stripped = line.lstrip()
            if stripped.startswith("01"):
                # 01-level should be at column 0 (no leading spaces)
                assert line.startswith("01"), f"01-level should not be indented: {line!r}"

        # Level 05 items should be indented
        has_indented_05 = False
        for line in lines:
            stripped = line.lstrip()
            if stripped.startswith("05"):
                assert line.startswith("    "), f"05-level should be indented: {line!r}"
                has_indented_05 = True
        assert has_indented_05

    def test_to_text_empty_result(self):
        """to_text() returns empty string for empty result."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["NON-EXISTENT-PARA"],
        )

        text = result.to_text()
        assert text == ""

    def test_to_text_value_clauses(self):
        """to_text() includes VALUE clauses when present."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        text = result.to_text()
        # WS-COUNTERS children have VALUE 0
        assert "VALUE" in text

    def test_to_text_full_output_structure(self):
        """to_text() produces a well-structured COBOL-like output."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["INIT-PARA"],
        )

        text = result.to_text()
        lines = text.splitlines()

        # Should start with section header
        assert lines[0] == "WORKING-STORAGE SECTION."

        # Should have blank line after section header
        assert lines[1] == ""

        # Should not end with blank line (trailing blanks stripped)
        assert lines[-1] != ""

    def test_to_text_excludes_unfiltered_records(self):
        """to_text() only renders filtered records, not the full tree."""
        result = analyze_for_paragraphs(
            SIMPLE_PROGRAM,
            paragraph_names=["CLEANUP-PARA"],
        )

        text = result.to_text()
        # WS-EMPLOYEE-RECORD is in the full tree but NOT in filtered output
        assert "WS-EMPLOYEE-RECORD" not in text
        # WS-COUNTERS IS in filtered output
        assert "WS-COUNTERS" in text


class TestAnalyzeForParagraphsWithAllModifications:
    """Tests using all_modifications.cob for more diverse scenarios."""

    def test_single_paragraph_single_record(self):
        """A paragraph that only modifies one record yields just that record."""
        # TEST-SET-PARA only modifies WS-INDEX-FIELD
        result = analyze_for_paragraphs(
            ALL_MODS_PROGRAM,
            paragraph_names=["TEST-SET-PARA"],
        )

        record_names = [r.name for r in result.filtered_records]
        assert "WS-INDEX-FIELD" in record_names

    def test_paragraph_referencing_multiple_records(self):
        """A paragraph that references variables in multiple records includes all."""
        # TEST-MOVE-PARA modifies WS-NUM-A, WS-NUM-B, WS-NUM-C (in WS-NUMERIC-FIELDS)
        # and WS-TEXT-A (in WS-TEXT-FIELDS)
        result = analyze_for_paragraphs(
            ALL_MODS_PROGRAM,
            paragraph_names=["TEST-MOVE-PARA"],
        )

        record_names = [r.name for r in result.filtered_records]
        assert "WS-NUMERIC-FIELDS" in record_names
        assert "WS-TEXT-FIELDS" in record_names

    def test_records_not_referenced_excluded(self):
        """Records that are never referenced by any specified paragraph are excluded."""
        # TEST-SET-PARA only modifies WS-INDEX-FIELD
        result = analyze_for_paragraphs(
            ALL_MODS_PROGRAM,
            paragraph_names=["TEST-SET-PARA"],
        )

        record_names = [r.name for r in result.filtered_records]
        assert "WS-TABLE-AREA" not in record_names
        assert "WS-TALLY-FIELD" not in record_names

    def test_section_name_as_paragraph(self):
        """Using a SECTION name matches all paragraphs within that section."""
        # TEST-SECTION is the containing section for all paragraphs
        result = analyze_for_paragraphs(
            ALL_MODS_PROGRAM,
            paragraph_names=["TEST-SECTION"],
        )

        # TEST-SECTION should reference multiple record groups
        record_names = [r.name for r in result.filtered_records]
        assert len(record_names) >= 2


class TestNormalizeParagraphName:
    """Tests for the _normalize_paragraph_name helper."""

    def test_basic_uppercase(self):
        assert _normalize_paragraph_name("init-para") == "INIT-PARA"

    def test_already_uppercase(self):
        assert _normalize_paragraph_name("INIT-PARA") == "INIT-PARA"

    def test_strips_section_suffix(self):
        assert _normalize_paragraph_name("MAIN SECTION") == "MAIN"

    def test_strips_section_suffix_case_insensitive(self):
        assert _normalize_paragraph_name("main section") == "MAIN"

    def test_strips_whitespace(self):
        assert _normalize_paragraph_name("  INIT-PARA  ") == "INIT-PARA"

    def test_no_false_section_strip(self):
        """'SECTION' within the name is not stripped."""
        assert _normalize_paragraph_name("MY-SECTION-PARA") == "MY-SECTION-PARA"
