"""Tests for the analyze_procedure_division() API."""

import json
from pathlib import Path

import pytest

from src.cobol_ast import (
    analyze_procedure_division,
    ProcedureDivisionOptions,
    ProcedureDivisionResult,
    AnalysisError,
)

FIXTURES_DIR = Path(__file__).parent / "fixtures"
PROC_DIV_FIXTURE = FIXTURES_DIR / "procedure_division_program.cob"
SIMPLE_FIXTURE = FIXTURES_DIR / "simple_program.cob"
ALL_MODS_FIXTURE = FIXTURES_DIR / "all_modifications.cob"


# ---------------------------------------------------------------------------
# Options tests
# ---------------------------------------------------------------------------


class TestProcedureDivisionOptions:
    """Tests for ProcedureDivisionOptions defaults and custom values."""

    def test_defaults(self):
        opts = ProcedureDivisionOptions()
        assert opts.copybook_paths is None
        assert opts.resolve_copies is True
        assert opts.include_source_info is True

    def test_custom_values(self):
        opts = ProcedureDivisionOptions(
            copybook_paths=[Path("/tmp/copy")],
            resolve_copies=False,
            include_source_info=False,
        )
        assert opts.copybook_paths == [Path("/tmp/copy")]
        assert opts.resolve_copies is False
        assert opts.include_source_info is False


# ---------------------------------------------------------------------------
# End-to-end tests against procedure_division_program.cob
# ---------------------------------------------------------------------------


class TestAnalyzeProcedureDivision:
    """End-to-end tests using the procedure_division_program.cob fixture."""

    @pytest.fixture(autouse=True)
    def setup(self):
        self.result = analyze_procedure_division(PROC_DIV_FIXTURE)

    # -- Inventory (sections + paragraphs) ------------------------------------

    def test_inventory_count(self):
        """6 sections + 12 paragraphs = 18 entries."""
        assert len(self.result.inventory) == 18

    def test_paragraph_names(self):
        names = [p["name"] for p in self.result.inventory]
        assert "MAIN-PARA" in names
        assert "INIT-PARA" in names
        assert "VALIDATE-PARA" in names
        assert "PROCESS-PARA" in names
        assert "PROCESS-ACTIVE" in names
        assert "PROCESS-EXIT" in names
        assert "REPORT-PARA" in names
        assert "SUMMARY-PARA" in names
        assert "ERROR-REPORT-PARA" in names
        assert "CALL-STATIC-PARA" in names
        assert "CALL-DYNAMIC-PARA" in names
        assert "CLEANUP-PARA" in names

    def test_section_names(self):
        section_names = [
            p["name"] for p in self.result.inventory if p["type"] == "section"
        ]
        assert "MAIN-SECTION" in section_names
        assert "VALIDATION-SECTION" in section_names
        assert "PROCESS-SECTION" in section_names
        assert "REPORT-SECTION" in section_names
        assert "EXTERNAL-SECTION" in section_names
        assert "CLEANUP-SECTION" in section_names

    def test_exit_statement_not_in_inventory(self):
        """EXIT. standalone statement should not appear as a paragraph."""
        names = [p["name"] for p in self.result.inventory]
        assert "EXIT" not in names

    def test_inventory_ordering(self):
        """Entries should be ordered by line_start."""
        line_starts = [p["line_start"] for p in self.result.inventory]
        assert line_starts == sorted(line_starts)

    def test_inventory_line_numbers(self):
        """Each entry should have positive line numbers."""
        for entry in self.result.inventory:
            assert entry["line_start"] > 0
            assert entry["line_end"] >= entry["line_start"]
            assert entry["line_count"] > 0

    def test_paragraph_parent_sections(self):
        """Paragraphs should have correct parent_section values."""
        section_map = {
            p["name"]: p["parent_section"]
            for p in self.result.inventory
            if p["type"] == "paragraph"
        }
        assert section_map["MAIN-PARA"] == "MAIN-SECTION"
        assert section_map["INIT-PARA"] == "MAIN-SECTION"
        assert section_map["VALIDATE-PARA"] == "VALIDATION-SECTION"
        assert section_map["PROCESS-PARA"] == "PROCESS-SECTION"
        assert section_map["CALL-STATIC-PARA"] == "EXTERNAL-SECTION"
        assert section_map["CLEANUP-PARA"] == "CLEANUP-SECTION"

    def test_entry_types(self):
        """Entries should have type 'paragraph' or 'section'."""
        for entry in self.result.inventory:
            assert entry["type"] in ("paragraph", "section")

    def test_section_entry_fields(self):
        """Section entries should have parent_section=None and a paragraphs list."""
        for entry in self.result.inventory:
            if entry["type"] == "section":
                assert entry["parent_section"] is None
                assert isinstance(entry["paragraphs"], list)

    def test_section_child_paragraphs(self):
        """Each section should list its child paragraphs."""
        sections = {
            e["name"]: e["paragraphs"]
            for e in self.result.inventory
            if e["type"] == "section"
        }
        assert sections["MAIN-SECTION"] == ["MAIN-PARA", "INIT-PARA"]
        assert sections["VALIDATION-SECTION"] == ["VALIDATE-PARA"]
        assert sections["PROCESS-SECTION"] == [
            "PROCESS-PARA", "PROCESS-ACTIVE", "PROCESS-EXIT",
        ]
        assert sections["REPORT-SECTION"] == [
            "REPORT-PARA", "SUMMARY-PARA", "ERROR-REPORT-PARA",
        ]
        assert sections["EXTERNAL-SECTION"] == [
            "CALL-STATIC-PARA", "CALL-DYNAMIC-PARA",
        ]
        assert sections["CLEANUP-SECTION"] == ["CLEANUP-PARA"]

    def test_section_before_its_paragraphs(self):
        """Each section entry should appear before its child paragraphs."""
        names = [p["name"] for p in self.result.inventory]
        for entry in self.result.inventory:
            if entry["type"] == "section":
                section_idx = names.index(entry["name"])
                for child in entry["paragraphs"]:
                    child_idx = names.index(child)
                    assert section_idx < child_idx

    def test_paragraph_entries_have_no_paragraphs_key(self):
        """Paragraph entries should not have a 'paragraphs' key."""
        for entry in self.result.inventory:
            if entry["type"] == "paragraph":
                assert "paragraphs" not in entry

    # -- PERFORM graph -------------------------------------------------------

    def test_perform_graph_main_para(self):
        """MAIN-PARA should PERFORM multiple targets."""
        performs = self.result.perform_graph.get("MAIN-PARA", [])
        targets = [p["target"] for p in performs]
        assert "INIT-PARA" in targets
        assert "CLEANUP-PARA" in targets

    def test_perform_thru(self):
        """PERFORM THRU should resolve included entries."""
        performs = self.result.perform_graph.get("MAIN-PARA", [])
        thru_performs = [p for p in performs if p.get("thru_target")]
        assert len(thru_performs) >= 1

        thru_perform = thru_performs[0]
        assert thru_perform["target"] == "PROCESS-PARA"
        assert thru_perform["thru_target"] == "PROCESS-EXIT"
        assert "PROCESS-PARA" in thru_perform["thru_includes"]
        assert "PROCESS-ACTIVE" in thru_perform["thru_includes"]
        assert "PROCESS-EXIT" in thru_perform["thru_includes"]

    def test_perform_until(self):
        """PERFORM UNTIL should capture the condition."""
        performs = self.result.perform_graph.get("MAIN-PARA", [])
        until_performs = [p for p in performs if p["type"] == "until"]
        assert len(until_performs) >= 1
        assert until_performs[0]["condition"] is not None

    def test_perform_simple(self):
        """Simple PERFORM should have type 'simple'."""
        performs = self.result.perform_graph.get("MAIN-PARA", [])
        simple = [p for p in performs if p["type"] == "simple"]
        assert len(simple) >= 1

    def test_perform_from_process_para(self):
        """PROCESS-PARA should PERFORM VALIDATE-PARA and PROCESS-ACTIVE."""
        performs = self.result.perform_graph.get("PROCESS-PARA", [])
        targets = [p["target"] for p in performs]
        assert "VALIDATE-PARA" in targets
        assert "PROCESS-ACTIVE" in targets

    # -- GO TO graph ---------------------------------------------------------

    def test_goto_graph(self):
        """REPORT-PARA should have a GO TO to ERROR-REPORT-PARA."""
        gotos = self.result.goto_graph.get("REPORT-PARA", [])
        assert len(gotos) >= 1
        assert gotos[0]["target"] == "ERROR-REPORT-PARA"

    def test_goto_conditional(self):
        """GO TO in REPORT-PARA should be detected as conditional."""
        gotos = self.result.goto_graph.get("REPORT-PARA", [])
        assert len(gotos) >= 1
        assert gotos[0]["conditional"] is True

    # -- CALL graph ----------------------------------------------------------

    def test_call_static(self):
        """CALL-STATIC-PARA should have a static CALL to DATEVAL."""
        calls = self.result.call_graph.get("CALL-STATIC-PARA", [])
        assert len(calls) == 1
        call = calls[0]
        assert call["target"] == "DATEVAL"
        assert call["is_dynamic"] is False
        assert "WS-EMP-ID" in call["using_fields"]
        assert "WS-RETURN-CODE" in call["using_fields"]

    def test_call_dynamic(self):
        """CALL-DYNAMIC-PARA should have a dynamic CALL."""
        calls = self.result.call_graph.get("CALL-DYNAMIC-PARA", [])
        assert len(calls) == 1
        call = calls[0]
        assert call["target"] == "WS-PROGRAM-NAME"
        assert call["is_dynamic"] is True
        assert "WS-EMPLOYEE-RECORD" in call["using_fields"]
        assert "WS-REPORT-LINE" in call["using_fields"]

    # -- Conditional branches ------------------------------------------------

    def test_conditions_evaluate(self):
        """PROCESS-PARA should have an EVALUATE nested inside IF."""
        conds = self.result.conditions.get("PROCESS-PARA", [])
        if_conds = [c for c in conds if c["type"] == "IF"]
        assert len(if_conds) >= 1
        # EVALUATE is nested inside the IF DATA-VALID block
        nested = if_conds[0].get("nested_conditions", [])
        evaluate_conds = [n for n in nested if n["type"] == "EVALUATE"]
        assert len(evaluate_conds) >= 1
        ev = evaluate_conds[0]
        assert ev["subject"] == "WS-EMP-STATUS"
        assert len(ev.get("branches", [])) >= 2

    def test_conditions_evaluate_when_other(self):
        """EVALUATE should have WHEN OTHER branch."""
        conds = self.result.conditions.get("PROCESS-PARA", [])
        if_conds = [c for c in conds if c["type"] == "IF"]
        nested = if_conds[0].get("nested_conditions", [])
        evaluate_conds = [n for n in nested if n["type"] == "EVALUATE"]
        assert len(evaluate_conds) >= 1
        branches = evaluate_conds[0].get("branches", [])
        other_branches = [b for b in branches if b["condition"] is None]
        assert len(other_branches) >= 1

    def test_conditions_if_with_else(self):
        """VALIDATE-PARA should have an IF with ELSE."""
        conds = self.result.conditions.get("VALIDATE-PARA", [])
        if_conds = [c for c in conds if c["type"] == "IF"]
        assert len(if_conds) >= 1
        assert if_conds[0]["has_else"] is True

    def test_conditions_nested_if(self):
        """VALIDATE-PARA IF should have a nested IF."""
        conds = self.result.conditions.get("VALIDATE-PARA", [])
        if_conds = [c for c in conds if c["type"] == "IF"]
        assert len(if_conds) >= 1
        nested = if_conds[0].get("nested_conditions", [])
        assert len(nested) >= 1
        assert nested[0]["type"] == "IF"

    def test_conditions_if_report_para(self):
        """REPORT-PARA should have an IF condition."""
        conds = self.result.conditions.get("REPORT-PARA", [])
        if_conds = [c for c in conds if c["type"] == "IF"]
        assert len(if_conds) >= 1

    # -- Field references ----------------------------------------------------

    def test_field_references_writes(self):
        """INIT-PARA should have writes for initialized variables."""
        refs = self.result.field_references.get("INIT-PARA")
        assert refs is not None
        assert "WS-LOOP-CTR" in refs["writes"]
        assert "WS-TOTAL-CTR" in refs["writes"]

    def test_field_references_reads(self):
        """PROCESS-PARA should read DATA-VALID and WS-EMP-STATUS."""
        refs = self.result.field_references.get("PROCESS-PARA")
        assert refs is not None
        assert "DATA-VALID" in refs["reads"]
        assert "WS-EMP-STATUS" in refs["reads"]

    def test_field_references_conditions_tested(self):
        """INIT-PARA should test 88-level conditions NOT-EOF and DATA-VALID."""
        refs = self.result.field_references.get("INIT-PARA")
        assert refs is not None
        assert "NOT-EOF" in refs["conditions_tested"]
        assert "DATA-VALID" in refs["conditions_tested"]

    def test_field_references_validate_para(self):
        """VALIDATE-PARA should write WS-ERROR-CTR and read WS-EMP-ID."""
        refs = self.result.field_references.get("VALIDATE-PARA")
        assert refs is not None
        assert "WS-ERROR-CTR" in refs["writes"]
        assert "WS-EMP-ID" in refs["reads"]

    # -- Convenience methods -------------------------------------------------

    def test_to_dict_serializable(self):
        """to_dict() should return a JSON-serializable dict."""
        d = self.result.to_dict()
        json_str = json.dumps(d)
        assert len(json_str) > 0
        assert d["program_name"] == "PROC-DIV-EXAMPLE"

    def test_to_dict_has_inventory(self):
        """to_dict() should contain 'inventory' key, not 'paragraphs'."""
        d = self.result.to_dict()
        assert "inventory" in d
        assert "paragraphs" not in d

    def test_to_text_format(self):
        """to_text() should return a non-empty string with expected sections."""
        text = self.result.to_text()
        assert isinstance(text, str)
        assert len(text) > 0
        assert "PROGRAM: PROC-DIV-EXAMPLE" in text
        assert "INVENTORY:" in text
        assert "PERFORM GRAPH:" in text
        assert "GO TO GRAPH:" in text
        assert "CALL GRAPH:" in text

    def test_to_text_shows_sections(self):
        """to_text() should show section entries with SECTION prefix."""
        text = self.result.to_text()
        assert "SECTION MAIN-SECTION" in text
        assert "SECTION REPORT-SECTION" in text

    def test_for_entries_filter(self):
        """for_entries() should filter to only specified entries."""
        filtered = self.result.for_entries(["MAIN-PARA", "INIT-PARA"])
        names = [p["name"] for p in filtered.inventory]
        assert "MAIN-PARA" in names
        assert "INIT-PARA" in names
        assert "PROCESS-PARA" not in names

    def test_for_entries_preserves_perform_targets(self):
        """for_entries() should keep PERFORM graph entries for filtered entries."""
        filtered = self.result.for_entries(["MAIN-PARA"])
        performs = filtered.perform_graph.get("MAIN-PARA", [])
        assert len(performs) >= 1
        targets = [p["target"] for p in performs]
        assert "INIT-PARA" in targets

    def test_for_entries_removes_unrelated(self):
        """for_entries() should remove graphs for non-specified entries."""
        filtered = self.result.for_entries(["MAIN-PARA"])
        assert "PROCESS-PARA" not in filtered.perform_graph
        assert "REPORT-PARA" not in filtered.goto_graph

    def test_for_entries_with_section(self):
        """for_entries() should work with section names."""
        filtered = self.result.for_entries(["MAIN-SECTION"])
        names = [p["name"] for p in filtered.inventory]
        assert "MAIN-SECTION" in names

    # -- Result metadata -----------------------------------------------------

    def test_program_name(self):
        assert self.result.program_name == "PROC-DIV-EXAMPLE"

    def test_execution_time(self):
        assert self.result.execution_time_seconds > 0

    def test_source_info_included(self):
        assert self.result.source_info is not None
        assert self.result.source_info["file_name"] == "procedure_division_program.cob"

    def test_source_info_excluded(self):
        opts = ProcedureDivisionOptions(include_source_info=False)
        result = analyze_procedure_division(PROC_DIV_FIXTURE, opts)
        assert result.source_info is None

    def test_warnings_empty(self):
        """No warnings expected for a self-contained fixture."""
        assert self.result.warnings == []


# ---------------------------------------------------------------------------
# Error handling tests
# ---------------------------------------------------------------------------


class TestErrorHandling:
    """Tests for error handling in analyze_procedure_division()."""

    def test_file_not_found(self):
        with pytest.raises(FileNotFoundError):
            analyze_procedure_division(Path("/nonexistent/file.cob"))

    def test_directory_path(self):
        with pytest.raises(FileNotFoundError):
            analyze_procedure_division(FIXTURES_DIR)


# ---------------------------------------------------------------------------
# Tests against existing fixtures
# ---------------------------------------------------------------------------


class TestExistingFixtures:
    """Verify the API works on existing test fixtures."""

    def test_simple_program(self):
        result = analyze_procedure_division(SIMPLE_FIXTURE)
        assert result.program_name == "SIMPLE-PROGRAM"
        assert len(result.inventory) > 0

        names = [p["name"] for p in result.inventory]
        assert "MAIN-PARA" in names
        assert "INIT-PARA" in names
        assert "PROCESS-PARA" in names
        assert "CLEANUP-PARA" in names
        # Section should also be present
        assert "MAIN-SECTION" in names

    def test_simple_program_performs(self):
        result = analyze_procedure_division(SIMPLE_FIXTURE)
        # MAIN-PARA performs INIT-PARA, PROCESS-PARA, CLEANUP-PARA
        performs = result.perform_graph.get("MAIN-PARA", [])
        targets = [p["target"] for p in performs]
        assert "INIT-PARA" in targets
        assert "CLEANUP-PARA" in targets

    def test_simple_program_field_refs(self):
        result = analyze_procedure_division(SIMPLE_FIXTURE)
        # INIT-PARA writes variables
        refs = result.field_references.get("INIT-PARA")
        assert refs is not None
        assert len(refs["writes"]) > 0

    def test_all_modifications(self):
        result = analyze_procedure_division(ALL_MODS_FIXTURE)
        assert result.program_name == "ALL-MODS-EXAMPLE"
        assert len(result.inventory) > 0

        names = [p["name"] for p in result.inventory]
        assert "TEST-MOVE-PARA" in names
        assert "TEST-COMPUTE-PARA" in names
        # Section should be present
        assert "TEST-SECTION" in names

    def test_all_modifications_field_refs(self):
        result = analyze_procedure_division(ALL_MODS_FIXTURE)
        # TEST-MOVE-PARA should write WS-NUM-A and others
        refs = result.field_references.get("TEST-MOVE-PARA")
        assert refs is not None
        assert "WS-NUM-A" in refs["writes"]

    def test_all_modifications_to_dict(self):
        result = analyze_procedure_division(ALL_MODS_FIXTURE)
        d = result.to_dict()
        assert d["program_name"] == "ALL-MODS-EXAMPLE"
        assert isinstance(d["inventory"], list)

    def test_all_modifications_to_text(self):
        result = analyze_procedure_division(ALL_MODS_FIXTURE)
        text = result.to_text()
        assert "ALL-MODS-EXAMPLE" in text
        assert "INVENTORY:" in text
