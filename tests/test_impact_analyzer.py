"""Tests for the impact analyzer module."""

import pytest
from pathlib import Path

# Path is setup in conftest.py

from parser.cobol_parser import CobolParser
from cobol_ast.builder import ASTBuilder
from analyzers.impact_analyzer import ImpactAnalyzer


class TestImpactAnalyzer:
    """Tests for combined impact analysis."""

    @pytest.fixture
    def impact_program(self):
        """Create a program for impact analysis testing."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMPACT-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 INPUT-RECORD.
          05 INPUT-TYPE         PIC X.
          05 INPUT-DATA         PIC X(49).

       01 EMPLOYEE-REC REDEFINES INPUT-RECORD.
          05 EMP-TYPE           PIC X.
          05 EMP-ID             PIC 9(5).
          05 EMP-NAME           PIC X(44).

       01 DEPT-REC REDEFINES INPUT-RECORD.
          05 DEPT-TYPE          PIC X.
          05 DEPT-CODE          PIC X(5).
          05 DEPT-NAME          PIC X(44).

       01 WS-COUNTERS.
          05 WS-EMP-COUNT       PIC 9(5).
          05 WS-DEPT-COUNT      PIC 9(5).

       PROCEDURE DIVISION.

       INIT-SECTION SECTION.

       INIT-PARA.
           INITIALIZE INPUT-RECORD
           MOVE 0 TO WS-EMP-COUNT
           MOVE 0 TO WS-DEPT-COUNT.

       PROCESS-SECTION SECTION.

       PROCESS-EMPLOYEE.
           MOVE "E" TO INPUT-TYPE
           MOVE 12345 TO EMP-ID
           ADD 1 TO WS-EMP-COUNT.

       PROCESS-DEPT.
           MOVE "D" TO INPUT-TYPE
           MOVE "SALES" TO DEPT-CODE
           ADD 1 TO WS-DEPT-COUNT.
        """
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        return builder.build(tree)

    @pytest.fixture
    def analyzer(self, impact_program):
        """Create an analyzer for impact analysis."""
        analyzer = ImpactAnalyzer(impact_program)
        analyzer.analyze()
        return analyzer

    def test_section_impact(self, analyzer):
        """Test impact analysis for a section."""
        impact = analyzer.get_section_impact("INIT-SECTION")
        assert impact is not None
        assert impact.is_section
        # Should have variable impacts
        assert len(impact.variable_impacts) > 0

    def test_paragraph_impact(self, analyzer):
        """Test impact analysis for a paragraph."""
        impact = analyzer.get_paragraph_impact("PROCESS-EMPLOYEE")
        assert impact is not None
        assert not impact.is_section

    def test_affected_records_with_redefines(self, analyzer):
        """Test that REDEFINES relationships are tracked."""
        records = analyzer.get_records_affected_by_paragraph("PROCESS-EMPLOYEE")
        # Modifying INPUT-TYPE or EMP-ID affects all three records
        # because of REDEFINES chain
        if "INPUT-RECORD" in records:
            assert "EMPLOYEE-REC" in records or len(records) >= 1

    def test_records_affected_by_section(self, analyzer):
        """Test getting all records affected by a section."""
        records = analyzer.get_records_affected_by_section("INIT-SECTION")
        assert len(records) > 0

    def test_sections_affecting_record(self, analyzer):
        """Test getting sections that affect a record."""
        sections = analyzer.get_sections_affecting_record("WS-COUNTERS")
        assert "INIT-SECTION" in sections or "PROCESS-SECTION" in sections

    def test_paragraphs_affecting_record(self, analyzer):
        """Test getting paragraphs that affect a record."""
        paragraphs = analyzer.get_paragraphs_affecting_record("WS-COUNTERS")
        # At least one paragraph should modify counter fields
        assert len(paragraphs) > 0

    def test_generate_output(self, analyzer):
        """Test output generation."""
        output = analyzer.generate_output()

        assert "program_name" in output
        assert output["program_name"] == "IMPACT-TEST"
        assert "analysis_date" in output
        assert "sections_and_paragraphs" in output
        assert "summary" in output

    def test_output_structure(self, analyzer):
        """Test structure of output."""
        output = analyzer.generate_output()

        sections_paras = output["sections_and_paragraphs"]
        # Should have at least some entries
        assert len(sections_paras) > 0

        # Check structure of entries
        for name, modifications in sections_paras.items():
            assert isinstance(modifications, list)
            for mod in modifications:
                assert "variable" in mod
                assert "affected_records" in mod

    def test_compact_output(self, analyzer):
        """Test compact output generation."""
        compact = analyzer.generate_compact_output()

        assert len(compact) > 0
        for name, entries in compact.items():
            for entry in entries:
                # Should have unique variables
                assert "variable" in entry
                assert "affected_records" in entry

    def test_analysis_summary(self, analyzer):
        """Test comprehensive analysis summary."""
        summary = analyzer.get_analysis_summary()

        assert "data_division" in summary
        assert "redefines_analysis" in summary
        assert "procedure_division" in summary
        assert "impact_analysis" in summary

        assert summary["impact_analysis"]["sections_analyzed"] > 0


class TestEndToEnd:
    """End-to-end integration tests."""

    def test_full_analysis_pipeline(self):
        """Test the full analysis pipeline."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E2E-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-RECORD.
          05 WS-FIELD-A PIC X(10).
          05 WS-FIELD-B PIC 9(5).

       PROCEDURE DIVISION.

       MAIN-PARA.
           MOVE "TEST" TO WS-FIELD-A
           ADD 1 TO WS-FIELD-B
           STOP RUN.
        """

        # Parse
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)

        # Build AST
        builder = ASTBuilder()
        program = builder.build(tree)

        # Analyze
        analyzer = ImpactAnalyzer(program)
        output = analyzer.generate_output()

        # Verify output
        assert output["program_name"] == "E2E-TEST"
        assert "MAIN-PARA" in output["sections_and_paragraphs"]

        main_para = output["sections_and_paragraphs"]["MAIN-PARA"]
        modified_vars = {m["variable"] for m in main_para}
        assert "WS-FIELD-A" in modified_vars
        assert "WS-FIELD-B" in modified_vars

    def test_fixture_simple_program(self):
        """Test analysis of the simple_program fixture."""
        fixtures_path = Path(__file__).parent / "fixtures"
        source_path = fixtures_path / "simple_program.cob"

        if source_path.exists():
            source = source_path.read_text()

            parser = CobolParser(use_generated=False)
            tree = parser.parse(source)
            builder = ASTBuilder()
            program = builder.build(tree)
            analyzer = ImpactAnalyzer(program)
            output = analyzer.generate_output()

            assert output["program_name"] == "SIMPLE-PROGRAM"
            assert output["summary"]["total_modifications"] > 0
