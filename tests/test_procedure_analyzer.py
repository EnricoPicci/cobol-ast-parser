"""Tests for the procedure analyzer module."""

import pytest
from pathlib import Path

# Path is setup in conftest.py

from parser.cobol_parser import CobolParser
from cobol_ast.builder import ASTBuilder
from cobol_ast.nodes import ModificationType
from analyzers.procedure_analyzer import ProcedureAnalyzer


class TestProcedureAnalyzer:
    """Tests for procedure division analysis."""

    @pytest.fixture
    def procedure_program(self):
        """Create a program with various modification statements."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROC-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-FIELDS.
          05 WS-NUM-A           PIC 9(5).
          05 WS-NUM-B           PIC 9(5).
          05 WS-TEXT-A          PIC X(20).

       PROCEDURE DIVISION.

       INIT-SECTION SECTION.

       INIT-PARA.
           INITIALIZE WS-FIELDS
           MOVE 0 TO WS-NUM-A
           MOVE 0 TO WS-NUM-B.

       PROCESS-SECTION SECTION.

       CALC-PARA.
           ADD 10 TO WS-NUM-A
           COMPUTE WS-NUM-B = WS-NUM-A * 2.

       UPDATE-PARA.
           MOVE "UPDATED" TO WS-TEXT-A
           MULTIPLY 2 BY WS-NUM-A.

       END-SECTION SECTION.

       FINAL-PARA.
           SUBTRACT 1 FROM WS-NUM-B.
        """
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        return builder.build(tree)

    @pytest.fixture
    def analyzer(self, procedure_program):
        """Create an analyzer for the procedure program."""
        analyzer = ProcedureAnalyzer(procedure_program)
        analyzer.analyze()
        return analyzer

    def test_section_summary(self, analyzer):
        """Test getting section modification summary."""
        summary = analyzer.get_section_summary("INIT-SECTION")
        assert summary is not None
        assert summary.is_section
        assert "WS-FIELDS" in summary.modified_variables or \
               "WS-NUM-A" in summary.modified_variables

    def test_paragraph_summary(self, analyzer):
        """Test getting paragraph modification summary."""
        summary = analyzer.get_paragraph_summary("CALC-PARA")
        assert summary is not None
        assert not summary.is_section
        assert "WS-NUM-A" in summary.modified_variables
        assert "WS-NUM-B" in summary.modified_variables

    def test_modifications_for_variable(self, analyzer):
        """Test getting all modifications for a variable."""
        mods = analyzer.get_modifications_for_variable("WS-NUM-A")
        assert len(mods) >= 2  # MOVE and ADD at minimum
        mod_types = {m.modification_type for m in mods}
        assert ModificationType.MOVE in mod_types or ModificationType.ADD in mod_types

    def test_modified_variables(self, analyzer):
        """Test getting all modified variables."""
        modified = analyzer.get_modified_variables()
        assert "WS-NUM-A" in modified
        assert "WS-NUM-B" in modified
        assert "WS-TEXT-A" in modified

    def test_sections_modifying_variable(self, analyzer):
        """Test getting sections that modify a variable."""
        sections = analyzer.get_sections_modifying_variable("WS-NUM-A")
        assert "INIT-SECTION" in sections
        assert "PROCESS-SECTION" in sections

    def test_paragraphs_modifying_variable(self, analyzer):
        """Test getting paragraphs that modify a variable."""
        paragraphs = analyzer.get_paragraphs_modifying_variable("WS-TEXT-A")
        assert "UPDATE-PARA" in paragraphs

    def test_modification_types(self, analyzer):
        """Test getting modification types for a variable."""
        types = analyzer.get_modification_types_for_variable("WS-NUM-A")
        # Should have at least MOVE and ADD or MULTIPLY
        assert len(types) >= 1

    def test_most_modified_variables(self, analyzer):
        """Test getting most frequently modified variables."""
        most_modified = analyzer.get_most_modified_variables(limit=5)
        assert len(most_modified) > 0
        # Should be sorted by count (descending)
        if len(most_modified) > 1:
            assert most_modified[0][1] >= most_modified[1][1]

    def test_analysis_summary(self, analyzer):
        """Test analysis summary generation."""
        summary = analyzer.get_analysis_summary()
        assert summary["total_sections"] >= 1
        assert summary["total_modifications"] > 0
        assert summary["unique_modified_variables"] > 0


class TestModificationTypes:
    """Tests for various modification statement types."""

    def parse_and_analyze(self, source):
        """Helper to parse and analyze source."""
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        program = builder.build(tree)
        analyzer = ProcedureAnalyzer(program)
        analyzer.analyze()
        return analyzer

    def test_move_statement(self):
        """Test MOVE statement detection."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVE-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC X(10).
       01 WS-B PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE "HELLO" TO WS-A, WS-B.
        """
        analyzer = self.parse_and_analyze(source)
        mods = analyzer.get_modifications_for_variable("WS-A")
        assert len(mods) >= 1
        assert mods[0].modification_type == ModificationType.MOVE

    def test_compute_statement(self):
        """Test COMPUTE statement detection."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPUTE-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           COMPUTE WS-RESULT = 10 + 20.
        """
        analyzer = self.parse_and_analyze(source)
        mods = analyzer.get_modifications_for_variable("WS-RESULT")
        assert len(mods) >= 1
        assert mods[0].modification_type == ModificationType.COMPUTE

    def test_add_statement(self):
        """Test ADD statement detection."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           ADD 10 TO WS-NUM.
        """
        analyzer = self.parse_and_analyze(source)
        mods = analyzer.get_modifications_for_variable("WS-NUM")
        assert len(mods) >= 1
        assert mods[0].modification_type == ModificationType.ADD
