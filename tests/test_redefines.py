"""Tests for the REDEFINES analyzer module."""

import pytest
from pathlib import Path

# Path is setup in conftest.py

from parser.cobol_parser import CobolParser
from cobol_ast.builder import ASTBuilder
from analyzers.redefines import RedefinesAnalyzer, RedefinesRelation


class TestRedefinesAnalyzer:
    """Tests for REDEFINES relationship analysis."""

    @pytest.fixture
    def redefines_program(self):
        """Create a program with REDEFINES."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REDEF-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 RECORD-A.
          05 FIELD-A1          PIC X(10).
          05 FIELD-A2          PIC X(10).

       01 RECORD-B REDEFINES RECORD-A.
          05 FIELD-B1          PIC 9(10).
          05 FIELD-B2          PIC 9(10).

       01 RECORD-C REDEFINES RECORD-A.
          05 FIELD-C1          PIC X(5).
          05 FIELD-C2          PIC X(5).
          05 FIELD-C3          PIC X(10).

       01 STANDALONE-RECORD.
          05 STAND-FIELD       PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
        """
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        return builder.build(tree)

    @pytest.fixture
    def analyzer(self, redefines_program):
        """Create an analyzer for the REDEFINES program."""
        analyzer = RedefinesAnalyzer(redefines_program)
        analyzer.analyze()
        return analyzer

    def test_affected_records_basic(self, analyzer):
        """Test getting affected records for a simple variable."""
        affected = analyzer.get_affected_records("FIELD-A1")
        # FIELD-A1 is in RECORD-A, which is redefined by B and C
        assert "RECORD-A" in affected
        assert "RECORD-B" in affected
        assert "RECORD-C" in affected

    def test_affected_records_redefining(self, analyzer):
        """Test affected records for variable in redefining record."""
        affected = analyzer.get_affected_records("FIELD-B1")
        # FIELD-B1 is in RECORD-B which redefines RECORD-A
        assert "RECORD-A" in affected
        assert "RECORD-B" in affected
        assert "RECORD-C" in affected

    def test_affected_records_standalone(self, analyzer):
        """Test affected records for standalone record."""
        affected = analyzer.get_affected_records("STAND-FIELD")
        # STANDALONE-RECORD has no REDEFINES relationships
        assert affected == {"STANDALONE-RECORD"}

    def test_redefines_chain(self, analyzer):
        """Test getting REDEFINES chain."""
        chain = analyzer.get_redefines_chain("RECORD-A")
        assert len(chain) == 3
        assert "RECORD-A" in chain
        assert "RECORD-B" in chain
        assert "RECORD-C" in chain

    def test_direct_redefines(self, analyzer):
        """Test getting direct REDEFINES target."""
        target = analyzer.get_direct_redefines("RECORD-B")
        assert target == "RECORD-A"

    def test_items_redefining(self, analyzer):
        """Test getting items that REDEFINE a target."""
        redefining = analyzer.get_items_redefining("RECORD-A")
        assert "RECORD-B" in redefining
        assert "RECORD-C" in redefining

    def test_has_redefines(self, analyzer):
        """Test checking if item has REDEFINES."""
        assert analyzer.has_redefines("RECORD-B")
        assert analyzer.has_redefines("RECORD-C")
        assert not analyzer.has_redefines("RECORD-A")

    def test_is_redefined(self, analyzer):
        """Test checking if item is redefined."""
        assert analyzer.is_redefined("RECORD-A")
        assert not analyzer.is_redefined("RECORD-B")
        assert not analyzer.is_redefined("STANDALONE-RECORD")

    def test_analysis_summary(self, analyzer):
        """Test analysis summary generation."""
        summary = analyzer.get_analysis_summary()
        assert summary["total_redefines_relations"] == 2
        assert summary["record_level_redefines"] == 2
        assert "RECORD-A" in summary["records_with_redefines"]
        assert "RECORD-B" in summary["records_with_redefines"]


class TestSubordinateRedefines:
    """Tests for subordinate (non-01 level) REDEFINES."""

    @pytest.fixture
    def subordinate_redefines_program(self):
        """Create a program with subordinate REDEFINES."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUB-REDEF.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-RECORD.
          05 WS-DATE-FIELD.
             10 WS-YEAR          PIC 9(4).
             10 WS-MONTH         PIC 9(2).
             10 WS-DAY           PIC 9(2).
          05 WS-DATE-NUM REDEFINES WS-DATE-FIELD PIC 9(8).

       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
        """
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        return builder.build(tree)

    def test_subordinate_redefines_detection(self, subordinate_redefines_program):
        """Test detection of subordinate REDEFINES."""
        analyzer = RedefinesAnalyzer(subordinate_redefines_program)
        analyzer.analyze()

        relations = analyzer.get_all_relations()
        assert len(relations) == 1
        assert relations[0].level == 5  # Subordinate level
