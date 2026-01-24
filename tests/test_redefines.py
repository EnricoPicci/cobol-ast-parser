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

    def test_ancestor_redefines(self, subordinate_redefines_program):
        """Test getting REDEFINES in ancestor chain."""
        analyzer = RedefinesAnalyzer(subordinate_redefines_program)
        analyzer.analyze()

        # WS-YEAR is under WS-DATE-FIELD, no direct REDEFINES in ancestors
        ancestor_redefines = analyzer.get_ancestor_redefines("WS-YEAR")
        assert len(ancestor_redefines) == 0

        # WS-DATE-NUM itself has REDEFINES
        ancestor_redefines = analyzer.get_ancestor_redefines("WS-DATE-NUM")
        assert len(ancestor_redefines) == 1
        assert ancestor_redefines[0].redefining_item == "WS-DATE-NUM"
        assert ancestor_redefines[0].redefined_item == "WS-DATE-FIELD"

    def test_overlapping_variables_from_redefines(self, subordinate_redefines_program):
        """Test finding overlapping variables due to REDEFINES."""
        analyzer = RedefinesAnalyzer(subordinate_redefines_program)
        analyzer.analyze()

        # WS-DATE-NUM overlaps with WS-DATE-FIELD and its children
        overlapping = analyzer.get_overlapping_variables("WS-DATE-NUM")
        overlapping_names = [v.name for v in overlapping]

        assert "WS-DATE-FIELD" in overlapping_names
        assert "WS-YEAR" in overlapping_names
        assert "WS-MONTH" in overlapping_names
        assert "WS-DAY" in overlapping_names

    def test_overlapping_variables_from_subordinate(self, subordinate_redefines_program):
        """Test finding overlapping from subordinate's perspective."""
        analyzer = RedefinesAnalyzer(subordinate_redefines_program)
        analyzer.analyze()

        # WS-YEAR is in WS-DATE-FIELD which is redefined by WS-DATE-NUM
        overlapping = analyzer.get_overlapping_variables("WS-YEAR")
        overlapping_names = [v.name for v in overlapping]

        # Should find WS-DATE-NUM as overlapping
        assert "WS-DATE-NUM" in overlapping_names


class TestNestedSubordinateRedefines:
    """Tests for nested subordinate REDEFINES scenarios."""

    @pytest.fixture
    def nested_redefines_program(self):
        """Create a program with nested subordinate REDEFINES."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NESTED-REDEF.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 MYLEVEL-01.
          05 MYLEVEL-05.
             10 MYVAR-10         PIC 9(5).
             10 MYVAR-10B        PIC X(5).
          05 MYLEVEL-05-REDEF REDEFINES MYLEVEL-05.
             10 OTHER-VAR-10     PIC X(5).
             10 OTHER-VAR-10B    PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 12345 TO MYVAR-10.
           STOP RUN.
        """
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        return builder.build(tree)

    def test_nested_overlapping_variables(self, nested_redefines_program):
        """Test the exact scenario from the user's request."""
        analyzer = RedefinesAnalyzer(nested_redefines_program)
        analyzer.analyze()

        # When MYVAR-10 changes, we should find variables in MYLEVEL-05-REDEF
        overlapping = analyzer.get_overlapping_variables("MYVAR-10")
        overlapping_names = [v.name for v in overlapping]

        # Should find variables from the REDEFINES structure
        assert "OTHER-VAR-10" in overlapping_names or "OTHER-VAR-10B" in overlapping_names
        assert "MYLEVEL-05-REDEF" in overlapping_names

    def test_redefines_chain_info(self, nested_redefines_program):
        """Test that REDEFINES chain information is provided."""
        analyzer = RedefinesAnalyzer(nested_redefines_program)
        analyzer.analyze()

        overlapping = analyzer.get_overlapping_variables("MYVAR-10")

        # Find one of the affected variables and check its info
        for av in overlapping:
            if av.name == "OTHER-VAR-10":
                assert "MYLEVEL-05-REDEF" in av.redefines_chain
                assert "MYLEVEL-05" in av.redefines_chain
                assert av.redefines_level == 5
                break

    def test_all_affected_variables_method(self, nested_redefines_program):
        """Test the combined affected records and variables method."""
        analyzer = RedefinesAnalyzer(nested_redefines_program)
        analyzer.analyze()

        result = analyzer.get_all_affected_variables("MYVAR-10")

        assert "MYLEVEL-01" in result["records"]
        assert len(result["variables"]) > 0


class TestMultipleRedefinesOfSameItem:
    """Tests for multiple items redefining the same target."""

    @pytest.fixture
    def multi_redefines_program(self):
        """Create a program where multiple items redefine the same target."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTI-REDEF.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-DATA.
          05 WS-ORIGINAL.
             10 WS-PART1         PIC X(5).
             10 WS-PART2         PIC X(5).
          05 WS-REDEF-A REDEFINES WS-ORIGINAL.
             10 WS-NUM-VIEW      PIC 9(10).
          05 WS-REDEF-B REDEFINES WS-ORIGINAL.
             10 WS-FULL-TEXT     PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
        """
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        return builder.build(tree)

    def test_multiple_redefines_overlapping(self, multi_redefines_program):
        """Test overlapping when multiple items redefine same target."""
        analyzer = RedefinesAnalyzer(multi_redefines_program)
        analyzer.analyze()

        # WS-PART1 should overlap with items from both WS-REDEF-A and WS-REDEF-B
        overlapping = analyzer.get_overlapping_variables("WS-PART1")
        overlapping_names = [v.name for v in overlapping]

        # Should see items from both redefining structures
        assert "WS-NUM-VIEW" in overlapping_names
        assert "WS-FULL-TEXT" in overlapping_names
        assert "WS-REDEF-A" in overlapping_names
        assert "WS-REDEF-B" in overlapping_names

    def test_all_related_items_found(self, multi_redefines_program):
        """Test that all related items are found through REDEFINES chain."""
        analyzer = RedefinesAnalyzer(multi_redefines_program)
        analyzer.analyze()

        # From WS-NUM-VIEW (in WS-REDEF-A), should find items in original and WS-REDEF-B
        overlapping = analyzer.get_overlapping_variables("WS-NUM-VIEW")
        overlapping_names = [v.name for v in overlapping]

        assert "WS-ORIGINAL" in overlapping_names
        assert "WS-PART1" in overlapping_names
        assert "WS-PART2" in overlapping_names
        assert "WS-REDEF-B" in overlapping_names
        assert "WS-FULL-TEXT" in overlapping_names
