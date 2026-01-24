"""Tests for the data analyzer module."""

import pytest
from pathlib import Path

# Path is setup in conftest.py

from parser.cobol_parser import CobolParser
from cobol_ast.builder import ASTBuilder
from analyzers.data_analyzer import DataStructureAnalyzer, MemoryRegion


class TestDataStructureAnalyzer:
    """Tests for data structure analysis."""

    @pytest.fixture
    def simple_program(self):
        """Create a simple program AST."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-EMPLOYEE.
          05 WS-EMP-ID          PIC 9(5).
          05 WS-EMP-NAME        PIC X(30).
          05 WS-EMP-ADDRESS.
             10 WS-STREET       PIC X(40).
             10 WS-CITY         PIC X(20).
             10 WS-STATE        PIC X(2).

       01 WS-COUNTERS.
          05 WS-CTR-A           PIC 9(3).
          05 WS-CTR-B           PIC 9(3).

       77 WS-INDEPENDENT        PIC X(10).

       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
        """
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        return builder.build(tree)

    @pytest.fixture
    def analyzer(self, simple_program):
        """Create an analyzer for the simple program."""
        analyzer = DataStructureAnalyzer(simple_program)
        analyzer.analyze()
        return analyzer

    def test_record_for_variable(self, analyzer):
        """Test getting the record for a variable."""
        record = analyzer.get_record_for_variable("WS-EMP-ID")
        assert record is not None
        assert record.name == "WS-EMPLOYEE"

    def test_record_for_nested_variable(self, analyzer):
        """Test getting record for a nested variable."""
        record = analyzer.get_record_for_variable("WS-STREET")
        assert record is not None
        assert record.name == "WS-EMPLOYEE"

    def test_record_for_level_77(self, analyzer):
        """Test handling of level 77 independent items."""
        record = analyzer.get_record_for_variable("WS-INDEPENDENT")
        assert record is not None
        assert record.name == "WS-INDEPENDENT"

    def test_variables_in_record(self, analyzer):
        """Test getting all variables in a record."""
        variables = analyzer.get_variables_in_record("WS-EMPLOYEE")
        assert "WS-EMPLOYEE" in variables
        assert "WS-EMP-ID" in variables
        assert "WS-EMP-NAME" in variables
        assert "WS-STREET" in variables
        assert "WS-CTR-A" not in variables

    def test_group_items(self, analyzer):
        """Test identification of group items."""
        groups = analyzer.get_group_items()
        # WS-EMPLOYEE and WS-EMP-ADDRESS are groups
        assert "WS-EMPLOYEE" in groups
        assert "WS-EMP-ADDRESS" in groups
        # Elementary items should not be groups
        assert "WS-EMP-ID" not in groups

    def test_elementary_items(self, analyzer):
        """Test identification of elementary items."""
        elementary = analyzer.get_elementary_items()
        assert "WS-EMP-ID" in elementary
        assert "WS-STREET" in elementary
        # Group items should not be elementary
        assert "WS-EMPLOYEE" not in elementary

    def test_item_hierarchy(self, analyzer):
        """Test getting item hierarchy path."""
        hierarchy = analyzer.get_item_hierarchy("WS-STREET")
        names = [item.name for item in hierarchy]
        assert names == ["WS-EMPLOYEE", "WS-EMP-ADDRESS", "WS-STREET"]

    def test_siblings(self, analyzer):
        """Test getting sibling items."""
        siblings = analyzer.get_siblings("WS-CTR-A")
        sibling_names = [s.name for s in siblings]
        assert "WS-CTR-B" in sibling_names

    def test_find_items_by_pattern(self, analyzer):
        """Test finding items by pattern."""
        items = analyzer.find_items_by_pattern("WS-CTR-*")
        names = [item.name for item in items]
        assert "WS-CTR-A" in names
        assert "WS-CTR-B" in names
        assert "WS-EMP-ID" not in names

    def test_analysis_summary(self, analyzer):
        """Test analysis summary generation."""
        summary = analyzer.get_analysis_summary()
        assert summary["total_records"] >= 2  # WS-EMPLOYEE, WS-COUNTERS
        assert summary["total_data_items"] > 0
        assert "WS-EMPLOYEE" in summary["record_names"]


class TestMemoryRegions:
    """Tests for memory region calculations."""

    @pytest.fixture
    def memory_program(self):
        """Create a program for memory region testing."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEM-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-RECORD.
          05 WS-FIELD-A         PIC X(10).
          05 WS-FIELD-B         PIC 9(5).
          05 WS-GROUP.
             10 WS-SUB-1        PIC X(3).
             10 WS-SUB-2        PIC 9(2).

       01 WS-REDEF-RECORD.
          05 WS-ORIGINAL        PIC X(10).
          05 WS-REDEF REDEFINES WS-ORIGINAL.
             10 WS-PART1        PIC X(5).
             10 WS-PART2        PIC X(5).

       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
        """
        parser = CobolParser(use_generated=False)
        tree = parser.parse(source)
        builder = ASTBuilder()
        return builder.build(tree)

    @pytest.fixture
    def memory_analyzer(self, memory_program):
        """Create an analyzer for memory testing."""
        analyzer = DataStructureAnalyzer(memory_program)
        analyzer.analyze()
        return analyzer

    def test_memory_region_basic(self, memory_analyzer):
        """Test basic memory region retrieval."""
        region = memory_analyzer.get_memory_region("WS-FIELD-A")
        assert region is not None
        assert region.item_name == "WS-FIELD-A"
        assert region.size == 10  # PIC X(10) = 10 bytes

    def test_memory_region_offset(self, memory_analyzer):
        """Test memory offset calculations."""
        # WS-FIELD-A is first, so offset should be 0
        region_a = memory_analyzer.get_memory_region("WS-FIELD-A")
        assert region_a.start_offset == 0

        # WS-FIELD-B comes after WS-FIELD-A (10 bytes)
        region_b = memory_analyzer.get_memory_region("WS-FIELD-B")
        assert region_b.start_offset == 10
        assert region_b.size == 5  # PIC 9(5) = 5 bytes

    def test_memory_region_nested(self, memory_analyzer):
        """Test memory regions for nested items."""
        # WS-GROUP starts after WS-FIELD-A (10) + WS-FIELD-B (5) = 15
        region_group = memory_analyzer.get_memory_region("WS-GROUP")
        assert region_group.start_offset == 15

        # WS-SUB-1 starts at beginning of WS-GROUP
        region_sub1 = memory_analyzer.get_memory_region("WS-SUB-1")
        assert region_sub1.start_offset == 15

        # WS-SUB-2 starts after WS-SUB-1 (3 bytes)
        region_sub2 = memory_analyzer.get_memory_region("WS-SUB-2")
        assert region_sub2.start_offset == 18
        assert region_sub2.size == 2

    def test_memory_region_redefines(self, memory_analyzer):
        """Test memory regions for REDEFINES items."""
        # WS-ORIGINAL and WS-REDEF should have same start_offset
        region_orig = memory_analyzer.get_memory_region("WS-ORIGINAL")
        region_redef = memory_analyzer.get_memory_region("WS-REDEF")

        assert region_orig.start_offset == region_redef.start_offset

        # WS-PART1 should be at same offset as WS-REDEF
        region_part1 = memory_analyzer.get_memory_region("WS-PART1")
        assert region_part1.start_offset == region_redef.start_offset

    def test_memory_region_overlaps(self, memory_analyzer):
        """Test memory region overlap detection."""
        region_orig = memory_analyzer.get_memory_region("WS-ORIGINAL")
        region_part1 = memory_analyzer.get_memory_region("WS-PART1")

        # They should overlap since REDEF shares memory
        assert region_orig.overlaps(region_part1)
        assert region_part1.overlaps(region_orig)

    def test_get_item_offset(self, memory_analyzer):
        """Test getting item offset directly."""
        offset = memory_analyzer.get_item_offset("WS-FIELD-B")
        assert offset == 10

    def test_get_item_size(self, memory_analyzer):
        """Test getting item size directly."""
        size = memory_analyzer.get_item_size("WS-FIELD-A")
        assert size == 10

    def test_get_overlapping_items(self, memory_analyzer):
        """Test finding overlapping items."""
        overlapping = memory_analyzer.get_overlapping_items("WS-ORIGINAL")

        # Should find WS-REDEF and its children
        assert "WS-REDEF" in overlapping
        assert "WS-PART1" in overlapping
        assert "WS-PART2" in overlapping
