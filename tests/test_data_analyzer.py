"""Tests for the data analyzer module."""

import pytest
from pathlib import Path

# Path is setup in conftest.py

from parser.cobol_parser import CobolParser
from cobol_ast.builder import ASTBuilder
from analyzers.data_analyzer import DataStructureAnalyzer


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
