"""Tests for the parser module."""

import pytest
from pathlib import Path

# Path is setup in conftest.py

from parser.cobol_parser import (
    CobolParser,
    ParseError,
    SimplifiedCobolParser,
    SimplifiedParseTree,
)


class TestSimplifiedParser:
    """Tests for the simplified regex-based parser."""

    @pytest.fixture
    def parser(self):
        """Create a SimplifiedCobolParser."""
        return SimplifiedCobolParser()

    @pytest.fixture
    def simple_source(self):
        """Simple COBOL source for testing."""
        return """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-RECORD.
          05 WS-FIELD-A PIC X(10).
          05 WS-FIELD-B PIC 9(5).

       PROCEDURE DIVISION.

       MAIN-PARA.
           MOVE "HELLO" TO WS-FIELD-A
           ADD 1 TO WS-FIELD-B
           STOP RUN.
        """

    def test_extract_program_name(self, parser, simple_source):
        """Test program name extraction."""
        tree = parser.parse(simple_source)
        assert tree.program_name == "TEST-PROGRAM"

    def test_parse_data_items(self, parser, simple_source):
        """Test data item parsing."""
        tree = parser.parse(simple_source)
        items = tree.data_division.working_storage

        # Should have at least the level 01 and its children
        item_names = [item.name for item in items]
        assert "WS-RECORD" in item_names
        assert "WS-FIELD-A" in item_names
        assert "WS-FIELD-B" in item_names

    def test_parse_data_item_levels(self, parser, simple_source):
        """Test data item level numbers."""
        tree = parser.parse(simple_source)
        items = {item.name: item for item in tree.data_division.working_storage}

        assert items["WS-RECORD"].level == 1
        assert items["WS-FIELD-A"].level == 5
        assert items["WS-FIELD-B"].level == 5

    def test_parse_picture_clauses(self, parser, simple_source):
        """Test PICTURE clause extraction."""
        tree = parser.parse(simple_source)
        items = {item.name: item for item in tree.data_division.working_storage}

        assert items["WS-FIELD-A"].picture == "X(10)"
        assert items["WS-FIELD-B"].picture == "9(5)"

    def test_parse_paragraph(self, parser, simple_source):
        """Test paragraph parsing."""
        tree = parser.parse(simple_source)

        # Should have paragraphs
        assert len(tree.procedure_division.paragraphs) > 0 or \
               any(s.paragraphs for s in tree.procedure_division.sections)

    def test_extract_move_statement(self, parser, simple_source):
        """Test MOVE statement extraction."""
        tree = parser.parse(simple_source)

        # Find all statements
        all_statements = []
        for para in tree.procedure_division.paragraphs:
            all_statements.extend(para.statements)
        for section in tree.procedure_division.sections:
            all_statements.extend(section.statements)
            for para in section.paragraphs:
                all_statements.extend(para.statements)

        # Should have a MOVE statement
        move_stmts = [s for s in all_statements if s.statement_type == "MOVE"]
        assert len(move_stmts) > 0
        assert "WS-FIELD-A" in move_stmts[0].targets

    def test_extract_add_statement(self, parser, simple_source):
        """Test ADD statement extraction."""
        tree = parser.parse(simple_source)

        all_statements = []
        for para in tree.procedure_division.paragraphs:
            all_statements.extend(para.statements)

        add_stmts = [s for s in all_statements if s.statement_type == "ADD"]
        assert len(add_stmts) > 0
        assert "WS-FIELD-B" in add_stmts[0].targets


class TestCobolParser:
    """Tests for the high-level CobolParser wrapper."""

    @pytest.fixture
    def parser(self):
        """Create a CobolParser using simplified mode."""
        return CobolParser(use_generated=False)

    def test_parse_simple_program(self, parser):
        """Test parsing a simple program."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
        """
        tree = parser.parse(source)
        assert tree is not None

    def test_parse_file(self, parser, tmp_path):
        """Test parsing from a file."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-TEST.
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
        """
        test_file = tmp_path / "test.cob"
        test_file.write_text(source)

        tree = parser.parse_file(test_file)
        assert tree.program_name == "FILE-TEST"


class TestRedefinesParsing:
    """Tests for REDEFINES clause parsing."""

    @pytest.fixture
    def parser(self):
        return SimplifiedCobolParser()

    def test_parse_redefines_clause(self, parser):
        """Test parsing REDEFINES clause."""
        source = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REDEF-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RECORD-A.
          05 FIELD-A PIC X(10).
       01 RECORD-B REDEFINES RECORD-A.
          05 FIELD-B PIC 9(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
        """
        tree = parser.parse(source)
        items = {item.name: item for item in tree.data_division.working_storage}

        assert "RECORD-A" in items
        assert "RECORD-B" in items
        assert items["RECORD-B"].redefines == "RECORD-A"
