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


class TestLineNumberAccuracy:
    """Tests for accurate line number calculation in statements."""

    @pytest.fixture
    def parser(self):
        return SimplifiedCobolParser()

    def test_line_numbers_in_single_paragraph(self, parser):
        """Test that line numbers are accurate for statements in a paragraph."""
        # Line numbers are 1-indexed from the start of the source
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINE-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR-A PIC X(10).
       01 WS-VAR-B PIC X(10).
       01 WS-VAR-C PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE "A" TO WS-VAR-A.
           MOVE "B" TO WS-VAR-B.
           MOVE "C" TO WS-VAR-C.
           STOP RUN.
"""
        tree = parser.parse(source)

        # Get statements from paragraph
        statements = tree.procedure_division.paragraphs[0].statements
        move_stmts = [s for s in statements if s.statement_type == "MOVE"]

        assert len(move_stmts) == 3
        # Each MOVE should be on consecutive lines starting at line 10
        assert move_stmts[0].line_number == 10
        assert move_stmts[1].line_number == 11
        assert move_stmts[2].line_number == 12

    def test_line_numbers_across_sections(self, parser):
        """Test that line numbers are accurate across different sections."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECTION-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR-A PIC X(10).
       01 WS-VAR-B PIC X(10).
       PROCEDURE DIVISION.
       SECTION-ONE SECTION.
       PARA-ONE.
           MOVE "A" TO WS-VAR-A.
       SECTION-TWO SECTION.
       PARA-TWO.
           MOVE "B" TO WS-VAR-B.
"""
        tree = parser.parse(source)

        # Get statements from each section
        section_one = tree.procedure_division.sections[0]
        section_two = tree.procedure_division.sections[1]

        stmt_one = section_one.paragraphs[0].statements[0]
        stmt_two = section_two.paragraphs[0].statements[0]

        # Each MOVE should have correct absolute line number
        assert stmt_one.line_number == 10
        assert stmt_two.line_number == 13

    def test_line_numbers_multiline_statements_without_periods(self, parser):
        """Test line numbers when multiple statements span lines without periods."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTI-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC X(10).
       01 WS-B PIC X(10).
       01 WS-C PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE "X" TO WS-A
           MOVE "Y" TO WS-B
           MOVE "Z" TO WS-C.
"""
        tree = parser.parse(source)

        statements = tree.procedure_division.paragraphs[0].statements
        move_stmts = [s for s in statements if s.statement_type == "MOVE"]

        assert len(move_stmts) == 3
        # Each statement should be on its own line
        assert move_stmts[0].line_number == 10
        assert move_stmts[1].line_number == 11
        assert move_stmts[2].line_number == 12


class TestKeywordFiltering:
    """Tests for COBOL keyword filtering in variable extraction."""

    @pytest.fixture
    def parser(self):
        return SimplifiedCobolParser()

    def test_keywords_not_captured_as_variables(self, parser):
        """Test that COBOL keywords are not captured as variable names."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KEYWORD-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER PIC 9(5).
       01 WS-INDEX PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 10
               ADD 1 TO WS-COUNTER
           END-PERFORM.
"""
        tree = parser.parse(source)

        # Get all statements
        statements = tree.procedure_division.paragraphs[0].statements

        # Collect all targets
        all_targets = []
        for stmt in statements:
            all_targets.extend(stmt.targets)

        # Keywords should never appear as targets
        keywords = {"PERFORM", "VARYING", "FROM", "BY", "UNTIL", "END-PERFORM",
                    "ADD", "TO", "MOVE", "IF", "ELSE", "END-IF"}
        for target in all_targets:
            assert target.upper() not in keywords, f"Keyword '{target}' found as variable"

    def test_split_variable_list_filters_keywords(self, parser):
        """Test that _split_variable_list filters out COBOL keywords."""
        # Direct test of the helper method
        result = parser._split_variable_list("WS-VAR-A, WS-VAR-B")
        assert result == ["WS-VAR-A", "WS-VAR-B"]

        # With keywords mixed in (shouldn't happen in real code, but tests filter)
        result = parser._split_variable_list("WS-VAR-A TO WS-VAR-B")
        assert "TO" not in result
        assert "WS-VAR-A" in result
        assert "WS-VAR-B" in result


class TestTargetOnlyExtraction:
    """Tests for ensuring only TARGET variables are captured, not SOURCE variables."""

    @pytest.fixture
    def parser(self):
        return SimplifiedCobolParser()

    def test_move_captures_only_target(self, parser):
        """Test that MOVE statement captures only the target, not the source."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVE-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE PIC X(10).
       01 WS-TARGET PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE WS-SOURCE TO WS-TARGET.
"""
        tree = parser.parse(source)

        statements = tree.procedure_division.paragraphs[0].statements
        move_stmt = [s for s in statements if s.statement_type == "MOVE"][0]

        # Only target should be captured
        assert "WS-TARGET" in move_stmt.targets
        assert "WS-SOURCE" not in move_stmt.targets

    def test_move_multiple_targets_no_source(self, parser):
        """Test MOVE with multiple targets doesn't include source."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTI-TARGET.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SOURCE PIC X(10).
       01 WS-TARGET-A PIC X(10).
       01 WS-TARGET-B PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE WS-SOURCE TO WS-TARGET-A, WS-TARGET-B.
"""
        tree = parser.parse(source)

        statements = tree.procedure_division.paragraphs[0].statements
        move_stmt = [s for s in statements if s.statement_type == "MOVE"][0]

        # Both targets should be captured, but not the source
        assert "WS-TARGET-A" in move_stmt.targets
        assert "WS-TARGET-B" in move_stmt.targets
        assert "WS-SOURCE" not in move_stmt.targets

    def test_consecutive_moves_no_source_leakage(self, parser):
        """Test that consecutive MOVE statements don't leak source variables."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSECUTIVE-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATE-MONTH PIC 9(2).
       01 WS-DATE-DAY PIC 9(2).
       01 WS-DATE-YEAR PIC 9(4).
       01 WS-US-MONTH PIC 9(2).
       01 WS-US-DAY PIC 9(2).
       01 WS-US-YEAR PIC 9(4).
       PROCEDURE DIVISION.
       CONVERT-DATE.
           MOVE WS-DATE-MONTH TO WS-US-MONTH
           MOVE WS-DATE-DAY TO WS-US-DAY
           MOVE WS-DATE-YEAR TO WS-US-YEAR.
"""
        tree = parser.parse(source)

        statements = tree.procedure_division.paragraphs[0].statements
        move_stmts = [s for s in statements if s.statement_type == "MOVE"]

        assert len(move_stmts) == 3

        # Each MOVE should only capture its target
        assert move_stmts[0].targets == ["WS-US-MONTH"]
        assert move_stmts[1].targets == ["WS-US-DAY"]
        assert move_stmts[2].targets == ["WS-US-YEAR"]

        # Source variables should not appear in any targets
        all_targets = []
        for stmt in move_stmts:
            all_targets.extend(stmt.targets)

        assert "WS-DATE-MONTH" not in all_targets
        assert "WS-DATE-DAY" not in all_targets
        assert "WS-DATE-YEAR" not in all_targets

    def test_add_captures_only_target(self, parser):
        """Test that ADD statement captures only the target variable."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AMOUNT PIC 9(5).
       01 WS-TOTAL PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           ADD WS-AMOUNT TO WS-TOTAL.
"""
        tree = parser.parse(source)

        statements = tree.procedure_division.paragraphs[0].statements
        add_stmt = [s for s in statements if s.statement_type == "ADD"][0]

        # Only target (WS-TOTAL) should be captured
        assert "WS-TOTAL" in add_stmt.targets
        assert "WS-AMOUNT" not in add_stmt.targets


class TestOccursParsing:
    """Tests for OCCURS clause parsing including OCCURS n TIMES syntax."""

    @pytest.fixture
    def parser(self):
        return SimplifiedCobolParser()

    def test_occurs_basic(self, parser):
        """Test basic OCCURS n clause."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          05 WS-ITEM OCCURS 10 PIC X(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
"""
        tree = parser.parse(source)
        items = {item.name: item for item in tree.data_division.working_storage}

        assert "WS-ITEM" in items
        assert items["WS-ITEM"].occurs == 10
        assert items["WS-ITEM"].picture == "X(5)"

    def test_occurs_times(self, parser):
        """Test OCCURS n TIMES syntax."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS-TIMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          05 WS-ITEM OCCURS 50 TIMES PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
"""
        tree = parser.parse(source)
        items = {item.name: item for item in tree.data_division.working_storage}

        assert "WS-ITEM" in items
        assert items["WS-ITEM"].occurs == 50
        assert items["WS-ITEM"].picture == "X(10)"

    def test_filler_occurs_times(self, parser):
        """Test FILLER with OCCURS n TIMES."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILLER-OCCURS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          02 FILLER OCCURS 50 TIMES.
             05 WS-FIELD-A PIC X(5).
             05 WS-FIELD-B PIC 9(3).
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
"""
        tree = parser.parse(source)
        items = {item.name: item for item in tree.data_division.working_storage}

        # Find the FILLER item (will have FILLER$n name)
        filler_items = [i for name, i in items.items() if "FILLER" in name]
        assert len(filler_items) >= 1
        filler = filler_items[0]
        assert filler.occurs == 50
        assert filler.is_filler is True

    def test_occurs_different_order(self, parser):
        """Test OCCURS clause in different positions relative to PIC."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS-ORDER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          05 WS-ITEM-A PIC X(5) OCCURS 10.
          05 WS-ITEM-B OCCURS 20 PIC 9(3).
          05 WS-ITEM-C OCCURS 30 TIMES.
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
"""
        tree = parser.parse(source)
        items = {item.name: item for item in tree.data_division.working_storage}

        assert items["WS-ITEM-A"].occurs == 10
        assert items["WS-ITEM-A"].picture == "X(5)"
        assert items["WS-ITEM-B"].occurs == 20
        assert items["WS-ITEM-B"].picture == "9(3)"
        assert items["WS-ITEM-C"].occurs == 30


class TestDataItemLineNumbers:
    """Tests for accurate line number assignment to data items."""

    @pytest.fixture
    def parser(self):
        return SimplifiedCobolParser()

    def test_data_item_line_numbers(self, parser):
        """Test that data items get correct line numbers."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINE-TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RECORD.
          05 WS-FIELD-A PIC X(10).
          05 WS-FIELD-B PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN.
"""
        tree = parser.parse(source)
        items = {item.name: item for item in tree.data_division.working_storage}

        # Line numbers should be accurate (1-indexed)
        assert items["WS-RECORD"].line_number == 5
        assert items["WS-FIELD-A"].line_number == 6
        assert items["WS-FIELD-B"].line_number == 7
