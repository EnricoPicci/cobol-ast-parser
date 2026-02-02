"""AST Builder for constructing domain-specific AST from parse tree.

This module provides the ASTBuilder class that transforms the parser
output (either ANTLR4 parse tree or simplified parse tree) into
the domain-specific AST defined in nodes.py.
"""

from typing import Dict, List, Optional, Tuple
from .nodes import (
    DataItem,
    RecordDescription,
    VariableModification,
    VariableAccess,
    ModificationType,
    Paragraph,
    Section,
    CobolProgram,
)
from parser.cobol_parser import (
    SimplifiedParseTree,
    SimplifiedDataDivision,
    SimplifiedDataItem,
    SimplifiedProcedureDivision,
    SimplifiedSection,
    SimplifiedParagraph,
    SimplifiedStatement,
)


class ASTBuilder:
    """Builds domain-specific AST from parser output.

    This class can work with either ANTLR4 parse trees or the
    simplified parse tree produced by the regex-based parser.
    """

    # COBOL keywords that should not be captured as variable names
    # (backup filter in case parser misses some)
    COBOL_KEYWORDS = {
        "MOVE", "TO", "FROM", "BY", "GIVING", "REMAINDER", "ROUNDED",
        "PERFORM", "VARYING", "UNTIL", "TIMES", "THRU", "THROUGH",
        "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE", "INTO",
        "IF", "ELSE", "END-IF", "THEN", "NOT", "AND", "OR",
        "DISPLAY", "ACCEPT", "READ", "WRITE", "REWRITE", "DELETE",
        "OPEN", "CLOSE", "START", "STOP", "RUN",
        "CALL", "USING", "RETURNING", "ON", "SIZE", "ERROR",
        "STRING", "UNSTRING", "INSPECT", "TALLYING", "REPLACING",
        "SET", "TRUE", "FALSE", "SEARCH", "WHEN", "ALL", "AT", "END",
        "INITIALIZE", "WITH", "FILLER", "CORRESPONDING", "CORR",
        "EVALUATE", "OTHER", "END-EVALUATE", "GO", "GOTO",
        "INPUT", "OUTPUT", "I-O", "EXTEND", "OVERFLOW", "EXCEPTION",
        "CONVERTING", "DELIMITED", "POINTER", "COUNT",
        "NUMERIC", "ALPHABETIC", "POSITIVE", "NEGATIVE",
        "CONTINUE", "EXIT", "NEXT", "SENTENCE",
        "AFTER", "BEFORE", "INITIAL", "REFERENCE", "CONTENT", "VALUE",
    }

    def __init__(self):
        self._all_data_items: Dict[str, DataItem] = {}
        self._record_descriptions: Dict[str, RecordDescription] = {}
        self._current_section: Optional[str] = None

    def build(self, parse_tree) -> CobolProgram:
        """Build AST from parse tree.

        Args:
            parse_tree: Either ANTLR4 parse tree or SimplifiedParseTree

        Returns:
            CobolProgram AST
        """
        if isinstance(parse_tree, SimplifiedParseTree):
            return self._build_from_simplified(parse_tree)
        else:
            # ANTLR4 parse tree - use visitor pattern
            return self._build_from_antlr(parse_tree)

    def _build_from_simplified(self, tree: SimplifiedParseTree) -> CobolProgram:
        """Build AST from SimplifiedParseTree."""
        self._all_data_items = {}
        self._record_descriptions = {}

        program = CobolProgram(
            name=tree.program_name or "UNKNOWN",
            source_lines=tree.source_lines,
        )

        # Build data items and record descriptions
        self._build_data_division(tree.data_division, program)

        # Build procedure division (sections and paragraphs)
        self._build_procedure_division(tree.procedure_division, program)

        return program

    def _build_data_division(
        self, data_div: SimplifiedDataDivision, program: CobolProgram
    ):
        """Build data items from data division."""
        # Process each section
        for section_name, items in [
            ("WORKING-STORAGE", data_div.working_storage),
            ("FILE", data_div.file_section),
            ("LINKAGE", data_div.linkage_section),
            ("LOCAL-STORAGE", data_div.local_storage),
        ]:
            self._process_data_items(items, section_name, program)

    def _process_data_items(
        self,
        items: List[SimplifiedDataItem],
        section_name: str,
        program: CobolProgram,
    ):
        """Process a list of data items and build hierarchy."""
        if not items:
            return

        # Build the level hierarchy
        item_stack: List[DataItem] = []
        current_01: Optional[DataItem] = None

        for sitem in items:
            data_item = DataItem(
                name=sitem.name.upper(),
                level=sitem.level,
                picture=sitem.picture,
                redefines=sitem.redefines.upper() if sitem.redefines else None,
                occurs=sitem.occurs,
                value=sitem.value,
                line_number=sitem.line_number,
                is_filler=sitem.is_filler,
            )

            # Handle level hierarchy
            if sitem.level == 1 or sitem.level == 77:
                # New record or independent item
                current_01 = data_item
                item_stack = [data_item]

                # Create record description
                record = RecordDescription(
                    name=data_item.name,
                    root_item=data_item,
                    redefines=data_item.redefines,
                    section=section_name,
                )
                program.record_descriptions[data_item.name] = record
                self._record_descriptions[data_item.name] = record

            elif sitem.level == 66:
                # RENAMES - special handling
                if current_01:
                    data_item.parent = current_01
                    current_01.children.append(data_item)

            elif sitem.level == 88:
                # Condition name - attach to previous item
                if item_stack:
                    parent = item_stack[-1]
                    data_item.parent = parent
                    parent.children.append(data_item)

            else:
                # Regular subordinate item (02-49)
                # Find parent based on level number
                while item_stack and item_stack[-1].level >= sitem.level:
                    item_stack.pop()

                if item_stack:
                    parent = item_stack[-1]
                    data_item.parent = parent
                    parent.children.append(data_item)
                elif current_01:
                    data_item.parent = current_01
                    current_01.children.append(data_item)

                item_stack.append(data_item)

            # Add to all_data_items index
            program.all_data_items[data_item.name] = data_item
            self._all_data_items[data_item.name] = data_item

    def _build_procedure_division(
        self, proc_div: SimplifiedProcedureDivision, program: CobolProgram
    ):
        """Build sections and paragraphs from procedure division."""
        # Process orphan statements (those not in any paragraph or section)
        if proc_div.orphan_statements:
            program.orphan_modifications = self._extract_modifications(
                proc_div.orphan_statements, None, None
            )
            program.orphan_accesses = self._extract_accesses(
                proc_div.orphan_statements, None, None
            )

        # Process sections
        for ssection in proc_div.sections:
            section = self._build_section(ssection)
            program.sections.append(section)

        # Process top-level paragraphs (those not in sections)
        for spara in proc_div.paragraphs:
            paragraph = self._build_paragraph(spara, None)
            program.paragraphs.append(paragraph)

    def _build_section(self, ssection: SimplifiedSection) -> Section:
        """Build a Section from SimplifiedSection."""
        section = Section(
            name=ssection.name.upper(),
            line_number=ssection.line_number,
        )

        # Process standalone statements (not in paragraphs)
        section.standalone_modifications = self._extract_modifications(
            ssection.statements, ssection.name, None
        )
        section.standalone_accesses = self._extract_accesses(
            ssection.statements, ssection.name, None
        )

        # Process paragraphs
        for spara in ssection.paragraphs:
            paragraph = self._build_paragraph(spara, ssection.name)
            section.paragraphs.append(paragraph)

        return section

    def _build_paragraph(
        self, spara: SimplifiedParagraph, section_name: Optional[str]
    ) -> Paragraph:
        """Build a Paragraph from SimplifiedParagraph."""
        paragraph = Paragraph(
            name=spara.name.upper(),
            line_number=spara.line_number,
            parent_section=section_name,
        )

        paragraph.modifications = self._extract_modifications(
            spara.statements, section_name, spara.name
        )
        paragraph.accesses = self._extract_accesses(
            spara.statements, section_name, spara.name
        )

        return paragraph

    def _extract_modifications(
        self,
        statements: List[SimplifiedStatement],
        section_name: Optional[str],
        paragraph_name: Optional[str],
    ) -> List[VariableModification]:
        """Extract variable modifications from statements."""
        modifications = []

        for stmt in statements:
            mod_type = ModificationType.from_string(stmt.statement_type)

            for target in stmt.targets:
                # Normalize variable name
                var_name = target.upper()

                # Skip literals and special values
                if self._is_literal(var_name):
                    continue

                # Skip COBOL keywords (backup filter)
                if var_name in self.COBOL_KEYWORDS:
                    continue

                modification = VariableModification(
                    variable_name=var_name,
                    modification_type=mod_type,
                    line_number=stmt.line_number,
                    statement_text=stmt.text,
                    section_name=section_name,
                    paragraph_name=paragraph_name,
                )
                modifications.append(modification)

        return modifications

    def _extract_accesses(
        self,
        statements: List[SimplifiedStatement],
        section_name: Optional[str],
        paragraph_name: Optional[str],
    ) -> List[VariableAccess]:
        """Extract variable accesses (reads) from statements."""
        accesses = []

        for stmt in statements:
            # Determine access context based on statement type
            access_context = f"{stmt.statement_type}_SOURCE"

            for source in stmt.sources:
                # Normalize variable name
                var_name = source.upper()

                # Skip literals and special values
                if self._is_literal(var_name):
                    continue

                # Skip COBOL keywords (backup filter)
                if var_name in self.COBOL_KEYWORDS:
                    continue

                access = VariableAccess(
                    variable_name=var_name,
                    access_context=access_context,
                    line_number=stmt.line_number,
                    statement_text=stmt.text,
                    section_name=section_name,
                    paragraph_name=paragraph_name,
                )
                accesses.append(access)

        return accesses

    def _is_literal(self, text: str) -> bool:
        """Check if text is a literal value rather than a variable name."""
        text = text.strip()

        # Numeric literals
        if text.lstrip("-+").replace(".", "").isdigit():
            return True

        # String literals
        if (text.startswith('"') and text.endswith('"')) or \
           (text.startswith("'") and text.endswith("'")):
            return True

        # Figurative constants
        figuratives = {
            "ZERO", "ZEROS", "ZEROES",
            "SPACE", "SPACES",
            "HIGH-VALUE", "HIGH-VALUES",
            "LOW-VALUE", "LOW-VALUES",
            "QUOTE", "QUOTES",
            "NULL", "NULLS",
            "ALL",
        }
        if text.upper() in figuratives:
            return True

        return False

    def _build_from_antlr(self, parse_tree) -> CobolProgram:
        """Build AST from ANTLR4 parse tree.

        This method would use the ANTLR4 visitor pattern to walk
        the parse tree and build the AST. Implementation requires
        the generated visitor class.
        """
        # This is a placeholder for ANTLR4 support
        # Full implementation would use a custom visitor class

        # For now, convert ANTLR tree to simplified format
        # and use the existing logic
        raise NotImplementedError(
            "ANTLR4 AST building not yet implemented. "
            "Use simplified parser or implement Cobol85Visitor."
        )


class ASTBuilderVisitor:
    """ANTLR4 visitor for building AST.

    This class would implement the Cobol85Visitor interface
    to walk the ANTLR4 parse tree and build the domain AST.

    Note: This is a skeleton - full implementation requires
    the generated Cobol85Visitor base class.
    """

    def __init__(self):
        self.program: Optional[CobolProgram] = None
        self._all_data_items: Dict[str, DataItem] = {}
        self._current_section: Optional[Section] = None
        self._current_paragraph: Optional[Paragraph] = None

    # Visitor methods would be implemented here when ANTLR4
    # generated files are available:

    # def visitStartRule(self, ctx):
    #     """Visit the start rule (program entry point)."""
    #     pass

    # def visitIdentificationDivision(self, ctx):
    #     """Extract program name."""
    #     pass

    # def visitDataDivision(self, ctx):
    #     """Process DATA DIVISION."""
    #     pass

    # def visitDataDescriptionEntry(self, ctx):
    #     """Process a data item definition."""
    #     pass

    # def visitProcedureDivision(self, ctx):
    #     """Process PROCEDURE DIVISION."""
    #     pass

    # def visitSectionHeader(self, ctx):
    #     """Process section header."""
    #     pass

    # def visitParagraph(self, ctx):
    #     """Process paragraph."""
    #     pass

    # def visitMoveStatement(self, ctx):
    #     """Track MOVE statement targets."""
    #     pass

    # def visitComputeStatement(self, ctx):
    #     """Track COMPUTE statement targets."""
    #     pass

    # ... etc for all modifying statements
