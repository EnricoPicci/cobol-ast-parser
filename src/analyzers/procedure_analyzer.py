"""Procedure division analyzer for COBOL programs.

This module analyzes the PROCEDURE DIVISION to extract
variable modifications from sections and paragraphs.
"""

from typing import Dict, List, Optional, Set, Tuple
from dataclasses import dataclass, field
from collections import defaultdict

from cobol_ast.nodes import (
    CobolProgram,
    Section,
    Paragraph,
    VariableModification,
    ModificationType,
)


@dataclass
class ModificationSummary:
    """Summary of modifications for a section or paragraph."""

    name: str
    is_section: bool
    modifications: List[VariableModification]
    modified_variables: Set[str]
    modification_counts: Dict[str, int]  # variable -> count


class ProcedureAnalyzer:
    """Analyzes COBOL procedure division for variable modifications.

    This analyzer extracts all variable modifications from sections
    and paragraphs, tracking:
    - Which variables are modified
    - What type of modification (MOVE, COMPUTE, etc.)
    - Where the modification occurs (section/paragraph, line number)
    """

    def __init__(self, program: CobolProgram):
        """Initialize the analyzer.

        Args:
            program: Parsed COBOL program AST
        """
        self.program = program
        self._section_summaries: Dict[str, ModificationSummary] = {}
        self._paragraph_summaries: Dict[str, ModificationSummary] = {}
        self._variable_modifications: Dict[str, List[VariableModification]] = {}
        self._modification_type_counts: Dict[ModificationType, int] = {}
        self._analyzed = False

    def analyze(self) -> None:
        """Perform procedure division analysis."""
        if self._analyzed:
            return

        self._analyze_sections()
        self._analyze_paragraphs()
        self._build_variable_index()
        self._count_modification_types()
        self._analyzed = True

    def _analyze_sections(self) -> None:
        """Analyze all sections in the procedure division."""
        for section in self.program.sections:
            summary = self._create_summary(
                name=section.name,
                is_section=True,
                modifications=section.all_modifications,
            )
            self._section_summaries[section.name] = summary

    def _analyze_paragraphs(self) -> None:
        """Analyze all paragraphs (including those in sections)."""
        # Top-level paragraphs
        for paragraph in self.program.paragraphs:
            summary = self._create_summary(
                name=paragraph.name,
                is_section=False,
                modifications=paragraph.modifications,
            )
            self._paragraph_summaries[paragraph.name] = summary

        # Paragraphs within sections
        for section in self.program.sections:
            for paragraph in section.paragraphs:
                # Use qualified name to avoid conflicts
                qualified_name = f"{section.name}.{paragraph.name}"
                summary = self._create_summary(
                    name=paragraph.name,
                    is_section=False,
                    modifications=paragraph.modifications,
                )
                self._paragraph_summaries[qualified_name] = summary

                # Also store by simple name if unique
                if paragraph.name not in self._paragraph_summaries:
                    self._paragraph_summaries[paragraph.name] = summary

    def _create_summary(
        self,
        name: str,
        is_section: bool,
        modifications: List[VariableModification],
    ) -> ModificationSummary:
        """Create a modification summary."""
        modified_vars = {m.variable_name for m in modifications}
        counts: Dict[str, int] = defaultdict(int)
        for mod in modifications:
            counts[mod.variable_name] += 1

        return ModificationSummary(
            name=name,
            is_section=is_section,
            modifications=modifications,
            modified_variables=modified_vars,
            modification_counts=dict(counts),
        )

    def _build_variable_index(self) -> None:
        """Build index of modifications by variable name."""
        self._variable_modifications = defaultdict(list)

        for mod in self.program.get_all_modifications():
            self._variable_modifications[mod.variable_name].append(mod)

    def _count_modification_types(self) -> None:
        """Count modifications by type."""
        self._modification_type_counts = defaultdict(int)

        for mod in self.program.get_all_modifications():
            self._modification_type_counts[mod.modification_type] += 1

    def get_section_summary(self, section_name: str) -> Optional[ModificationSummary]:
        """Get modification summary for a section.

        Args:
            section_name: Name of the section

        Returns:
            ModificationSummary if found, None otherwise
        """
        if not self._analyzed:
            self.analyze()

        return self._section_summaries.get(section_name.upper())

    def get_paragraph_summary(
        self, paragraph_name: str, section_name: Optional[str] = None
    ) -> Optional[ModificationSummary]:
        """Get modification summary for a paragraph.

        Args:
            paragraph_name: Name of the paragraph
            section_name: Optional section name for qualified lookup

        Returns:
            ModificationSummary if found, None otherwise
        """
        if not self._analyzed:
            self.analyze()

        para_upper = paragraph_name.upper()

        if section_name:
            qualified = f"{section_name.upper()}.{para_upper}"
            if qualified in self._paragraph_summaries:
                return self._paragraph_summaries[qualified]

        return self._paragraph_summaries.get(para_upper)

    def get_modifications_for_variable(
        self, variable_name: str
    ) -> List[VariableModification]:
        """Get all modifications for a variable.

        Args:
            variable_name: Name of the variable

        Returns:
            List of VariableModification objects
        """
        if not self._analyzed:
            self.analyze()

        return self._variable_modifications.get(variable_name.upper(), [])

    def get_modified_variables(self) -> Set[str]:
        """Get all variables that are modified in the program.

        Returns:
            Set of variable names
        """
        if not self._analyzed:
            self.analyze()

        return set(self._variable_modifications.keys())

    def get_sections_modifying_variable(self, variable_name: str) -> List[str]:
        """Get all sections that modify a variable.

        Args:
            variable_name: Name of the variable

        Returns:
            List of section names
        """
        if not self._analyzed:
            self.analyze()

        var_upper = variable_name.upper()
        sections = []

        for section_name, summary in self._section_summaries.items():
            if var_upper in summary.modified_variables:
                sections.append(section_name)

        return sections

    def get_paragraphs_modifying_variable(self, variable_name: str) -> List[str]:
        """Get all paragraphs that modify a variable.

        Args:
            variable_name: Name of the variable

        Returns:
            List of paragraph names
        """
        if not self._analyzed:
            self.analyze()

        var_upper = variable_name.upper()
        paragraphs = []

        for para_name, summary in self._paragraph_summaries.items():
            if var_upper in summary.modified_variables:
                # Avoid duplicates from qualified names
                if "." not in para_name:
                    paragraphs.append(para_name)

        return paragraphs

    def get_modification_types_for_variable(
        self, variable_name: str
    ) -> Set[ModificationType]:
        """Get all modification types used for a variable.

        Args:
            variable_name: Name of the variable

        Returns:
            Set of ModificationType values
        """
        mods = self.get_modifications_for_variable(variable_name)
        return {m.modification_type for m in mods}

    def get_all_sections_with_modifications(self) -> List[str]:
        """Get all sections that contain modifications.

        Returns:
            List of section names
        """
        if not self._analyzed:
            self.analyze()

        return [
            name
            for name, summary in self._section_summaries.items()
            if summary.modifications
        ]

    def get_all_paragraphs_with_modifications(self) -> List[str]:
        """Get all paragraphs that contain modifications.

        Returns:
            List of paragraph names
        """
        if not self._analyzed:
            self.analyze()

        return [
            name
            for name, summary in self._paragraph_summaries.items()
            if summary.modifications and "." not in name
        ]

    def get_most_modified_variables(self, limit: int = 10) -> List[Tuple[str, int]]:
        """Get the most frequently modified variables.

        Args:
            limit: Maximum number of results

        Returns:
            List of (variable_name, modification_count) tuples
        """
        if not self._analyzed:
            self.analyze()

        counts = [
            (name, len(mods))
            for name, mods in self._variable_modifications.items()
        ]
        counts.sort(key=lambda x: x[1], reverse=True)
        return counts[:limit]

    def get_analysis_summary(self) -> Dict:
        """Get a summary of the procedure analysis.

        Returns:
            Dictionary with analysis statistics
        """
        if not self._analyzed:
            self.analyze()

        return {
            "total_sections": len(self.program.sections),
            "total_paragraphs": len(self.program.paragraphs) + sum(
                len(s.paragraphs) for s in self.program.sections
            ),
            "sections_with_modifications": len(
                self.get_all_sections_with_modifications()
            ),
            "paragraphs_with_modifications": len(
                self.get_all_paragraphs_with_modifications()
            ),
            "total_modifications": len(self.program.get_all_modifications()),
            "unique_modified_variables": len(self._variable_modifications),
            "modification_type_counts": {
                mt.name: count
                for mt, count in self._modification_type_counts.items()
            },
        }
