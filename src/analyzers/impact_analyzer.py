"""Impact analyzer for COBOL programs.

This module combines data structure analysis, REDEFINES analysis,
and procedure analysis to produce the final output mapping each
SECTION/PARAGRAPH to modified variables and affected records.
"""

from typing import Dict, List, Optional, Set, Any
from dataclasses import dataclass, field
from datetime import datetime

from cobol_ast.nodes import CobolProgram, VariableModification
from .data_analyzer import DataStructureAnalyzer
from .redefines import RedefinesAnalyzer
from .procedure_analyzer import ProcedureAnalyzer


@dataclass
class VariableImpact:
    """Impact information for a single variable modification."""

    variable_name: str
    affected_records: List[str]
    modification_type: str
    line_number: int


@dataclass
class SectionParagraphImpact:
    """Impact information for a section or paragraph."""

    name: str
    is_section: bool
    variable_impacts: List[VariableImpact]
    all_affected_records: Set[str] = field(default_factory=set)

    def __post_init__(self):
        # Compute all affected records
        for impact in self.variable_impacts:
            self.all_affected_records.update(impact.affected_records)


class ImpactAnalyzer:
    """Analyzes the impact of variable modifications in COBOL programs.

    This analyzer combines:
    - Data structure analysis (variable to record mapping)
    - REDEFINES analysis (record sharing through REDEFINES)
    - Procedure analysis (where variables are modified)

    To produce a complete impact analysis showing which records
    are affected by modifications in each section/paragraph.
    """

    def __init__(self, program: CobolProgram):
        """Initialize the impact analyzer.

        Args:
            program: Parsed COBOL program AST
        """
        self.program = program
        self.data_analyzer = DataStructureAnalyzer(program)
        self.redefines_analyzer = RedefinesAnalyzer(program)
        self.procedure_analyzer = ProcedureAnalyzer(program)

        self._section_impacts: Dict[str, SectionParagraphImpact] = {}
        self._paragraph_impacts: Dict[str, SectionParagraphImpact] = {}
        self._analyzed = False

    def analyze(self) -> None:
        """Perform complete impact analysis."""
        if self._analyzed:
            return

        # Run component analyses
        self.data_analyzer.analyze()
        self.redefines_analyzer.analyze()
        self.procedure_analyzer.analyze()

        # Build impact information
        self._analyze_sections()
        self._analyze_paragraphs()

        self._analyzed = True

    def _analyze_sections(self) -> None:
        """Analyze impact for all sections."""
        for section in self.program.sections:
            impacts = self._analyze_modifications(section.all_modifications)
            self._section_impacts[section.name] = SectionParagraphImpact(
                name=section.name,
                is_section=True,
                variable_impacts=impacts,
            )

    def _analyze_paragraphs(self) -> None:
        """Analyze impact for all paragraphs."""
        # Top-level paragraphs
        for paragraph in self.program.paragraphs:
            impacts = self._analyze_modifications(paragraph.modifications)
            self._paragraph_impacts[paragraph.name] = SectionParagraphImpact(
                name=paragraph.name,
                is_section=False,
                variable_impacts=impacts,
            )

        # Paragraphs within sections
        for section in self.program.sections:
            for paragraph in section.paragraphs:
                impacts = self._analyze_modifications(paragraph.modifications)
                self._paragraph_impacts[paragraph.name] = SectionParagraphImpact(
                    name=paragraph.name,
                    is_section=False,
                    variable_impacts=impacts,
                )

    def _analyze_modifications(
        self, modifications: List[VariableModification]
    ) -> List[VariableImpact]:
        """Analyze a list of modifications to determine impact.

        Args:
            modifications: List of variable modifications

        Returns:
            List of VariableImpact objects
        """
        impacts = []

        for mod in modifications:
            # Get the record containing this variable
            record = self.data_analyzer.get_record_for_variable(mod.variable_name)

            if record:
                # Get all affected records through REDEFINES
                affected = self.redefines_analyzer.get_affected_records(record.name)
                affected_list = sorted(list(affected))
            else:
                # Variable not found in data division (might be external or typo)
                affected_list = []

            impact = VariableImpact(
                variable_name=mod.variable_name,
                affected_records=affected_list,
                modification_type=mod.modification_type.name,
                line_number=mod.line_number,
            )
            impacts.append(impact)

        return impacts

    def get_section_impact(self, section_name: str) -> Optional[SectionParagraphImpact]:
        """Get impact information for a section.

        Args:
            section_name: Name of the section

        Returns:
            SectionParagraphImpact if found, None otherwise
        """
        if not self._analyzed:
            self.analyze()

        return self._section_impacts.get(section_name.upper())

    def get_paragraph_impact(self, paragraph_name: str) -> Optional[SectionParagraphImpact]:
        """Get impact information for a paragraph.

        Args:
            paragraph_name: Name of the paragraph

        Returns:
            SectionParagraphImpact if found, None otherwise
        """
        if not self._analyzed:
            self.analyze()

        return self._paragraph_impacts.get(paragraph_name.upper())

    def get_records_affected_by_section(self, section_name: str) -> Set[str]:
        """Get all records affected by modifications in a section.

        Args:
            section_name: Name of the section

        Returns:
            Set of record names
        """
        impact = self.get_section_impact(section_name)
        if impact:
            return impact.all_affected_records
        return set()

    def get_records_affected_by_paragraph(self, paragraph_name: str) -> Set[str]:
        """Get all records affected by modifications in a paragraph.

        Args:
            paragraph_name: Name of the paragraph

        Returns:
            Set of record names
        """
        impact = self.get_paragraph_impact(paragraph_name)
        if impact:
            return impact.all_affected_records
        return set()

    def get_sections_affecting_record(self, record_name: str) -> List[str]:
        """Get all sections that affect a record.

        Args:
            record_name: Name of the record

        Returns:
            List of section names
        """
        if not self._analyzed:
            self.analyze()

        record_upper = record_name.upper()
        sections = []

        for section_name, impact in self._section_impacts.items():
            if record_upper in impact.all_affected_records:
                sections.append(section_name)

        return sections

    def get_paragraphs_affecting_record(self, record_name: str) -> List[str]:
        """Get all paragraphs that affect a record.

        Args:
            record_name: Name of the record

        Returns:
            List of paragraph names
        """
        if not self._analyzed:
            self.analyze()

        record_upper = record_name.upper()
        paragraphs = []

        for para_name, impact in self._paragraph_impacts.items():
            if record_upper in impact.all_affected_records:
                paragraphs.append(para_name)

        return paragraphs

    def generate_output(self) -> Dict[str, Any]:
        """Generate the final output dictionary.

        Returns:
            Dictionary containing complete impact analysis
        """
        if not self._analyzed:
            self.analyze()

        sections_and_paragraphs = {}

        # Add sections
        for section_name, impact in self._section_impacts.items():
            if impact.variable_impacts:
                sections_and_paragraphs[section_name] = [
                    {
                        "variable": vi.variable_name,
                        "affected_records": vi.affected_records,
                        "modification_type": vi.modification_type,
                        "line_number": vi.line_number,
                    }
                    for vi in impact.variable_impacts
                ]

        # Add paragraphs
        for para_name, impact in self._paragraph_impacts.items():
            if impact.variable_impacts:
                sections_and_paragraphs[para_name] = [
                    {
                        "variable": vi.variable_name,
                        "affected_records": vi.affected_records,
                        "modification_type": vi.modification_type,
                        "line_number": vi.line_number,
                    }
                    for vi in impact.variable_impacts
                ]

        # Get records with REDEFINES
        redefines_summary = self.redefines_analyzer.get_analysis_summary()
        records_with_redefines = redefines_summary.get("records_with_redefines", [])

        # Build summary
        summary = {
            "total_sections": len(self.program.sections),
            "total_paragraphs": len(self.program.paragraphs) + sum(
                len(s.paragraphs) for s in self.program.sections
            ),
            "total_modifications": len(self.program.get_all_modifications()),
            "unique_modified_variables": len(
                self.procedure_analyzer.get_modified_variables()
            ),
            "records_with_redefines": records_with_redefines,
        }

        return {
            "program_name": self.program.name,
            "analysis_date": datetime.now().isoformat(),
            "sections_and_paragraphs": sections_and_paragraphs,
            "summary": summary,
        }

    def generate_compact_output(self) -> Dict[str, List[Dict]]:
        """Generate a compact output with unique variable/records per section.

        Returns:
            Dictionary mapping section/paragraph names to unique modifications
        """
        if not self._analyzed:
            self.analyze()

        result = {}

        # Process sections and paragraphs
        for name, impact in {**self._section_impacts, **self._paragraph_impacts}.items():
            if not impact.variable_impacts:
                continue

            # Deduplicate by variable name, keeping all affected records
            var_to_records: Dict[str, Set[str]] = {}
            for vi in impact.variable_impacts:
                if vi.variable_name not in var_to_records:
                    var_to_records[vi.variable_name] = set()
                var_to_records[vi.variable_name].update(vi.affected_records)

            result[name] = [
                {
                    "variable": var_name,
                    "affected_records": sorted(list(records)),
                }
                for var_name, records in var_to_records.items()
            ]

        return result

    def get_analysis_summary(self) -> Dict:
        """Get a comprehensive analysis summary.

        Returns:
            Dictionary with analysis statistics
        """
        if not self._analyzed:
            self.analyze()

        data_summary = self.data_analyzer.get_analysis_summary()
        redefines_summary = self.redefines_analyzer.get_analysis_summary()
        procedure_summary = self.procedure_analyzer.get_analysis_summary()

        return {
            "data_division": data_summary,
            "redefines_analysis": redefines_summary,
            "procedure_division": procedure_summary,
            "impact_analysis": {
                "sections_analyzed": len(self._section_impacts),
                "paragraphs_analyzed": len(self._paragraph_impacts),
                "sections_with_impact": sum(
                    1 for i in self._section_impacts.values() if i.variable_impacts
                ),
                "paragraphs_with_impact": sum(
                    1 for i in self._paragraph_impacts.values() if i.variable_impacts
                ),
            },
        }
