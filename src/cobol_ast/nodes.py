"""AST node definitions for COBOL programs.

This module defines the domain-specific AST nodes used to represent
parsed COBOL programs in a clean, analyzable form.
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Dict, List, Optional, Set


class ModificationType(Enum):
    """Types of variable modifications in COBOL."""

    # Data movement
    MOVE = auto()
    MOVE_CORRESPONDING = auto()

    # Arithmetic
    COMPUTE = auto()
    ADD = auto()
    SUBTRACT = auto()
    MULTIPLY = auto()
    DIVIDE = auto()

    # String operations
    STRING = auto()
    UNSTRING = auto()
    INSPECT = auto()

    # Input operations
    ACCEPT = auto()
    READ = auto()
    RETURN = auto()

    # Initialization
    INITIALIZE = auto()
    SET = auto()

    # Table operations
    SEARCH = auto()

    # Other
    CALL_BY_REFERENCE = auto()
    UNKNOWN = auto()

    @classmethod
    def from_string(cls, name: str) -> "ModificationType":
        """Convert string name to ModificationType."""
        name_upper = name.upper().replace("-", "_").replace(" ", "_")
        try:
            return cls[name_upper]
        except KeyError:
            return cls.UNKNOWN


@dataclass
class DataItem:
    """Represents a data item in DATA DIVISION.

    COBOL data items form a hierarchy based on level numbers:
    - Level 01: Record description (top-level)
    - Levels 02-49: Subordinate items (group or elementary)
    - Level 66: RENAMES clause
    - Level 77: Independent elementary items
    - Level 88: Condition names
    """

    name: str
    level: int
    parent: Optional["DataItem"] = None
    children: List["DataItem"] = field(default_factory=list)
    redefines: Optional[str] = None
    picture: Optional[str] = None
    usage: Optional[str] = None
    value: Optional[str] = None
    occurs: Optional[int] = None
    occurs_depending_on: Optional[str] = None
    line_number: int = 0

    @property
    def is_group(self) -> bool:
        """Check if this is a group item (has children)."""
        return len(self.children) > 0 or (self.picture is None and self.level not in (66, 77, 88))

    @property
    def is_elementary(self) -> bool:
        """Check if this is an elementary item."""
        return not self.is_group

    @property
    def is_record(self) -> bool:
        """Check if this is a Level 01 record."""
        return self.level == 1

    @property
    def root_record(self) -> "DataItem":
        """Get the Level 01 ancestor record.

        Returns:
            The Level 01 DataItem that this item belongs to
        """
        if self.level == 1 or self.level == 77:
            return self

        current = self
        while current.parent is not None:
            current = current.parent
            if current.level == 1:
                return current

        return current

    @property
    def qualified_name(self) -> str:
        """Get the fully qualified name (name OF parent OF grandparent...)."""
        parts = [self.name]
        current = self.parent
        while current is not None:
            parts.append(current.name)
            current = current.parent
        return " OF ".join(parts)

    def get_all_subordinates(self) -> List["DataItem"]:
        """Get all subordinate items recursively."""
        result = []
        for child in self.children:
            result.append(child)
            result.extend(child.get_all_subordinates())
        return result

    def find_child(self, name: str) -> Optional["DataItem"]:
        """Find a direct child by name."""
        name_upper = name.upper()
        for child in self.children:
            if child.name.upper() == name_upper:
                return child
        return None

    def find_descendant(self, name: str) -> Optional["DataItem"]:
        """Find a descendant at any level by name."""
        name_upper = name.upper()
        for item in self.get_all_subordinates():
            if item.name.upper() == name_upper:
                return item
        return None

    def __hash__(self):
        return hash((self.name, self.level, self.line_number))

    def __eq__(self, other):
        if not isinstance(other, DataItem):
            return False
        return (
            self.name == other.name
            and self.level == other.level
            and self.line_number == other.line_number
        )


@dataclass
class RecordDescription:
    """Level 01 record description with all subordinate items.

    A record description represents a complete data structure starting
    at level 01, containing all its subordinate fields.
    """

    name: str
    root_item: DataItem
    redefines: Optional[str] = None
    section: str = "WORKING-STORAGE"  # WORKING-STORAGE, FILE, LINKAGE, etc.

    @property
    def items(self) -> Dict[str, DataItem]:
        """Get all items in this record as a dictionary."""
        result = {self.name: self.root_item}
        for item in self.root_item.get_all_subordinates():
            result[item.name] = item
        return result

    def get_item(self, name: str) -> Optional[DataItem]:
        """Get an item by name."""
        name_upper = name.upper()
        if self.name.upper() == name_upper:
            return self.root_item
        return self.root_item.find_descendant(name)


@dataclass
class VariableModification:
    """Records a variable modification in procedure code."""

    variable_name: str
    modification_type: ModificationType
    line_number: int
    statement_text: str = ""
    section_name: Optional[str] = None
    paragraph_name: Optional[str] = None

    def __hash__(self):
        return hash((self.variable_name, self.modification_type, self.line_number))


@dataclass
class Paragraph:
    """Represents a COBOL paragraph.

    A paragraph is a named group of statements that can be
    performed via PERFORM.
    """

    name: str
    modifications: List[VariableModification] = field(default_factory=list)
    line_number: int = 0
    parent_section: Optional[str] = None

    @property
    def modified_variables(self) -> Set[str]:
        """Get set of all variables modified in this paragraph."""
        return {mod.variable_name for mod in self.modifications}


@dataclass
class Section:
    """Represents a COBOL section.

    A section contains zero or more paragraphs and may have
    standalone statements.
    """

    name: str
    paragraphs: List[Paragraph] = field(default_factory=list)
    standalone_modifications: List[VariableModification] = field(default_factory=list)
    line_number: int = 0

    @property
    def all_modifications(self) -> List[VariableModification]:
        """Get all modifications in this section and its paragraphs."""
        result = list(self.standalone_modifications)
        for para in self.paragraphs:
            result.extend(para.modifications)
        return result

    @property
    def modified_variables(self) -> Set[str]:
        """Get set of all variables modified in this section."""
        return {mod.variable_name for mod in self.all_modifications}

    def get_paragraph(self, name: str) -> Optional[Paragraph]:
        """Get a paragraph by name."""
        name_upper = name.upper()
        for para in self.paragraphs:
            if para.name.upper() == name_upper:
                return para
        return None


@dataclass
class CobolProgram:
    """Complete parsed COBOL program.

    This is the top-level AST node containing all parsed
    information about a COBOL program.
    """

    name: str
    record_descriptions: Dict[str, RecordDescription] = field(default_factory=dict)
    all_data_items: Dict[str, DataItem] = field(default_factory=dict)
    sections: List[Section] = field(default_factory=list)
    paragraphs: List[Paragraph] = field(default_factory=list)  # Top-level paragraphs (no section)
    source_lines: List[str] = field(default_factory=list)

    def get_record_for_variable(self, variable_name: str) -> Optional[RecordDescription]:
        """Get the record description containing a variable.

        Args:
            variable_name: Name of the variable to look up

        Returns:
            RecordDescription containing the variable, or None
        """
        var_upper = variable_name.upper()

        # Check if it's a record itself
        if var_upper in self.record_descriptions:
            return self.record_descriptions[var_upper]

        # Look up in all_data_items
        if var_upper in self.all_data_items:
            item = self.all_data_items[var_upper]
            root = item.root_record
            if root.name.upper() in self.record_descriptions:
                return self.record_descriptions[root.name.upper()]

        return None

    def get_section(self, name: str) -> Optional[Section]:
        """Get a section by name."""
        name_upper = name.upper()
        for section in self.sections:
            if section.name.upper() == name_upper:
                return section
        return None

    def get_paragraph(self, name: str, section_name: Optional[str] = None) -> Optional[Paragraph]:
        """Get a paragraph by name, optionally within a section.

        Args:
            name: Paragraph name
            section_name: Optional section to search in

        Returns:
            Paragraph if found, None otherwise
        """
        name_upper = name.upper()

        if section_name:
            section = self.get_section(section_name)
            if section:
                return section.get_paragraph(name)
            return None

        # Search all sections and top-level paragraphs
        for section in self.sections:
            para = section.get_paragraph(name)
            if para:
                return para

        for para in self.paragraphs:
            if para.name.upper() == name_upper:
                return para

        return None

    def get_all_modifications(self) -> List[VariableModification]:
        """Get all variable modifications in the program."""
        result = []

        for section in self.sections:
            result.extend(section.all_modifications)

        for para in self.paragraphs:
            result.extend(para.modifications)

        return result

    def get_modifications_by_variable(self) -> Dict[str, List[VariableModification]]:
        """Get modifications grouped by variable name."""
        result: Dict[str, List[VariableModification]] = {}

        for mod in self.get_all_modifications():
            if mod.variable_name not in result:
                result[mod.variable_name] = []
            result[mod.variable_name].append(mod)

        return result
