"""Data structure analyzer for COBOL programs.

This module analyzes the DATA DIVISION to:
- Build variable to Level 01 record mappings
- Track data item hierarchies
- Identify data relationships
"""

from typing import Dict, List, Optional, Set, Tuple
from cobol_ast.nodes import CobolProgram, DataItem, RecordDescription


class DataStructureAnalyzer:
    """Analyzes COBOL data structures.

    This analyzer processes the DATA DIVISION to build comprehensive
    mappings between variables and their containing records.
    """

    def __init__(self, program: CobolProgram):
        """Initialize the analyzer.

        Args:
            program: Parsed COBOL program AST
        """
        self.program = program
        self._variable_to_record: Dict[str, RecordDescription] = {}
        self._record_to_variables: Dict[str, Set[str]] = {}
        self._group_items: Dict[str, DataItem] = {}
        self._elementary_items: Dict[str, DataItem] = {}
        self._analyzed = False

    def analyze(self) -> None:
        """Perform the data structure analysis."""
        if self._analyzed:
            return

        self._build_variable_mappings()
        self._categorize_items()
        self._analyzed = True

    def _build_variable_mappings(self) -> None:
        """Build mappings between variables and records."""
        for record_name, record in self.program.record_descriptions.items():
            # Map record to itself
            self._variable_to_record[record_name] = record

            # Initialize the set of variables for this record
            if record_name not in self._record_to_variables:
                self._record_to_variables[record_name] = set()

            self._record_to_variables[record_name].add(record_name)

            # Map all subordinate items
            for item_name, item in record.items.items():
                self._variable_to_record[item_name] = record
                self._record_to_variables[record_name].add(item_name)

    def _categorize_items(self) -> None:
        """Categorize data items as group or elementary."""
        for item_name, item in self.program.all_data_items.items():
            if item.is_group:
                self._group_items[item_name] = item
            else:
                self._elementary_items[item_name] = item

    def get_record_for_variable(self, variable_name: str) -> Optional[RecordDescription]:
        """Get the Level 01 record containing a variable.

        Args:
            variable_name: Name of the variable

        Returns:
            RecordDescription if found, None otherwise
        """
        if not self._analyzed:
            self.analyze()

        return self._variable_to_record.get(variable_name.upper())

    def get_variables_in_record(self, record_name: str) -> Set[str]:
        """Get all variables in a record.

        Args:
            record_name: Name of the Level 01 record

        Returns:
            Set of variable names
        """
        if not self._analyzed:
            self.analyze()

        return self._record_to_variables.get(record_name.upper(), set())

    def get_group_items(self) -> Dict[str, DataItem]:
        """Get all group items (items with children).

        Returns:
            Dictionary of group item name to DataItem
        """
        if not self._analyzed:
            self.analyze()

        return self._group_items.copy()

    def get_elementary_items(self) -> Dict[str, DataItem]:
        """Get all elementary items (items without children).

        Returns:
            Dictionary of elementary item name to DataItem
        """
        if not self._analyzed:
            self.analyze()

        return self._elementary_items.copy()

    def get_item_hierarchy(self, item_name: str) -> List[DataItem]:
        """Get the hierarchy path from root to item.

        Args:
            item_name: Name of the item

        Returns:
            List of DataItems from root (Level 01) to the item
        """
        item = self.program.all_data_items.get(item_name.upper())
        if not item:
            return []

        hierarchy = []
        current = item
        while current is not None:
            hierarchy.insert(0, current)
            current = current.parent

        return hierarchy

    def get_siblings(self, item_name: str) -> List[DataItem]:
        """Get sibling items (same parent and level).

        Args:
            item_name: Name of the item

        Returns:
            List of sibling DataItems
        """
        item = self.program.all_data_items.get(item_name.upper())
        if not item or not item.parent:
            return []

        return [
            child
            for child in item.parent.children
            if child.name != item.name and child.level == item.level
        ]

    def find_items_by_pattern(self, pattern: str) -> List[DataItem]:
        """Find data items matching a pattern.

        Args:
            pattern: Pattern to match (supports * wildcard)

        Returns:
            List of matching DataItems
        """
        import fnmatch

        pattern_upper = pattern.upper()
        results = []

        for name, item in self.program.all_data_items.items():
            if fnmatch.fnmatch(name, pattern_upper):
                results.append(item)

        return results

    def get_items_at_level(self, level: int) -> List[DataItem]:
        """Get all items at a specific level.

        Args:
            level: COBOL level number (1-49, 66, 77, 88)

        Returns:
            List of DataItems at that level
        """
        return [
            item
            for item in self.program.all_data_items.values()
            if item.level == level
        ]

    def get_items_with_redefines(self) -> List[DataItem]:
        """Get all items that have REDEFINES clause.

        Returns:
            List of DataItems with REDEFINES
        """
        return [
            item
            for item in self.program.all_data_items.values()
            if item.redefines is not None
        ]

    def get_items_with_occurs(self) -> List[DataItem]:
        """Get all items that have OCCURS clause.

        Returns:
            List of DataItems with OCCURS
        """
        return [
            item
            for item in self.program.all_data_items.values()
            if item.occurs is not None
        ]

    def calculate_approximate_size(self, item: DataItem) -> int:
        """Calculate approximate byte size of a data item.

        This is an approximation based on PICTURE clause analysis.

        Args:
            item: DataItem to calculate size for

        Returns:
            Approximate size in bytes
        """
        if item.is_group:
            # Sum of children sizes
            total = 0
            for child in item.children:
                if child.level != 88:  # Skip condition names
                    child_size = self.calculate_approximate_size(child)
                    multiplier = child.occurs or 1
                    total += child_size * multiplier
            return total

        # Elementary item - parse PICTURE
        if not item.picture:
            return 0

        return self._parse_picture_size(item.picture)

    def _parse_picture_size(self, picture: str) -> int:
        """Parse PICTURE clause to estimate size.

        Args:
            picture: PICTURE clause string

        Returns:
            Estimated size in bytes
        """
        import re

        if not picture:
            return 0

        pic = picture.upper()
        size = 0

        # Expand repetition notation: X(10) -> XXXXXXXXXX
        expanded = re.sub(
            r"(\w)\((\d+)\)",
            lambda m: m.group(1) * int(m.group(2)),
            pic,
        )

        # Count characters
        for char in expanded:
            if char in "9XABZ*+-.,$CR":
                size += 1
            elif char in "SV":
                pass  # Sign and decimal don't take space in display

        return size

    def get_analysis_summary(self) -> Dict:
        """Get a summary of the data structure analysis.

        Returns:
            Dictionary with analysis statistics
        """
        if not self._analyzed:
            self.analyze()

        return {
            "total_records": len(self.program.record_descriptions),
            "total_data_items": len(self.program.all_data_items),
            "group_items": len(self._group_items),
            "elementary_items": len(self._elementary_items),
            "items_with_redefines": len(self.get_items_with_redefines()),
            "items_with_occurs": len(self.get_items_with_occurs()),
            "record_names": list(self.program.record_descriptions.keys()),
        }
