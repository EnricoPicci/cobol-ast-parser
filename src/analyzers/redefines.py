"""REDEFINES relationship analyzer for COBOL programs.

This module analyzes REDEFINES clauses to build a graph of
memory-sharing relationships between records. When a variable
in one record is modified, all records that REDEFINE or are
REDEFINED by that record are potentially affected.
"""

from typing import Dict, List, Optional, Set, Tuple
from dataclasses import dataclass, field

try:
    import networkx as nx
    NETWORKX_AVAILABLE = True
except ImportError:
    NETWORKX_AVAILABLE = False

from cobol_ast.nodes import CobolProgram, DataItem, RecordDescription


@dataclass
class RedefinesRelation:
    """Represents a REDEFINES relationship."""

    redefining_item: str  # The item with REDEFINES clause
    redefined_item: str   # The item being redefined
    level: int            # Level number (01 for record-level)


class RedefinesAnalyzer:
    """Analyzes REDEFINES relationships in COBOL programs.

    Uses graph analysis to find connected components of records
    that share memory through REDEFINES chains.
    """

    def __init__(self, program: CobolProgram):
        """Initialize the analyzer.

        Args:
            program: Parsed COBOL program AST
        """
        self.program = program
        self._relations: List[RedefinesRelation] = []
        self._record_graph: Optional[object] = None  # networkx.Graph if available
        self._variable_to_records: Dict[str, Set[str]] = {}
        self._connected_components: List[Set[str]] = []
        self._analyzed = False

    def analyze(self) -> None:
        """Perform REDEFINES analysis."""
        if self._analyzed:
            return

        self._extract_redefines_relations()
        self._build_graph()
        self._find_connected_components()
        self._build_variable_to_records_mapping()
        self._analyzed = True

    def _extract_redefines_relations(self) -> None:
        """Extract all REDEFINES relationships from the program."""
        self._relations = []

        for item_name, item in self.program.all_data_items.items():
            if item.redefines:
                relation = RedefinesRelation(
                    redefining_item=item_name,
                    redefined_item=item.redefines.upper(),
                    level=item.level,
                )
                self._relations.append(relation)

    def _build_graph(self) -> None:
        """Build the REDEFINES relationship graph."""
        if NETWORKX_AVAILABLE:
            self._record_graph = nx.Graph()

            # Add all Level 01 records as nodes
            for record_name in self.program.record_descriptions:
                self._record_graph.add_node(record_name)

            # Add edges for REDEFINES relationships
            for relation in self._relations:
                if relation.level == 1:
                    # Record-level REDEFINES
                    self._record_graph.add_edge(
                        relation.redefining_item,
                        relation.redefined_item,
                    )
                else:
                    # Subordinate REDEFINES - find parent records
                    redefining_record = self._get_parent_record(relation.redefining_item)
                    redefined_record = self._get_parent_record(relation.redefined_item)

                    if redefining_record and redefined_record:
                        if redefining_record != redefined_record:
                            self._record_graph.add_edge(
                                redefining_record,
                                redefined_record,
                            )
        else:
            # Fallback without networkx
            self._record_graph = None

    def _get_parent_record(self, item_name: str) -> Optional[str]:
        """Get the Level 01 record containing an item."""
        item = self.program.all_data_items.get(item_name.upper())
        if item:
            root = item.root_record
            return root.name
        return None

    def _find_connected_components(self) -> None:
        """Find connected components in the REDEFINES graph."""
        self._connected_components = []

        if NETWORKX_AVAILABLE and self._record_graph:
            for component in nx.connected_components(self._record_graph):
                self._connected_components.append(component)
        else:
            # Fallback: use union-find algorithm
            self._connected_components = self._find_components_union_find()

    def _find_components_union_find(self) -> List[Set[str]]:
        """Find connected components using union-find (without networkx)."""
        # Build adjacency list
        adjacency: Dict[str, Set[str]] = {}

        for record_name in self.program.record_descriptions:
            adjacency[record_name] = set()

        for relation in self._relations:
            if relation.level == 1:
                adjacency.setdefault(relation.redefining_item, set()).add(
                    relation.redefined_item
                )
                adjacency.setdefault(relation.redefined_item, set()).add(
                    relation.redefining_item
                )

        # Find components using BFS
        visited: Set[str] = set()
        components: List[Set[str]] = []

        for record in adjacency:
            if record in visited:
                continue

            # BFS to find all connected records
            component: Set[str] = set()
            queue = [record]

            while queue:
                current = queue.pop(0)
                if current in visited:
                    continue

                visited.add(current)
                component.add(current)

                for neighbor in adjacency.get(current, set()):
                    if neighbor not in visited:
                        queue.append(neighbor)

            if component:
                components.append(component)

        return components

    def _build_variable_to_records_mapping(self) -> None:
        """Build mapping from variables to all affected records."""
        self._variable_to_records = {}

        for item_name, item in self.program.all_data_items.items():
            # Get the record containing this variable
            root = item.root_record
            record_name = root.name

            # Find all records in the same connected component
            affected_records = self._find_affected_records_internal(record_name)

            self._variable_to_records[item_name] = affected_records

    def _find_affected_records_internal(self, record_name: str) -> Set[str]:
        """Internal method to find affected records without triggering analyze().

        Args:
            record_name: Name of the record

        Returns:
            Set of affected record names
        """
        record_upper = record_name.upper()

        # Find the connected component containing this record
        for component in self._connected_components:
            if record_upper in component:
                return component

        # Not in any component (no REDEFINES)
        return {record_upper}

    def get_affected_records(self, variable_name: str) -> Set[str]:
        """Get all Level 01 records affected when a variable is modified.

        This includes:
        - The record containing the variable
        - All records connected via REDEFINES chains

        Args:
            variable_name: Name of the variable being modified

        Returns:
            Set of affected record names
        """
        if not self._analyzed:
            self.analyze()

        var_upper = variable_name.upper()

        # Check if we already computed this
        if var_upper in self._variable_to_records:
            return self._variable_to_records[var_upper]

        # Find the record containing this variable
        item = self.program.all_data_items.get(var_upper)
        if not item:
            return set()

        root = item.root_record
        record_name = root.name

        # Find the connected component containing this record
        for component in self._connected_components:
            if record_name in component:
                return component

        # Not in any component (no REDEFINES)
        return {record_name}

    def get_redefines_chain(self, record_name: str) -> List[str]:
        """Get the chain of records that REDEFINE each other.

        Args:
            record_name: Starting record name

        Returns:
            List of record names in the REDEFINES chain
        """
        if not self._analyzed:
            self.analyze()

        record_upper = record_name.upper()

        for component in self._connected_components:
            if record_upper in component:
                return sorted(list(component))

        return [record_upper]

    def get_direct_redefines(self, item_name: str) -> Optional[str]:
        """Get the item directly redefined by this item.

        Args:
            item_name: Name of the item with REDEFINES

        Returns:
            Name of the redefined item, or None
        """
        item = self.program.all_data_items.get(item_name.upper())
        if item:
            return item.redefines
        return None

    def get_items_redefining(self, item_name: str) -> List[str]:
        """Get all items that REDEFINE this item.

        Args:
            item_name: Name of the item being redefined

        Returns:
            List of item names that REDEFINE this item
        """
        item_upper = item_name.upper()
        return [
            relation.redefining_item
            for relation in self._relations
            if relation.redefined_item == item_upper
        ]

    def has_redefines(self, item_name: str) -> bool:
        """Check if an item has a REDEFINES clause.

        Args:
            item_name: Name of the item

        Returns:
            True if item has REDEFINES
        """
        item = self.program.all_data_items.get(item_name.upper())
        return item is not None and item.redefines is not None

    def is_redefined(self, item_name: str) -> bool:
        """Check if an item is redefined by another item.

        Args:
            item_name: Name of the item

        Returns:
            True if item is redefined by another
        """
        item_upper = item_name.upper()
        return any(
            relation.redefined_item == item_upper
            for relation in self._relations
        )

    def get_all_relations(self) -> List[RedefinesRelation]:
        """Get all REDEFINES relationships.

        Returns:
            List of RedefinesRelation objects
        """
        if not self._analyzed:
            self.analyze()

        return self._relations.copy()

    def get_analysis_summary(self) -> Dict:
        """Get a summary of the REDEFINES analysis.

        Returns:
            Dictionary with analysis statistics
        """
        if not self._analyzed:
            self.analyze()

        records_with_redefines = set()
        for relation in self._relations:
            if relation.level == 1:
                records_with_redefines.add(relation.redefining_item)
                records_with_redefines.add(relation.redefined_item)

        multi_record_components = [
            c for c in self._connected_components if len(c) > 1
        ]

        return {
            "total_redefines_relations": len(self._relations),
            "record_level_redefines": sum(
                1 for r in self._relations if r.level == 1
            ),
            "subordinate_redefines": sum(
                1 for r in self._relations if r.level != 1
            ),
            "records_with_redefines": list(records_with_redefines),
            "connected_components": len(self._connected_components),
            "multi_record_components": len(multi_record_components),
            "largest_component_size": max(
                (len(c) for c in self._connected_components), default=0
            ),
        }
