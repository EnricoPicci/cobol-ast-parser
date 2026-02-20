"""REDEFINES relationship analyzer for COBOL programs.

This module analyzes REDEFINES clauses to build a graph of
memory-sharing relationships between records. When a variable
in one record is modified, all records that REDEFINE or are
REDEFINED by that record are potentially affected.
"""

from collections import deque
from typing import Any, Dict, List, Optional, Set, Tuple, TYPE_CHECKING
from dataclasses import dataclass, field

if TYPE_CHECKING:
    from .data_analyzer import DataStructureAnalyzer

try:
    import networkx as nx  # type: ignore[import-untyped]
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


@dataclass
class AffectedVariable:
    """Information about a variable affected by REDEFINES."""

    name: str                    # Variable name
    overlap_type: str            # "full", "partial", "contains", "contained_by"
    redefines_chain: str         # Description of the REDEFINES relationship
    redefines_level: int         # Level at which REDEFINES occurs
    redefining_ancestor: str     # The ancestor item that has REDEFINES clause
    redefined_ancestor: str      # The ancestor item being redefined


class RedefinesAnalyzer:
    """Analyzes REDEFINES relationships in COBOL programs.

    Uses graph analysis to find connected components of records
    that share memory through REDEFINES chains.
    """

    def __init__(self, program: CobolProgram, data_analyzer: Optional["DataStructureAnalyzer"] = None):
        """Initialize the analyzer.

        Args:
            program: Parsed COBOL program AST
            data_analyzer: Optional DataStructureAnalyzer for memory calculations
        """
        self.program = program
        self._data_analyzer = data_analyzer
        self._relations: List[RedefinesRelation] = []
        self._record_graph: Optional[Any] = None  # networkx.Graph if available
        self._variable_to_records: Dict[str, Set[str]] = {}
        self._connected_components: List[Set[str]] = []
        # New: map of item -> items that redefine the same memory region
        self._subordinate_redefines_map: Dict[str, Set[str]] = {}
        self._overlapping_cache: Dict[str, List[AffectedVariable]] = {}
        self._analyzed = False

    def analyze(self) -> None:
        """Perform REDEFINES analysis."""
        if self._analyzed:
            return

        self._extract_redefines_relations()
        self._build_graph()
        self._find_connected_components()
        self._build_variable_to_records_mapping()
        self._build_subordinate_redefines_map()
        self._analyzed = True

    def set_data_analyzer(self, data_analyzer: "DataStructureAnalyzer") -> None:
        """Set the data analyzer for memory calculations.

        Args:
            data_analyzer: DataStructureAnalyzer instance
        """
        self._data_analyzer = data_analyzer
        # Rebuild subordinate map if already analyzed
        if self._analyzed:
            self._build_subordinate_redefines_map()

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
            queue = deque([record])

            while queue:
                current = queue.popleft()
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

    def _build_subordinate_redefines_map(self) -> None:
        """Build mapping of variables to their REDEFINES-related siblings.

        For each REDEFINES at subordinate levels, map the subordinates
        of the redefining item to subordinates of the redefined item.
        """
        self._subordinate_redefines_map = {}

        # Group REDEFINES relations by the redefined item
        redefined_to_redefining: Dict[str, List[str]] = {}
        for relation in self._relations:
            if relation.redefined_item not in redefined_to_redefining:
                redefined_to_redefining[relation.redefined_item] = []
            redefined_to_redefining[relation.redefined_item].append(relation.redefining_item)

        # For each group of items that REDEFINE each other:
        # 1. Collect subordinate name sets per group member (one traversal each)
        # 2. Cross-link: each subordinate maps to all subordinates of other members
        for redefined_item, redefining_items in redefined_to_redefining.items():
            all_in_group = [redefined_item] + redefining_items

            # Phase 1: Collect subordinate names per group member
            group_sub_names: List[Set[str]] = []
            for item_name in all_in_group:
                item = self.program.all_data_items.get(item_name.upper())
                if not item:
                    group_sub_names.append(set())
                    continue
                names = {sub.name.upper() for sub in item.get_all_subordinates()}
                names.add(item.name.upper())
                group_sub_names.append(names)

            # Phase 2: For each member's subordinates, add all other members' subordinates
            for i, sub_names in enumerate(group_sub_names):
                # Collect all subordinate names from other group members
                other_names: Set[str] = set()
                for j, other_sub_names in enumerate(group_sub_names):
                    if j != i:
                        other_names.update(other_sub_names)

                for sub_name in sub_names:
                    if sub_name not in self._subordinate_redefines_map:
                        self._subordinate_redefines_map[sub_name] = set()
                    # Add others, excluding self
                    self._subordinate_redefines_map[sub_name].update(
                        other_names - {sub_name}
                    )

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
            "records_with_redefines": sorted(records_with_redefines),
            "connected_components": len(self._connected_components),
            "multi_record_components": len(multi_record_components),
            "largest_component_size": max(
                (len(c) for c in self._connected_components), default=0
            ),
        }

    def get_ancestor_redefines(self, variable_name: str) -> List[RedefinesRelation]:
        """Get all REDEFINES relationships in the variable's ancestor chain.

        Walks up the hierarchy from the variable to Level 01 and collects
        all REDEFINES relationships encountered.

        Args:
            variable_name: Name of the variable

        Returns:
            List of RedefinesRelation for ancestors with REDEFINES clauses
        """
        if not self._analyzed:
            self.analyze()

        item = self.program.all_data_items.get(variable_name.upper())
        if not item:
            return []

        ancestor_redefines: List[RedefinesRelation] = []
        current: Optional[DataItem] = item

        while current is not None:
            if current.redefines:
                redefined = self.program.all_data_items.get(current.redefines.upper())
                if redefined:
                    ancestor_redefines.append(RedefinesRelation(
                        redefining_item=current.name.upper(),
                        redefined_item=current.redefines.upper(),
                        level=current.level,
                    ))
            current = current.parent

        return ancestor_redefines

    def get_overlapping_variables(self, variable_name: str) -> List[AffectedVariable]:
        """Get all variables that share memory with the given variable.

        This finds variables that overlap due to REDEFINES at any level
        in the hierarchy.

        Args:
            variable_name: Name of the variable

        Returns:
            List of AffectedVariable objects describing overlapping variables
        """
        if not self._analyzed:
            self.analyze()

        var_upper = variable_name.upper()

        if var_upper in self._overlapping_cache:
            return self._overlapping_cache[var_upper]

        item = self.program.all_data_items.get(var_upper)
        if not item:
            self._overlapping_cache[var_upper] = []
            return []

        result: List[AffectedVariable] = []
        seen_variables: Set[str] = set()

        # Method 1: Check if this variable is directly in a REDEFINES group
        if var_upper in self._subordinate_redefines_map:
            for other_name in self._subordinate_redefines_map[var_upper]:
                if other_name not in seen_variables:
                    seen_variables.add(other_name)
                    result.append(self._create_affected_variable(
                        var_upper, other_name, "direct_redefines_group"
                    ))

        # Method 2: Walk up ancestors and find REDEFINES, then find overlapping descendants
        ancestor_redefines = self.get_ancestor_redefines(variable_name)
        for relation in ancestor_redefines:
            # Get all items that REDEFINE the same item
            items_redefining_same = self.get_items_redefining(relation.redefined_item)
            # Include the redefined item itself
            all_related_items = [relation.redefined_item] + items_redefining_same

            for related_item_name in all_related_items:
                if related_item_name == relation.redefining_item:
                    continue  # Skip the item in our own ancestor chain

                related_item = self.program.all_data_items.get(related_item_name.upper())
                if not related_item:
                    continue

                # Get all subordinates of this related item
                subordinates = [related_item] + related_item.get_all_subordinates()
                for sub in subordinates:
                    sub_name = sub.name.upper()
                    if sub_name not in seen_variables and sub_name != var_upper:
                        seen_variables.add(sub_name)
                        result.append(AffectedVariable(
                            name=sub_name,
                            overlap_type="ancestor_redefines",
                            redefines_chain=f"{relation.redefining_item} REDEFINES {relation.redefined_item}",
                            redefines_level=relation.level,
                            redefining_ancestor=relation.redefining_item,
                            redefined_ancestor=relation.redefined_item,
                        ))

        self._overlapping_cache[var_upper] = result
        return result

    def _create_affected_variable(
        self, source_var: str, target_var: str, overlap_type: str
    ) -> AffectedVariable:
        """Create an AffectedVariable object for a given relationship.

        Args:
            source_var: The source variable name
            target_var: The target variable name
            overlap_type: Type of overlap

        Returns:
            AffectedVariable object
        """
        # Find the REDEFINES relationship that connects these variables
        source_item = self.program.all_data_items.get(source_var)
        target_item = self.program.all_data_items.get(target_var)

        if not source_item or not target_item:
            return AffectedVariable(
                name=target_var,
                overlap_type=overlap_type,
                redefines_chain="unknown",
                redefines_level=0,
                redefining_ancestor="",
                redefined_ancestor="",
            )

        # Walk up both hierarchies to find the REDEFINES connection
        source_ancestors = self._get_ancestor_chain(source_item)
        target_ancestors = self._get_ancestor_chain(target_item)

        # Find where the chains connect via REDEFINES
        for s_ancestor in source_ancestors:
            for t_ancestor in target_ancestors:
                # Check if one redefines the other
                if s_ancestor.redefines and s_ancestor.redefines.upper() == t_ancestor.name.upper():
                    return AffectedVariable(
                        name=target_var,
                        overlap_type=overlap_type,
                        redefines_chain=f"{s_ancestor.name} REDEFINES {t_ancestor.name}",
                        redefines_level=s_ancestor.level,
                        redefining_ancestor=s_ancestor.name,
                        redefined_ancestor=t_ancestor.name,
                    )
                if t_ancestor.redefines and t_ancestor.redefines.upper() == s_ancestor.name.upper():
                    return AffectedVariable(
                        name=target_var,
                        overlap_type=overlap_type,
                        redefines_chain=f"{t_ancestor.name} REDEFINES {s_ancestor.name}",
                        redefines_level=t_ancestor.level,
                        redefining_ancestor=t_ancestor.name,
                        redefined_ancestor=s_ancestor.name,
                    )

        # Fallback
        return AffectedVariable(
            name=target_var,
            overlap_type=overlap_type,
            redefines_chain="indirect",
            redefines_level=0,
            redefining_ancestor="",
            redefined_ancestor="",
        )

    def _get_ancestor_chain(self, item: DataItem) -> List[DataItem]:
        """Get the chain of ancestors from item to root.

        Args:
            item: Starting data item

        Returns:
            List of ancestors including the item itself
        """
        chain = [item]
        current = item.parent
        while current is not None:
            chain.append(current)
            current = current.parent
        return chain

    def get_all_affected_variables(self, variable_name: str) -> Dict[str, Any]:
        """Get complete affected variable information.

        Returns both record-level and variable-level affected items.

        Args:
            variable_name: Name of the variable

        Returns:
            Dictionary with 'records' and 'variables' keys
        """
        if not self._analyzed:
            self.analyze()

        affected_records = self.get_affected_records(variable_name)
        affected_variables = self.get_overlapping_variables(variable_name)

        return {
            "records": list(affected_records),
            "variables": affected_variables,
        }
