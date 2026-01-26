"""Paragraph variables map for transforming analysis to paragraph-centric view."""

from typing import Dict, Any, Set
from datetime import datetime
import time


class ParagraphVariablesMapper:
    """Transforms analysis to paragraph -> changed variables map."""

    def __init__(self, analysis_data: Dict[str, Any]):
        """
        Initialize with analysis JSON from 'analyze' command.

        Args:
            analysis_data: Output dictionary from analyze_cobol_file()
        """
        self.analysis_data = analysis_data
        self._data_hierarchy = analysis_data.get("data_hierarchy", {})
        self._redefines_graph = self._build_redefines_graph()

    def _build_redefines_graph(self) -> Dict[str, str]:
        """Build {redefining -> redefined} from redefines_chain strings.

        Parses redefines_chain entries like "PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD"
        to build a mapping for REDEFINES chain resolution.

        Returns:
            Dictionary mapping redefining record name to redefined record name
        """
        graph = {}
        for section_mods in self.analysis_data.get("sections_and_paragraphs", {}).values():
            for mod in section_mods:
                for av in mod.get("affected_variables", []):
                    chain = av.get("redefines_chain", "")
                    # Parse "PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD"
                    if " REDEFINES " in chain:
                        parts = chain.split(" REDEFINES ")
                        if len(parts) == 2:
                            graph[parts[0].strip().upper()] = parts[1].strip().upper()
        return graph

    def _get_base_record(self, record_name: str) -> str:
        """Follow REDEFINES chain to find ultimate non-REDEFINE root.

        Args:
            record_name: Name of the record to find base for

        Returns:
            Name of the base record (follows REDEFINES chain to root)
        """
        visited = set()
        current = record_name.upper()
        while current in self._redefines_graph and current not in visited:
            visited.add(current)
            current = self._redefines_graph[current]
        return current

    def _get_record_for_variable(self, var_name: str) -> str:
        """Get the Level 01 record containing this variable.

        Args:
            var_name: Name of the variable

        Returns:
            Name of the Level 01 record, or the variable name if not found
        """
        hierarchy = self._data_hierarchy.get(var_name.upper(), [])
        if hierarchy:
            return hierarchy[0].upper()
        return var_name.upper()

    def _is_77_level(self, var_name: str) -> bool:
        """Check if variable is a 77-level (standalone) item.

        77-level variables have a hierarchy containing only themselves.

        Args:
            var_name: Name of the variable to check

        Returns:
            True if variable is a 77-level item
        """
        hierarchy = self._data_hierarchy.get(var_name.upper(), [])
        # 77-level vars have hierarchy containing only themselves
        return len(hierarchy) == 1 and hierarchy[0].upper() == var_name.upper()

    def _is_filler(self, var_name: str) -> bool:
        """Check if variable is a FILLER item (internal placeholder).

        FILLER items are given unique names like FILLER$1, FILLER$2, etc.
        during parsing to track their memory positions, but they should
        not appear in user-facing output.

        Args:
            var_name: Name of the variable to check

        Returns:
            True if variable is a FILLER item
        """
        return var_name.upper().startswith("FILLER$")

    def _get_children_of_group(self, group_name: str) -> Set[str]:
        """Find all variables where group appears in their hierarchy.

        Args:
            group_name: Name of the group variable

        Returns:
            Set of variable names that are children of the group
        """
        children = set()
        group_upper = group_name.upper()
        for var_name, hierarchy in self._data_hierarchy.items():
            # Check if group is an ancestor (but not the variable itself)
            hierarchy_upper = [h.upper() for h in hierarchy]
            if group_upper in hierarchy_upper and hierarchy_upper[-1] != group_upper:
                children.add(var_name.upper())
        return children

    def _collect_changed_variables(
        self,
        section_name: str,
        include_redefines: bool = True,
        include_ancestor_mods: bool = True
    ) -> Dict[str, Dict[str, Any]]:
        """Collect all changed variables for a section/paragraph.

        Collects from three sources:
        - Direct: mod["variable"]
        - REDEFINES: mod["affected_variables"][*]["name"]
        - Ancestor: children of modified groups

        Args:
            section_name: Name of the section/paragraph
            include_redefines: Include REDEFINES-affected variables
            include_ancestor_mods: Include ancestor-modified variables

        Returns:
            Dictionary of variable names to their record info
        """
        variables = {}
        modifications = self.analysis_data.get("sections_and_paragraphs", {}).get(section_name, [])

        for mod in modifications:
            # Direct modification
            direct_var = mod.get("variable", "").upper()
            if direct_var and direct_var not in variables and not self._is_filler(direct_var):
                self._add_variable(variables, direct_var)

            # REDEFINES-related modifications
            if include_redefines:
                for av in mod.get("affected_variables", []):
                    affected_var = av.get("name", "").upper()
                    if affected_var and affected_var not in variables and not self._is_filler(affected_var):
                        self._add_variable(variables, affected_var)

            # Ancestor modifications - children of modified groups
            if include_ancestor_mods and direct_var:
                children = self._get_children_of_group(direct_var)
                for child in children:
                    if child not in variables and not self._is_filler(child):
                        self._add_variable(variables, child)

        return variables

    def _add_variable(self, variables: Dict[str, Dict[str, Any]], var_name: str) -> None:
        """Add a variable to the variables dictionary with record info.

        Args:
            variables: Dictionary to add variable to
            var_name: Name of the variable to add
        """
        defined_in = self._get_record_for_variable(var_name)
        base_record = self._get_base_record(defined_in)

        entry = {
            "defined_in_record": defined_in,
            "base_record": base_record
        }

        if self._is_77_level(var_name):
            entry["77-level-var"] = True

        variables[var_name] = entry

    def map(
        self,
        include_redefines: bool = True,
        include_ancestor_mods: bool = True
    ) -> Dict[str, Any]:
        """Transform analysis to paragraph -> changed variables map.

        Args:
            include_redefines: Include REDEFINES-affected variables
            include_ancestor_mods: Include ancestor-modified variables

        Returns:
            Output dictionary with paragraph-centric variable mapping
        """
        start_time = time.perf_counter()

        paragraphs = {}
        total_unique_vars = set()
        redefines_vars = set()
        ancestor_mod_vars = set()
        level_77_vars = set()

        sections_and_paragraphs = self.analysis_data.get("sections_and_paragraphs", {})

        for section_name in sections_and_paragraphs:
            variables = self._collect_changed_variables(
                section_name,
                include_redefines=include_redefines,
                include_ancestor_mods=include_ancestor_mods
            )

            if variables:  # Only include paragraphs with changes
                paragraphs[section_name] = variables
                total_unique_vars.update(variables.keys())

                # Track statistics
                for var_name, var_info in variables.items():
                    if var_info.get("defined_in_record") != var_info.get("base_record"):
                        redefines_vars.add(var_name)
                    if var_info.get("77-level-var"):
                        level_77_vars.add(var_name)

        # Count ancestor modification vars by comparing direct mods vs total
        if include_ancestor_mods:
            for section_name in sections_and_paragraphs:
                direct_vars = self._collect_changed_variables(
                    section_name,
                    include_redefines=include_redefines,
                    include_ancestor_mods=False
                )
                full_vars = self._collect_changed_variables(
                    section_name,
                    include_redefines=include_redefines,
                    include_ancestor_mods=True
                )
                ancestor_mod_vars.update(
                    set(full_vars.keys()) - set(direct_vars.keys())
                )

        end_time = time.perf_counter()
        execution_time = end_time - start_time

        return self._build_output(
            paragraphs,
            total_unique_vars,
            redefines_vars,
            ancestor_mod_vars,
            level_77_vars,
            execution_time
        )

    def _build_output(
        self,
        paragraphs: Dict[str, Dict[str, Dict[str, Any]]],
        total_unique_vars: Set[str],
        redefines_vars: Set[str],
        ancestor_mod_vars: Set[str],
        level_77_vars: Set[str],
        execution_time: float
    ) -> Dict[str, Any]:
        """Build the final output dictionary.

        Args:
            paragraphs: Paragraph to variables mapping
            total_unique_vars: Set of all unique variable names
            redefines_vars: Set of variables in REDEFINES records
            ancestor_mod_vars: Set of variables modified via ancestor
            level_77_vars: Set of 77-level variables
            execution_time: Time taken to perform the mapping

        Returns:
            Complete output dictionary
        """
        return {
            "program_name": self.analysis_data.get("program_name", "UNKNOWN"),
            "analysis_date": self.analysis_data.get("analysis_date", datetime.now().isoformat()),
            "execution_time_seconds": round(execution_time, 4),
            "paragraphs": paragraphs,
            "summary": {
                "total_paragraphs_with_changes": len(paragraphs),
                "total_unique_variables": len(total_unique_vars),
                "variables_in_redefines_records": len(redefines_vars),
                "variables_via_ancestor_modification": len(ancestor_mod_vars),
                "level_77_variables": len(level_77_vars)
            }
        }
