"""Variable filter for transforming section-centric analysis to variable-centric view."""

from dataclasses import dataclass, field
from typing import List, Dict, Any, Optional
from datetime import datetime
import time


@dataclass
class VariableModificationInfo:
    """Information about where a variable is modified."""

    section_or_paragraph: str
    modification_type: str
    line_number: int
    affected_records: List[str] = field(default_factory=list)
    # For REDEFINES-related modifications:
    modified_variable: Optional[str] = None
    overlap_type: Optional[str] = None
    redefines_chain: Optional[str] = None


class VariableFilter:
    """Transforms section-centric analysis to variable-centric view."""

    def __init__(self, analysis_data: Dict[str, Any]):
        """
        Initialize with analysis JSON from 'analyze' command.

        Args:
            analysis_data: Output dictionary from analyze_cobol_file()
        """
        self.analysis_data = analysis_data
        self._data_hierarchy = analysis_data.get("data_hierarchy", {})

    def _get_ancestors(self, variable_name: str) -> List[str]:
        """Get ancestor variable names (excluding the variable itself).

        Args:
            variable_name: Name of the variable

        Returns:
            List of ancestor names from root to parent (excluding self)
        """
        hierarchy = self._data_hierarchy.get(variable_name.upper(), [])
        # Return all ancestors except the variable itself
        return hierarchy[:-1] if len(hierarchy) > 1 else []

    def _find_ancestor_modifications(self, var_name: str) -> List[Dict[str, Any]]:
        """Find modifications where an ancestor of var_name is modified.

        Args:
            var_name: Name of the variable to find ancestor modifications for

        Returns:
            List of ancestor modification dictionaries
        """
        ancestor_mods = []
        ancestors = self._get_ancestors(var_name.upper())
        if not ancestors:
            return []

        ancestor_set = set(a.upper() for a in ancestors)

        for section_name, modifications in self.analysis_data.get(
            "sections_and_paragraphs", {}
        ).items():
            for mod in modifications:
                modified_var = mod.get("variable", "").upper()
                if modified_var in ancestor_set:
                    ancestor_mods.append({
                        "section_or_paragraph": section_name,
                        "modification_type": mod.get("modification_type"),
                        "line_number": mod.get("line_number"),
                        "affected_records": mod.get("affected_records", []),
                        "ancestor_variable": modified_var,
                        "ancestor_level": ancestors.index(modified_var) + 1
                    })

        return ancestor_mods

    def filter(
        self,
        variable_names: List[str],
        include_redefines: bool = True,
        include_ancestor_mods: bool = True
    ) -> Dict[str, Any]:
        """
        Filter analysis by variable names.

        Args:
            variable_names: List of COBOL variable names to filter
            include_redefines: Include indirect modifications via REDEFINES
            include_ancestor_mods: Include modifications to ancestor groups

        Returns:
            Variable-centric output dictionary
        """
        start_time = time.perf_counter()
        results = {}
        not_found = []

        # Normalize variable names to uppercase (COBOL is case-insensitive)
        normalized_names = [name.upper() for name in variable_names]

        for var_name in normalized_names:
            direct_mods = []
            redefines_mods = []
            ancestor_mods = []

            # Scan all sections and paragraphs
            sections_and_paragraphs = self.analysis_data.get("sections_and_paragraphs", {})
            for section_name, modifications in sections_and_paragraphs.items():
                for mod in modifications:
                    # Check for direct modification
                    if mod.get("variable", "").upper() == var_name:
                        direct_mods.append({
                            "section_or_paragraph": section_name,
                            "modification_type": mod.get("modification_type"),
                            "line_number": mod.get("line_number"),
                            "affected_records": mod.get("affected_records", [])
                        })

                    # Check for REDEFINES-related modification
                    elif include_redefines:
                        for av in mod.get("affected_variables", []):
                            if av.get("name", "").upper() == var_name:
                                redefines_mods.append({
                                    "section_or_paragraph": section_name,
                                    "modification_type": mod.get("modification_type"),
                                    "line_number": mod.get("line_number"),
                                    "affected_records": mod.get("affected_records", []),
                                    "modified_variable": mod.get("variable"),
                                    "overlap_type": av.get("overlap_type"),
                                    "redefines_chain": av.get("redefines_chain")
                                })

            # Find ancestor modifications
            if include_ancestor_mods:
                ancestor_mods = self._find_ancestor_modifications(var_name)

            if direct_mods or redefines_mods or ancestor_mods:
                results[var_name] = {
                    "direct_modifications": direct_mods,
                    "redefines_modifications": redefines_mods,
                    "ancestor_modifications": ancestor_mods
                }
            else:
                not_found.append(var_name)

        end_time = time.perf_counter()
        execution_time = end_time - start_time

        return self._build_output(variable_names, results, not_found, execution_time)

    def _build_output(
        self,
        requested_vars: List[str],
        results: Dict[str, Dict],
        not_found: List[str],
        execution_time: float
    ) -> Dict[str, Any]:
        """
        Build the final output dictionary.

        Args:
            requested_vars: Original list of requested variable names
            results: Filtered results dictionary
            not_found: List of variables that weren't found
            execution_time: Filter execution time in seconds

        Returns:
            Complete output dictionary
        """
        # Calculate totals
        total_direct = 0
        total_redefines = 0
        total_ancestor = 0
        for var_data in results.values():
            total_direct += len(var_data.get("direct_modifications", []))
            total_redefines += len(var_data.get("redefines_modifications", []))
            total_ancestor += len(var_data.get("ancestor_modifications", []))

        return {
            "program_name": self.analysis_data.get("program_name", "UNKNOWN"),
            "analysis_date": self.analysis_data.get("analysis_date", datetime.now().isoformat()),
            "execution_time_seconds": round(execution_time, 4),
            "filter_variables": [v.upper() for v in requested_vars],
            "variables": results,
            "summary": {
                "variables_requested": len(requested_vars),
                "variables_found": len(results),
                "variables_not_found": not_found,
                "total_direct_modifications": total_direct,
                "total_redefines_modifications": total_redefines,
                "total_ancestor_modifications": total_ancestor
            }
        }
