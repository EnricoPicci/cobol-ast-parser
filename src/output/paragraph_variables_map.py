"""Paragraph variables map for transforming analysis to paragraph-centric view."""

from typing import Dict, Any, Set, Optional, Tuple, List
from dataclasses import dataclass, field
from datetime import datetime
import time


@dataclass
class VariableChangeInfo:
    """Tracks why a variable changes in a paragraph."""

    variable_name: str
    defined_in_record: str
    base_record: str
    is_77_level: bool = False

    # All modification tracking
    modification_lines: Set[int] = field(default_factory=set)
    modification_types: Set[str] = field(default_factory=set)

    # Change type flags
    has_direct_modification: bool = False
    has_redefines_modification: bool = False
    has_ancestor_modification: bool = False

    # REDEFINES example (one example for explanation)
    # Keys: source_var, source_start, source_end, source_record,
    #       target_start, target_end, target_record, source_line
    redefines_example: Optional[Dict[str, Any]] = None

    # Ancestor info
    ancestor_name: Optional[str] = None


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
        self._memory_regions = analysis_data.get("memory_regions", {})
        self._line_mapping = analysis_data.get("_line_mapping", {})
        self._original_line_count = analysis_data.get("_original_line_count", 0)
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

        77-level variables are standalone items at level 77, not part of any
        record structure. We check the actual level number from memory_regions.

        Args:
            var_name: Name of the variable to check

        Returns:
            True if variable is a 77-level item
        """
        region = self._memory_regions.get(var_name.upper())
        if region and region.get("level") == 77:
            return True
        return False

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

    def _get_positions(self, var_name: str) -> Optional[Tuple[int, int]]:
        """Get 1-indexed positions for a variable.

        Args:
            var_name: Name of the variable

        Returns:
            Tuple of (start, end) positions (1-indexed, inclusive) or None if not found
        """
        region = self._memory_regions.get(var_name.upper())
        if region and region.get("size", 0) > 0:
            start = region["start_offset"] + 1  # Convert to 1-indexed
            end = start + region["size"] - 1    # Inclusive end
            return (start, end)
        return None

    def _get_original_line(self, expanded_line: int) -> int:
        """Convert an expanded line number to the original source line number.

        When COPY statements are resolved, line numbers in the expanded source
        may not match the original source file. This method converts expanded
        line numbers back to original line numbers.

        Args:
            expanded_line: Line number in the expanded (COPY-resolved) source

        Returns:
            Original line number in the source file, or the expanded line if
            it's within the original file range or no mapping exists
        """
        # If no line mapping, return as-is but cap at original line count if known
        if not self._line_mapping:
            if self._original_line_count > 0 and expanded_line > self._original_line_count:
                # Line is beyond original file, likely from copybook
                return expanded_line  # Can't map without mapping data
            return expanded_line

        # Look up the mapping
        mapping = self._line_mapping.get(str(expanded_line))
        if mapping:
            # If it's from a copybook, return the COPY statement line
            # If it's from main source, return the original line
            return mapping.get("original_line", expanded_line)

        # Fallback: if line is beyond original count, it's likely from copybook
        if self._original_line_count > 0 and expanded_line > self._original_line_count:
            # Try to find the nearest mapped line
            for line_num in range(expanded_line, 0, -1):
                mapping = self._line_mapping.get(str(line_num))
                if mapping and not mapping.get("is_copybook", False):
                    return mapping.get("original_line", expanded_line)
            return expanded_line

        return expanded_line

    def _get_copybook_source(self, var_name: str) -> Optional[str]:
        """Get copybook name if variable is defined in a copybook.

        Args:
            var_name: Name of the variable to check

        Returns:
            Copybook filename (without extension) if variable is from a copybook,
            None otherwise
        """
        region = self._memory_regions.get(var_name.upper())
        if not region:
            return None

        definition_line = region.get("definition_line")
        if not definition_line:
            return None

        mapping = self._line_mapping.get(str(definition_line))
        if mapping and mapping.get("is_copybook", False):
            return mapping.get("source_file")
        return None

    def _format_defined_in_record(self, raw_record: str, var_name: str) -> str:
        """Format defined_in_record, handling FILLER cases specially.

        For FILLER records, shows what the FILLER redefines (if applicable),
        or falls back to copybook source information.

        Args:
            raw_record: Raw record name (may be FILLER$n)
            var_name: Name of the variable (used to look up copybook source)

        Returns:
            Formatted record name:
            - Normal records: unchanged
            - FILLER that redefines: "FILLER ({redefined_record})"
            - FILLER from copybook: "FILLER ({copybook} copybook)"
            - FILLER from main source: "FILLER"
        """
        if not raw_record.upper().startswith("FILLER$"):
            return raw_record  # Normal named record, no change

        # It's a FILLER - first check if it redefines another record
        redefined = self._redefines_graph.get(raw_record.upper())
        if redefined:
            return f"FILLER ({redefined})"

        # Fallback: check for copybook source
        copybook = self._get_copybook_source(var_name)
        if copybook:
            return f"FILLER ({copybook} copybook)"
        return "FILLER"

    def _collect_changed_variables(
        self,
        section_name: str,
        include_redefines: bool = True,
        include_ancestor_mods: bool = True
    ) -> Dict[str, VariableChangeInfo]:
        """Collect all changed variables for a section/paragraph with explanation metadata.

        Collects from three sources:
        - Direct: mod["variable"]
        - REDEFINES: mod["affected_variables"][*]["name"]
        - Ancestor: children of modified groups

        Args:
            section_name: Name of the section/paragraph
            include_redefines: Include REDEFINES-affected variables
            include_ancestor_mods: Include ancestor-modified variables

        Returns:
            Dictionary of variable names to VariableChangeInfo objects
        """
        variables: Dict[str, VariableChangeInfo] = {}
        modifications = self.analysis_data.get("sections_and_paragraphs", {}).get(section_name, [])

        for mod in modifications:
            direct_var = mod.get("variable", "").upper()
            line_number = mod.get("line_number", 0)
            mod_type = mod.get("modification_type", "UNKNOWN")

            # Direct modification
            if direct_var and not self._is_filler(direct_var):
                if direct_var not in variables:
                    variables[direct_var] = self._create_variable_info(direct_var)

                info = variables[direct_var]
                info.has_direct_modification = True
                info.modification_lines.add(line_number)
                info.modification_types.add(mod_type)

            # REDEFINES-related modifications
            if include_redefines:
                for av in mod.get("affected_variables", []):
                    affected_var = av.get("name", "").upper()
                    if affected_var and not self._is_filler(affected_var):
                        if affected_var not in variables:
                            variables[affected_var] = self._create_variable_info(affected_var)

                        info = variables[affected_var]
                        info.has_redefines_modification = True
                        info.modification_lines.add(line_number)
                        info.modification_types.add(mod_type)

                        # Store one REDEFINES example if we don't have one yet
                        if info.redefines_example is None:
                            redefines_example = self._build_redefines_example(
                                source_var=direct_var,
                                target_var=affected_var,
                                redefines_chain=av.get("redefines_chain", ""),
                                source_line=line_number
                            )
                            if redefines_example:
                                info.redefines_example = redefines_example

            # Ancestor modifications - children of modified groups
            if include_ancestor_mods and direct_var:
                children = self._get_children_of_group(direct_var)
                for child in children:
                    if not self._is_filler(child):
                        if child not in variables:
                            variables[child] = self._create_variable_info(child)

                        info = variables[child]
                        info.has_ancestor_modification = True
                        info.modification_lines.add(line_number)
                        info.modification_types.add(mod_type)

                        # Store ancestor name if not set
                        if info.ancestor_name is None:
                            info.ancestor_name = direct_var

        return variables

    def _create_variable_info(self, var_name: str) -> VariableChangeInfo:
        """Create a new VariableChangeInfo for a variable.

        Args:
            var_name: Name of the variable

        Returns:
            New VariableChangeInfo object with basic record info
        """
        defined_in = self._get_record_for_variable(var_name)
        base_record = self._get_base_record(defined_in)

        return VariableChangeInfo(
            variable_name=var_name,
            defined_in_record=defined_in,
            base_record=base_record,
            is_77_level=self._is_77_level(var_name)
        )

    def _build_redefines_example(
        self,
        source_var: str,
        target_var: str,
        redefines_chain: str,
        source_line: int
    ) -> Optional[Dict[str, Any]]:
        """Build a REDEFINES example for the explanation.

        Args:
            source_var: The directly modified variable
            target_var: The variable affected via REDEFINES
            redefines_chain: The REDEFINES chain string (e.g., "A REDEFINES B")
            source_line: Line number of the modification

        Returns:
            Dictionary with REDEFINES example data, or None if positions unavailable
        """
        source_positions = self._get_positions(source_var)
        target_positions = self._get_positions(target_var)

        if not source_positions or not target_positions:
            return None

        # Parse the redefines chain to get record names
        source_record = self._get_record_for_variable(source_var)
        target_record = self._get_record_for_variable(target_var)

        return {
            "source_var": source_var,
            "source_start": source_positions[0],
            "source_end": source_positions[1],
            "source_record": source_record,
            "target_start": target_positions[0],
            "target_end": target_positions[1],
            "target_record": target_record,
            "source_line": source_line
        }

    def _build_explanation(self, info: VariableChangeInfo) -> str:
        """Build human-readable explanation for why variable changes.

        Args:
            info: VariableChangeInfo object with all change metadata

        Returns:
            Human-readable explanation string
        """
        # Convert expanded line numbers to original line numbers
        original_lines = sorted(set(
            self._get_original_line(line) for line in info.modification_lines
        ))

        if not original_lines:
            return "unknown modification"

        # Single modification case
        if len(original_lines) == 1:
            line = original_lines[0]
            if info.has_direct_modification:
                mod_types = "/".join(sorted(info.modification_types))
                return f"direct modification: {mod_types} at line {line}"
            elif info.has_ancestor_modification and info.ancestor_name:
                mod_types = "/".join(sorted(info.modification_types))
                return f"changes because ancestor group {info.ancestor_name} was modified ({mod_types} at line {line})"
            elif info.has_redefines_modification and info.redefines_example:
                ex = info.redefines_example
                source_line = self._get_original_line(ex['source_line'])
                return (
                    f"{info.variable_name} occupies positions {ex['target_start']}-{ex['target_end']} "
                    f"in record {ex['target_record']} which REDEFINEs {ex['source_record']} "
                    f"where {ex['source_var']} (modified at line {source_line}) "
                    f"occupies positions {ex['source_start']}-{ex['source_end']}"
                )
            elif info.has_redefines_modification:
                # REDEFINES but no position info available
                mod_types = "/".join(sorted(info.modification_types))
                return f"changes due to REDEFINES relationship ({mod_types} at line {line})"

        # Multiple modifications case
        lines_str = ", ".join(map(str, original_lines))
        explanation = f"affected by multiple modifications at lines {lines_str}"

        if info.has_redefines_modification and info.redefines_example:
            ex = info.redefines_example
            source_line = self._get_original_line(ex['source_line'])
            explanation += (
                f". Some modifications are due to REDEFINES: "
                f"{info.variable_name} occupies positions {ex['target_start']}-{ex['target_end']} "
                f"in record {ex['target_record']} which REDEFINEs {ex['source_record']} "
                f"where {ex['source_var']} (modified at line {source_line}) "
                f"occupies positions {ex['source_start']}-{ex['source_end']}"
            )
        elif info.has_redefines_modification:
            # REDEFINES but no position info
            explanation += ". Some modifications are due to REDEFINES relationship"

        return explanation

    def _variable_info_to_output(self, info: VariableChangeInfo) -> Dict[str, Any]:
        """Convert VariableChangeInfo to output dictionary format.

        Args:
            info: VariableChangeInfo object

        Returns:
            Dictionary suitable for JSON output
        """
        entry: Dict[str, Any] = {
            "base_record": info.base_record,
            "defined_in_record": self._format_defined_in_record(
                info.defined_in_record, info.variable_name
            ),
        }

        # Add position info for all variables (byte positions within record)
        positions = self._get_positions(info.variable_name)
        if positions:
            entry["position"] = {
                "start": positions[0],
                "end": positions[1]
            }

        if info.is_77_level:
            entry["77-level-var"] = True

        entry["explanation"] = self._build_explanation(info)

        return entry

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

        paragraphs: Dict[str, Dict[str, Dict[str, Any]]] = {}
        total_unique_vars: Set[str] = set()
        redefines_vars: Set[str] = set()
        ancestor_mod_vars: Set[str] = set()
        level_77_vars: Set[str] = set()

        sections_and_paragraphs = self.analysis_data.get("sections_and_paragraphs", {})

        for section_name in sections_and_paragraphs:
            variable_infos = self._collect_changed_variables(
                section_name,
                include_redefines=include_redefines,
                include_ancestor_mods=include_ancestor_mods
            )

            if variable_infos:  # Only include paragraphs with changes
                # Convert VariableChangeInfo objects to output format
                variables_output: Dict[str, Dict[str, Any]] = {}
                for var_name, info in variable_infos.items():
                    variables_output[var_name] = self._variable_info_to_output(info)

                    # Track statistics
                    total_unique_vars.add(var_name)
                    if info.defined_in_record != info.base_record:
                        redefines_vars.add(var_name)
                    if info.is_77_level:
                        level_77_vars.add(var_name)
                    if info.has_ancestor_modification and not info.has_direct_modification:
                        ancestor_mod_vars.add(var_name)

                paragraphs[section_name] = variables_output

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
