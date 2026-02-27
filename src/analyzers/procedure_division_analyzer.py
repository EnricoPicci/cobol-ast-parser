"""Procedure Division analyzer for COBOL programs.

Extracts structural information from the PROCEDURE DIVISION including:
- Section and paragraph inventory with line ranges
- PERFORM graph (call targets, THRU ranges)
- GO TO graph (targets, conditional detection)
- CALL graph (static/dynamic, USING fields)
- Conditional branches (IF/EVALUATE with nesting)
- Field references (reads, writes, conditions tested)
"""

from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any
import re


@dataclass
class ProcedureEntry:
    """An entry (section or paragraph) in the PROCEDURE DIVISION inventory."""
    name: str
    type: str  # "paragraph" or "section"
    parent_section: Optional[str]
    line_start: int
    line_end: int
    line_count: int
    paragraphs: Optional[List[str]] = None  # child paragraph names (sections only)


@dataclass
class PerformEntry:
    """A PERFORM statement target."""
    target: str
    type: str  # "simple", "thru", "until", "times", "varying"
    thru_target: Optional[str] = None
    thru_includes: List[str] = field(default_factory=list)
    condition: Optional[str] = None
    times: Optional[int] = None
    line: int = 0


@dataclass
class GotoEntry:
    """A GO TO statement target."""
    target: str
    line: int = 0
    conditional: bool = False
    condition_text: Optional[str] = None


@dataclass
class CallEntry:
    """A CALL statement."""
    target: str
    line: int = 0
    using_fields: List[str] = field(default_factory=list)
    is_dynamic: bool = False


@dataclass
class ConditionBranch:
    """A branch within a conditional statement."""
    condition: Optional[str]  # None for ELSE/WHEN OTHER
    line: int = 0


@dataclass
class ConditionEntry:
    """A conditional statement (IF or EVALUATE)."""
    type: str  # "IF" or "EVALUATE"
    condition: Optional[str]  # Full condition for IF, subject for EVALUATE
    subject: Optional[str]  # EVALUATE subject variable
    line: int = 0
    has_else: bool = False
    branches: List[ConditionBranch] = field(default_factory=list)
    nested_conditions: List["ConditionEntry"] = field(default_factory=list)


@dataclass
class FieldReferenceEntry:
    """Field reference aggregation for a paragraph."""
    reads: List[str] = field(default_factory=list)
    writes: List[str] = field(default_factory=list)
    conditions_tested: List[str] = field(default_factory=list)


class ProcedureDivisionAnalyzer:
    """Analyzes the PROCEDURE DIVISION structure of a COBOL program.

    Operates on the SimplifiedParseTree and CobolProgram to build six views:
    1. Section and paragraph inventory with line ranges
    2. PERFORM graph
    3. GO TO graph
    4. CALL graph
    5. Condition map
    6. Field references
    """

    # COBOL statements that may appear alone on a line and look like paragraph headers
    _STATEMENT_KEYWORDS = {
        "EXIT", "CONTINUE", "STOP", "GOBACK",
    }

    def __init__(self, parse_tree: Any, program: Any, source_lines: List[str]) -> None:
        self.parse_tree = parse_tree
        self.program = program
        self.source_lines = source_lines

        self.inventory: List[ProcedureEntry] = []
        self.perform_graph: Dict[str, List[PerformEntry]] = {}
        self.goto_graph: Dict[str, List[GotoEntry]] = {}
        self.call_graph: Dict[str, List[CallEntry]] = {}
        self.conditions: Dict[str, List[ConditionEntry]] = {}
        self.field_references: Dict[str, FieldReferenceEntry] = {}

    def analyze(self) -> None:
        """Run all six extraction passes."""
        self._build_inventory()
        self._build_perform_graph()
        self._build_goto_graph()
        self._build_call_graph()
        self._build_condition_map()
        self._build_field_references()

    def _build_inventory(self) -> None:
        """Build ordered list of sections and paragraphs with line ranges."""
        proc_div = self.parse_tree.procedure_division
        total_lines = len(self.source_lines)

        raw_entries: List[Dict[str, Any]] = []

        for section in proc_div.sections:
            # Collect child paragraph names (excluding misidentified keywords)
            child_names = [
                p.name for p in section.paragraphs
                if p.name not in self._STATEMENT_KEYWORDS
            ]

            # Add the section entry itself
            raw_entries.append({
                "name": section.name,
                "type": "section",
                "parent_section": None,
                "line_start": section.line_number,
                "paragraphs": child_names,
            })

            for para in section.paragraphs:
                if para.name in self._STATEMENT_KEYWORDS:
                    continue
                raw_entries.append({
                    "name": para.name,
                    "type": "paragraph",
                    "parent_section": section.name,
                    "line_start": para.line_number,
                    "paragraphs": None,
                })

        for para in proc_div.paragraphs:
            if para.name in self._STATEMENT_KEYWORDS:
                continue
            raw_entries.append({
                "name": para.name,
                "type": "paragraph",
                "parent_section": None,
                "line_start": para.line_number,
                "paragraphs": None,
            })

        # Sort by line_start
        raw_entries.sort(key=lambda e: e["line_start"])

        # Compute line_end from next entry's line_start
        for i, entry in enumerate(raw_entries):
            if i + 1 < len(raw_entries):
                entry["line_end"] = raw_entries[i + 1]["line_start"] - 1
            else:
                entry["line_end"] = total_lines

            entry["line_count"] = max(0, entry["line_end"] - entry["line_start"] + 1)

        self.inventory = [
            ProcedureEntry(
                name=e["name"],
                type=e["type"],
                parent_section=e["parent_section"],
                line_start=e["line_start"],
                line_end=e["line_end"],
                line_count=e["line_count"],
                paragraphs=e["paragraphs"],
            )
            for e in raw_entries
        ]

    def _get_entry_names_ordered(self) -> List[str]:
        """Get section and paragraph names in order of appearance."""
        return [p.name for p in self.inventory]

    def _resolve_thru_range(self, start: str, end: str) -> List[str]:
        """Resolve PERFORM THRU range to list of included entry names."""
        names = self._get_entry_names_ordered()
        try:
            start_idx = names.index(start)
            end_idx = names.index(end)
        except ValueError:
            return []

        if start_idx <= end_idx:
            return names[start_idx:end_idx + 1]
        return []

    def _build_perform_graph(self) -> None:
        """Build PERFORM graph from PERFORM_TARGET statements."""
        proc_div = self.parse_tree.procedure_division

        for section in proc_div.sections:
            # Standalone section statements
            entries = self._extract_performs_from_statements(section.statements)
            if entries:
                self.perform_graph[section.name] = entries

            for para in section.paragraphs:
                entries = self._extract_performs_from_statements(para.statements)
                if entries:
                    self.perform_graph[para.name] = entries

        for para in proc_div.paragraphs:
            entries = self._extract_performs_from_statements(para.statements)
            if entries:
                self.perform_graph[para.name] = entries

    def _extract_performs_from_statements(
        self, statements: List[Any]
    ) -> List[PerformEntry]:
        """Extract PERFORM entries from a list of statements."""
        entries: List[PerformEntry] = []
        for stmt in statements:
            if stmt.statement_type != "PERFORM_TARGET":
                continue

            target = stmt.targets[0] if stmt.targets else ""
            thru_target = stmt.targets[1] if len(stmt.targets) > 1 else None

            # Determine type
            text_upper = stmt.text.upper()
            if "TIMES" in text_upper:
                perform_type = "times"
                # Extract times count from text
                times_match = re.search(r"(\d+)\s+TIMES", text_upper)
                times = int(times_match.group(1)) if times_match else None
            elif "UNTIL" in text_upper:
                perform_type = "until"
                times = None
            elif "VARYING" in text_upper:
                perform_type = "varying"
                times = None
            elif thru_target:
                perform_type = "thru"
                times = None
            else:
                perform_type = "simple"
                times = None

            # Extract condition
            condition = None
            if stmt.sources and perform_type in ("until", "varying"):
                condition = stmt.sources[0] if stmt.sources else None

            # Resolve THRU range
            thru_includes: List[str] = []
            if thru_target:
                thru_includes = self._resolve_thru_range(target, thru_target)

            entry = PerformEntry(
                target=target,
                type=perform_type,
                thru_target=thru_target,
                thru_includes=thru_includes,
                condition=condition,
                times=times,
                line=stmt.line_number,
            )
            entries.append(entry)

        return entries

    def _build_goto_graph(self) -> None:
        """Build GO TO graph from GOTO_TARGET statements."""
        proc_div = self.parse_tree.procedure_division

        for section in proc_div.sections:
            # Standalone section statements
            entries = self._extract_gotos_from_statements(
                section.statements, section.name
            )
            if entries:
                self.goto_graph[section.name] = entries

            for para in section.paragraphs:
                entries = self._extract_gotos_from_statements(
                    para.statements, para.name
                )
                if entries:
                    self.goto_graph[para.name] = entries

        for para in proc_div.paragraphs:
            entries = self._extract_gotos_from_statements(
                para.statements, para.name
            )
            if entries:
                self.goto_graph[para.name] = entries

    def _extract_gotos_from_statements(
        self, statements: List[Any], para_name: str
    ) -> List[GotoEntry]:
        """Extract GO TO entries from statements."""
        entries: List[GotoEntry] = []
        for stmt in statements:
            if stmt.statement_type != "GOTO_TARGET":
                continue

            target = stmt.targets[0] if stmt.targets else ""

            # Detect if this GO TO is inside a conditional
            conditional = False
            condition_text = None
            # Check if there's an IF or EVALUATE context by looking at nearby statements
            for other_stmt in statements:
                if other_stmt.statement_type == "IF" and other_stmt.line_number <= stmt.line_number:
                    # IF is before or on the same line as the GO TO
                    conditional = True
                    condition_text = other_stmt.text[:80] if other_stmt.text else None
                    break

            entries.append(
                GotoEntry(
                    target=target,
                    line=stmt.line_number,
                    conditional=conditional,
                    condition_text=condition_text,
                )
            )

        return entries

    def _build_call_graph(self) -> None:
        """Build CALL graph from CALL_PROGRAM statements."""
        proc_div = self.parse_tree.procedure_division

        for section in proc_div.sections:
            # Standalone section statements
            entries = self._extract_calls_from_statements(section.statements)
            if entries:
                self.call_graph[section.name] = entries

            for para in section.paragraphs:
                entries = self._extract_calls_from_statements(para.statements)
                if entries:
                    self.call_graph[para.name] = entries

        for para in proc_div.paragraphs:
            entries = self._extract_calls_from_statements(para.statements)
            if entries:
                self.call_graph[para.name] = entries

    def _extract_calls_from_statements(
        self, statements: List[Any]
    ) -> List[CallEntry]:
        """Extract CALL entries from statements."""
        entries: List[CallEntry] = []
        for stmt in statements:
            if stmt.statement_type != "CALL_PROGRAM":
                continue

            target = stmt.targets[0] if stmt.targets else ""
            # Dynamic calls use identifier (uppercase), static use literal
            # We check if the original text has quotes around the program name
            is_dynamic = "'" not in stmt.text and '"' not in stmt.text

            entries.append(
                CallEntry(
                    target=target,
                    line=stmt.line_number,
                    using_fields=list(stmt.sources),
                    is_dynamic=is_dynamic,
                )
            )

        return entries

    def _build_condition_map(self) -> None:
        """Build condition map by scanning source lines within each paragraph."""
        # IF pattern
        if_pattern = re.compile(
            r"\bIF\s+(.+?)(?:\s*$|\s+THEN\s*$)",
            re.IGNORECASE,
        )
        else_pattern = re.compile(r"\bELSE\b", re.IGNORECASE)
        evaluate_pattern = re.compile(
            r"\bEVALUATE\s+(.+?)(?:\s*$)",
            re.IGNORECASE,
        )
        when_pattern = re.compile(
            r"\bWHEN\s+(.+?)(?:\s*$)",
            re.IGNORECASE,
        )

        for para_info in self.inventory:
            conditions: List[ConditionEntry] = []

            start = max(0, para_info.line_start - 1)
            end = min(len(self.source_lines), para_info.line_end)
            para_lines = self.source_lines[start:end]

            i = 0
            while i < len(para_lines):
                line = para_lines[i].strip()
                line_num = para_info.line_start + i

                # Check for EVALUATE
                eval_match = evaluate_pattern.search(line)
                if eval_match:
                    subject_text = eval_match.group(1).strip()
                    # Extract variable from subject
                    subject_var = None
                    if subject_text.upper() != "TRUE":
                        var_match = re.match(r"([A-Za-z0-9][-A-Za-z0-9]*)", subject_text)
                        if var_match:
                            subject_var = var_match.group(1).upper()

                    branches: List[ConditionBranch] = []
                    # Scan forward for WHEN clauses
                    j = i + 1
                    while j < len(para_lines):
                        when_line = para_lines[j].strip()
                        when_line_num = para_info.line_start + j
                        if re.search(r"\bEND-EVALUATE\b", when_line, re.IGNORECASE):
                            break
                        when_match = when_pattern.search(when_line)
                        if when_match:
                            when_value = when_match.group(1).strip()
                            if when_value.upper() == "OTHER":
                                branches.append(ConditionBranch(
                                    condition=None, line=when_line_num
                                ))
                            else:
                                branches.append(ConditionBranch(
                                    condition=when_value, line=when_line_num
                                ))
                        j += 1

                    conditions.append(ConditionEntry(
                        type="EVALUATE",
                        condition=subject_text,
                        subject=subject_var,
                        line=line_num,
                        has_else=any(b.condition is None for b in branches),
                        branches=branches,
                    ))
                    i = j + 1
                    continue

                # Check for IF
                if_match = if_pattern.search(line)
                if if_match:
                    condition_text = if_match.group(1).strip()
                    has_else = False
                    nested: List[ConditionEntry] = []

                    # Scan forward for ELSE, nested IF, and nested EVALUATE
                    j = i + 1
                    depth = 1
                    while j < len(para_lines) and depth > 0:
                        inner_line = para_lines[j].strip()
                        inner_line_num = para_info.line_start + j
                        if re.search(r"\bEND-IF\b", inner_line, re.IGNORECASE):
                            depth -= 1
                        elif re.search(r"\bIF\b", inner_line, re.IGNORECASE) and depth == 1:
                            # Nested IF at current depth
                            nested_if_match = if_pattern.search(inner_line)
                            if nested_if_match:
                                nested.append(ConditionEntry(
                                    type="IF",
                                    condition=nested_if_match.group(1).strip(),
                                    subject=None,
                                    line=inner_line_num,
                                ))
                            depth += 1
                        elif else_pattern.search(inner_line) and depth == 1:
                            has_else = True
                        # Detect nested EVALUATE inside IF
                        nested_eval = evaluate_pattern.search(inner_line)
                        if nested_eval and depth == 1:
                            eval_subject_text = nested_eval.group(1).strip()
                            eval_subject_var = None
                            if eval_subject_text.upper() != "TRUE":
                                var_m = re.match(r"([A-Za-z0-9][-A-Za-z0-9]*)", eval_subject_text)
                                if var_m:
                                    eval_subject_var = var_m.group(1).upper()
                            eval_branches: List[ConditionBranch] = []
                            k = j + 1
                            while k < len(para_lines):
                                eval_line = para_lines[k].strip()
                                eval_line_num = para_info.line_start + k
                                if re.search(r"\bEND-EVALUATE\b", eval_line, re.IGNORECASE):
                                    break
                                wm = when_pattern.search(eval_line)
                                if wm:
                                    wv = wm.group(1).strip()
                                    if wv.upper() == "OTHER":
                                        eval_branches.append(ConditionBranch(condition=None, line=eval_line_num))
                                    else:
                                        eval_branches.append(ConditionBranch(condition=wv, line=eval_line_num))
                                k += 1
                            nested.append(ConditionEntry(
                                type="EVALUATE",
                                condition=eval_subject_text,
                                subject=eval_subject_var,
                                line=inner_line_num,
                                has_else=any(b.condition is None for b in eval_branches),
                                branches=eval_branches,
                            ))
                            j = k + 1
                            continue
                        j += 1

                    conditions.append(ConditionEntry(
                        type="IF",
                        condition=condition_text,
                        subject=None,
                        line=line_num,
                        has_else=has_else,
                        nested_conditions=nested,
                    ))
                    i = j
                    continue

                i += 1

            if conditions:
                self.conditions[para_info.name] = conditions

    def _build_field_references(self) -> None:
        """Build field references by aggregating from AST section/paragraph data."""
        # Collect from sections
        for section in self.program.sections:
            # Standalone section modifications/accesses
            if section.standalone_modifications or section.standalone_accesses:
                self._aggregate_field_refs_from_lists(
                    section.name,
                    section.standalone_modifications,
                    section.standalone_accesses,
                )
            for para in section.paragraphs:
                self._aggregate_field_refs_from_lists(
                    para.name, para.modifications, para.accesses
                )

        # Collect from top-level paragraphs
        for para in self.program.paragraphs:
            self._aggregate_field_refs_from_lists(
                para.name, para.modifications, para.accesses
            )

    def _aggregate_field_refs_from_lists(
        self,
        name: str,
        modifications: List[Any],
        accesses: List[Any],
    ) -> None:
        """Aggregate field references from modification and access lists."""
        all_data_items = self.program.all_data_items
        writes: List[str] = []
        reads: List[str] = []
        conditions_tested: List[str] = []

        for mod in modifications:
            var_name = mod.variable_name
            if var_name not in writes:
                writes.append(var_name)

        for acc in accesses:
            var_name = acc.variable_name
            if var_name not in reads:
                reads.append(var_name)

        # Find 88-level items referenced in conditions
        for var_name in reads:
            item = all_data_items.get(var_name)
            if item and item.level == 88:
                if var_name not in conditions_tested:
                    conditions_tested.append(var_name)

        # Also check writes for SET of 88-level items
        for var_name in writes:
            item = all_data_items.get(var_name)
            if item and item.level == 88:
                if var_name not in conditions_tested:
                    conditions_tested.append(var_name)

        if writes or reads or conditions_tested:
            self.field_references[name] = FieldReferenceEntry(
                reads=reads,
                writes=writes,
                conditions_tested=conditions_tested,
            )
