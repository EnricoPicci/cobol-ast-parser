# Strategy: Add Explanation to Affected Variables

## 1. Requirement Summary

Add an `explanation` property to each affected variable in the paragraph-variables JSON output, explaining **why** that variable may change during paragraph execution.

### Target Output Format
```json
"2000-PROCESS": {
  "PAY-BANK-CODE": {
    "base_record": "TRANSACTION-RECORD",
    "defined_in_record": "PAYMENT-DETAIL",
    "explanation": "affected by multiple modifications at lines 245, 267. Some modifications are due to REDEFINES: PAY-BANK-CODE occupies positions 15-22 in record PAYMENT-DETAIL which REDEFINEs TRANSACTION-RECORD where TRAN-CUSTOMER-ID (modified at line 245) occupies positions 10-25"
  },
  "TRAN-CUSTOMER-ID": {
    "base_record": "TRANSACTION-RECORD",
    "defined_in_record": "TRANSACTION-RECORD",
    "explanation": "direct modification: MOVE at line 245"
  },
  "WS-COUNTS": {
    "base_record": "WS-COUNTS",
    "defined_in_record": "WS-COUNTS",
    "77-level-var": true,
    "explanation": "affected by multiple modifications at lines 112, 156, 178"
  }
}
```

---

## 2. Design Decisions (User Confirmed)

### 2.1 Position Numbering Convention
**Decision:** Use 1-indexed, inclusive positions (COBOL convention)
- "positions 1-10" means bytes 1 through 10 inclusive
- Convert internal 0-indexed offsets by adding 1

### 2.2 Multiple Modifications Per Variable
**Decision:** Aggregate all line numbers in a single explanation with the format:
- "affected by multiple modifications at lines xx, yy, zz"
- If some modifications are due to REDEFINES, append: "Some modifications are due to REDEFINES:" followed by details for **one example** of the overlap

### 2.3 No Compact Mode
**Decision:** No `--compact-explanations` flag will be implemented. Single explanation format only.

### 2.4 No Backward Compatibility Required
**Decision:** The `explanation` field will always be included in the output. No opt-in flag needed.

---

## 3. Identified Gaps and Solutions

### 3.1 Ancestor Modification Explanation
When a group item is modified, all its children are also affected.

**Solution:** Use explanation format: "changes because ancestor group WS-COUNTS was modified at line 112"

### 3.2 Multiple REDEFINES Chains
A variable might be affected through multiple REDEFINES paths.

**Solution:** Show only one example of REDEFINES overlap in the explanation (per user requirement)

### 3.3 88-Level Condition Names
88-level items don't occupy memory but are bound to their parent's value.

**Solution:** Explain 88-level changes by referencing the parent: "changes because parent PAY-METHOD was modified at line 234"

---

## 4. Analysis of Existing Codebase

### 4.1 Available Information

| Data Needed | Source Location | Available? |
|-------------|-----------------|------------|
| Line number | `VariableImpact.line_number` | Yes |
| Modification type | `VariableImpact.modification_type` | Yes |
| Byte positions | `MemoryRegion.start_offset`, `.size` | Yes |
| REDEFINES chain | `AffectedVariable.redefines_chain` | Yes |
| Overlap type | `AffectedVariable.overlap_type` | Yes |
| Parent hierarchy | `DataItem.parent` | Yes |

### 4.2 Key Files to Modify

1. **`src/output/paragraph_variables_map.py`** - `ParagraphVariablesMapper` class
   - Primary file for generating paragraph-variables output
   - Current `_add_variable()` method only adds record info
   - Need to extend to include explanation

2. **`src/analyzers/impact_analyzer.py`** - `ImpactAnalyzer` class
   - Contains `VariableImpact` with line numbers and modification types
   - Need to expose memory region data for position calculations

### 4.3 Current Data Flow

```
                    ┌─────────────────────────────────────┐
                    │  ImpactAnalyzer.generate_output()   │
                    │                                     │
                    │  - sections_and_paragraphs: dict    │
                    │    - variable: str                  │
                    │    - line_number: int               │
                    │    - modification_type: str         │
                    │    - affected_variables: list       │
                    │       - name, overlap_type          │
                    │       - redefines_chain             │
                    │  - data_hierarchy: dict             │
                    └──────────────┬──────────────────────┘
                                   │
                                   ▼
                    ┌─────────────────────────────────────┐
                    │  ParagraphVariablesMapper.map()     │
                    │                                     │
                    │  Currently only extracts:           │
                    │  - variable names                   │
                    │  - record membership                │
                    │  - 77-level flag                    │
                    │                                     │
                    │  *** Needs to add: explanation ***  │
                    └─────────────────────────────────────┘
```

---

## 5. Implementation Strategy

### 5.1 Approach: Enrich ParagraphVariablesMapper

The most efficient approach is to enhance `ParagraphVariablesMapper` to:
1. Track the **reason** each variable is added (direct, REDEFINES, ancestor)
2. Collect all modification line numbers that affect each variable
3. Build the explanation string with aggregated line numbers and one REDEFINES example

### 5.2 Data Structure for Tracking

Create an intermediate structure to collect explanation components:

```python
@dataclass
class VariableChangeInfo:
    """Tracks why a variable changes."""
    variable_name: str
    defined_in_record: str
    base_record: str
    is_77_level: bool = False

    # All line numbers where modifications occur
    modification_lines: Set[int] = field(default_factory=set)
    modification_types: Set[str] = field(default_factory=set)

    # Change type flags
    has_direct_modification: bool = False
    has_redefines_modification: bool = False
    has_ancestor_modification: bool = False

    # For REDEFINES - store one example for explanation
    redefines_example: Optional[Dict] = None  # {source_var, source_positions, target_positions, ...}

    # For ancestor modifications
    ancestor_name: Optional[str] = None
    ancestor_line: Optional[int] = None
```

### 5.3 Explanation Templates

```python
def _build_explanation(self, info: VariableChangeInfo) -> str:
    """Build human-readable explanation for why variable changes."""
    lines = sorted(info.modification_lines)

    # Single modification
    if len(lines) == 1:
        if info.has_direct_modification:
            mod_types = "/".join(sorted(info.modification_types))
            return f"direct modification: {mod_types} at line {lines[0]}"
        elif info.has_ancestor_modification:
            return f"changes because ancestor group {info.ancestor_name} was modified at line {lines[0]}"
        elif info.has_redefines_modification and info.redefines_example:
            ex = info.redefines_example
            return (
                f"{info.variable_name} occupies positions {ex['target_start']}-{ex['target_end']} "
                f"in record {ex['target_record']} which REDEFINEs {ex['source_record']} "
                f"where {ex['source_var']} (modified at line {lines[0]}) "
                f"occupies positions {ex['source_start']}-{ex['source_end']}"
            )

    # Multiple modifications
    lines_str = ", ".join(map(str, lines))
    explanation = f"affected by multiple modifications at lines {lines_str}"

    if info.has_redefines_modification and info.redefines_example:
        ex = info.redefines_example
        explanation += (
            f". Some modifications are due to REDEFINES: "
            f"{info.variable_name} occupies positions {ex['target_start']}-{ex['target_end']} "
            f"in record {ex['target_record']} which REDEFINEs {ex['source_record']} "
            f"where {ex['source_var']} (modified at line {ex['source_line']}) "
            f"occupies positions {ex['source_start']}-{ex['source_end']}"
        )

    return explanation
```

---

## 6. Implementation Plan

### Phase 1: Extend Data Collection (impact_analyzer.py)

**Task 1.1: Add memory region info to output**
- Extend `_format_variable_impact()` to include offset/size data for affected variables
- Add new fields: `source_offset`, `source_size`, `target_offset`, `target_size`

**Files:** `src/analyzers/impact_analyzer.py`

**Estimated changes:** ~20 lines

### Phase 2: Enhance ParagraphVariablesMapper

**Task 2.1: Add VariableChangeInfo dataclass**
- Create dataclass at top of file to track explanation metadata
- Include fields for all change types and REDEFINES example

**Task 2.2: Add memory region lookup**
- Add `_memory_regions` dict populated from analysis data
- Add `_get_memory_positions()` helper to convert offsets to 1-indexed positions

**Task 2.3: Refactor `_collect_changed_variables()` method**
- Return `Dict[str, VariableChangeInfo]` instead of `Dict[str, Dict[str, Any]]`
- Track modification lines and types for each variable
- Capture one REDEFINES example when applicable
- Track ancestor modifications

**Task 2.4: Add `_build_explanation()` method**
- Implement logic per templates in section 5.3
- Handle single vs multiple modifications
- Include REDEFINES example when present

**Task 2.5: Update output generation**
- Modify `_add_variable()` or `map()` to include explanation in final output
- Ensure explanation is always present

**Files:** `src/output/paragraph_variables_map.py`

**Estimated changes:** ~150 lines

### Phase 3: Pass Memory Region Data

**Task 3.1: Include memory regions in analysis output**
- Modify `ImpactAnalyzer.generate_output()` to include memory region data
- Or pass DataStructureAnalyzer reference to ParagraphVariablesMapper

**Task 3.2: Update main.py if needed**
- Ensure memory region data flows to the mapper

**Files:** `src/analyzers/impact_analyzer.py`, `src/main.py`

**Estimated changes:** ~30 lines

### Phase 4: Testing

**Task 4.1: Add unit tests for explanation generation**
- Test single direct modification
- Test multiple direct modifications
- Test REDEFINES-related explanation with positions
- Test ancestor modification explanation
- Test mixed modification types

**Task 4.2: Integration testing**
- Run against TRANPROC.cbl - verify explanations
- Run against SIMPLE-PROGRAM.cbl - verify explanations
- Verify JSON output is valid and well-formatted

**Files:** `tests/test_paragraph_variables_map.py`

**Estimated changes:** ~100 lines of tests

---

## 7. Detailed Code Changes

### 7.1 Changes to `paragraph_variables_map.py`

```python
# New imports
from dataclasses import dataclass, field
from typing import List, Tuple, Optional, Set

# New dataclass (add near top of file)
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
    redefines_example: Optional[dict] = None

    # Ancestor info
    ancestor_name: Optional[str] = None


class ParagraphVariablesMapper:
    def __init__(self, analysis_data: Dict[str, Any]):
        # ... existing code ...
        self._memory_regions = analysis_data.get("memory_regions", {})

    def _get_positions(self, var_name: str) -> Optional[Tuple[int, int]]:
        """Get 1-indexed positions for a variable."""
        region = self._memory_regions.get(var_name.upper())
        if region:
            start = region["start_offset"] + 1  # Convert to 1-indexed
            end = start + region["size"] - 1    # Inclusive end
            return (start, end)
        return None

    def _build_explanation(self, info: VariableChangeInfo) -> str:
        """Build human-readable explanation for why variable changes."""
        lines = sorted(info.modification_lines)

        # Single modification case
        if len(lines) == 1:
            line = lines[0]
            if info.has_direct_modification:
                mod_types = "/".join(sorted(info.modification_types))
                return f"direct modification: {mod_types} at line {line}"
            elif info.has_ancestor_modification and info.ancestor_name:
                return f"changes because ancestor group {info.ancestor_name} was modified at line {line}"
            elif info.has_redefines_modification and info.redefines_example:
                ex = info.redefines_example
                return (
                    f"{info.variable_name} occupies positions {ex['target_start']}-{ex['target_end']} "
                    f"in record {ex['target_record']} which REDEFINEs {ex['source_record']} "
                    f"where {ex['source_var']} (modified at line {line}) "
                    f"occupies positions {ex['source_start']}-{ex['source_end']}"
                )

        # Multiple modifications case
        lines_str = ", ".join(map(str, lines))
        explanation = f"affected by multiple modifications at lines {lines_str}"

        if info.has_redefines_modification and info.redefines_example:
            ex = info.redefines_example
            explanation += (
                f". Some modifications are due to REDEFINES: "
                f"{info.variable_name} occupies positions {ex['target_start']}-{ex['target_end']} "
                f"in record {ex['target_record']} which REDEFINEs {ex['source_record']} "
                f"where {ex['source_var']} (modified at line {ex['source_line']}) "
                f"occupies positions {ex['source_start']}-{ex['source_end']}"
            )

        return explanation
```

### 7.2 Changes to `impact_analyzer.py`

```python
def generate_output(self) -> Dict[str, Any]:
    # ... existing code ...

    # Add memory regions to output
    memory_regions = {}
    for var_name, region in self.data_analyzer._memory_regions.items():
        memory_regions[var_name] = {
            "start_offset": region.start_offset,
            "size": region.size,
            "record_name": region.record_name
        }

    return {
        "program_name": self.program.name,
        "analysis_date": datetime.now().isoformat(),
        "sections_and_paragraphs": sections_and_paragraphs,
        "data_hierarchy": self._build_data_hierarchy(),
        "memory_regions": memory_regions,  # NEW
        "summary": summary,
    }
```

---

## 8. Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Memory region data missing for some variables | Medium | Medium | Use fallback text without positions |
| Explanation text too long for edge cases | Low | Low | Test with complex COBOL samples |
| Missing edge cases | Medium | Medium | Comprehensive testing with real COBOL |
| Performance impact from position lookups | Low | Low | Use dictionary lookups (O(1)) |

---

## 9. Success Criteria

1. All variables in paragraph-variables.json have an `explanation` property
2. Single direct modifications show: "direct modification: {type} at line {n}"
3. Multiple modifications show: "affected by multiple modifications at lines x, y, z"
4. REDEFINES changes include byte positions (1-indexed) and one overlap example
5. Ancestor modifications identify the modified ancestor group
6. Output remains valid JSON
7. Existing tests continue to pass

---

## 10. Example Outputs

### Single Direct Modification
```json
"WS-EOF": {
  "base_record": "WS-FLAGS",
  "defined_in_record": "WS-FLAGS",
  "explanation": "direct modification: MOVE at line 156"
}
```

### Multiple Direct Modifications
```json
"WS-ERROR-COUNT": {
  "base_record": "WS-COUNTS",
  "defined_in_record": "WS-COUNTS",
  "explanation": "affected by multiple modifications at lines 178, 234, 289"
}
```

### REDEFINES-Related Change
```json
"PAY-BANK-CODE": {
  "base_record": "TRANSACTION-RECORD",
  "defined_in_record": "PAYMENT-DETAIL",
  "explanation": "PAY-BANK-CODE occupies positions 15-22 in record PAYMENT-DETAIL which REDEFINEs TRANSACTION-RECORD where TRAN-CUSTOMER-ID (modified at line 245) occupies positions 10-25"
}
```

### Multiple Modifications with REDEFINES
```json
"PAY-AUTH-CODE": {
  "base_record": "TRANSACTION-RECORD",
  "defined_in_record": "PAYMENT-DETAIL",
  "explanation": "affected by multiple modifications at lines 245, 267, 312. Some modifications are due to REDEFINES: PAY-AUTH-CODE occupies positions 23-30 in record PAYMENT-DETAIL which REDEFINEs TRANSACTION-RECORD where TRAN-CUSTOMER-ID (modified at line 245) occupies positions 10-25"
}
```

### Ancestor Modification
```json
"WS-PAYMENT-COUNT": {
  "base_record": "WS-COUNTS",
  "defined_in_record": "WS-COUNTS",
  "explanation": "changes because ancestor group WS-COUNTS was modified at line 112"
}
```
