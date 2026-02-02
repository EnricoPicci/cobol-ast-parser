# Implementation Plan: Add Variable Access Tracking to variable_index

## Objective
Extend `variable_index` to track not only paragraphs that **modify** variables, but also paragraphs that **access** (read) them.

## Current State

The `SimplifiedStatement` class already has a `sources` field (line 296 in `cobol_parser.py`) that is **never populated**. The pipeline only tracks modifications.

### Current `variable_index` structure:
```python
{
    defined_in_record: {
        "start:end": {
            "variable_name": str,
            "paragraphs": [paragraph_names]  # Modifying paragraphs only
        }
    }
}
```

### Target `variable_index` structure:
```python
{
    defined_in_record: {
        "start:end": {
            "variable_name": str,
            "modifying_paragraphs": [paragraph_names],  # Renamed from "paragraphs"
            "accessing_paragraphs": [paragraph_names]   # NEW
        }
    }
}
```

## Files to Modify

### 1. `src/parser/cobol_parser.py`
- Extend `_extract_statements()` to populate `sources` field for each statement
- Extract source variables from:
  - MOVE source (`MOVE X TO Y` → X is read)
  - COMPUTE right-side (`COMPUTE Y = X + Z` → X, Z are read)
  - ADD/SUBTRACT/MULTIPLY/DIVIDE sources
  - IF/EVALUATE conditions (all variables referenced)
  - DISPLAY statements
  - PERFORM VARYING/UNTIL conditions

### 2. `src/cobol_ast/nodes.py`
- Add `VariableAccess` dataclass (parallel to `VariableModification`)
- Add `accesses` field to `Paragraph` class
- Add `standalone_accesses` field to `Section` class
- Add `accessed_variables` property to both

### 3. `src/cobol_ast/builder.py`
- Add `_extract_accesses()` method (parallel to `_extract_modifications`)
- Populate `accesses` field when building paragraphs/sections

### 4. `src/output/paragraph_variables_map.py`
- Add `VariableAccessInfo` dataclass
- Add `_collect_accessed_variables()` method
- Include accessed variables in output

### 5. `src/cobol_ast/api.py`
- Rename "paragraphs" → "modifying_paragraphs" in `_build_variable_index()`
- Add "accessing_paragraphs" array
- Update docstrings for `AnalysisResult.variable_index`

### 6. Tests
- Update existing tests for renamed field
- Add new tests for access tracking

## Detailed Implementation Steps

### Step 1: Parser - Extract Source Variables
Add source extraction patterns and methods:

```python
# Add to SimplifiedCobolParser class:

# Patterns for statements that read variables (for access tracking)
DISPLAY_STMT = re.compile(r"\bDISPLAY\s+([A-Za-z0-9][-A-Za-z0-9]*)", re.IGNORECASE)
IF_STMT = re.compile(r"\bIF\s+(.+?)(?:\s+THEN|\n)", re.IGNORECASE | re.DOTALL)
PERFORM_UNTIL = re.compile(r"\bPERFORM\s+.+?\s+UNTIL\s+([^.]+)", re.IGNORECASE)

def _extract_move_sources(self, match) -> List[str]:
    """Extract source from MOVE statement."""
    source_str = match.group(1)
    return self._split_variable_list(source_str)
```

### Step 2: Nodes - Add Access Tracking Classes

```python
@dataclass
class VariableAccess:
    """Records a variable access (read) in procedure code."""
    variable_name: str
    access_context: str  # e.g., "MOVE_SOURCE", "COMPUTE_EXPRESSION", "CONDITION"
    line_number: int
    statement_text: str = ""
    section_name: Optional[str] = None
    paragraph_name: Optional[str] = None

# Modify Paragraph class:
@dataclass
class Paragraph:
    name: str
    modifications: List[VariableModification] = field(default_factory=list)
    accesses: List[VariableAccess] = field(default_factory=list)  # NEW
    line_number: int = 0
    parent_section: Optional[str] = None

    @property
    def accessed_variables(self) -> Set[str]:  # NEW
        """Get set of all variables accessed in this paragraph."""
        return {acc.variable_name for acc in self.accesses}
```

### Step 3: Builder - Extract Accesses

```python
def _extract_accesses(
    self,
    statements: List[SimplifiedStatement],
    section_name: Optional[str],
    paragraph_name: Optional[str],
) -> List[VariableAccess]:
    """Extract variable accesses from statements."""
    accesses = []
    for stmt in statements:
        for source in stmt.sources:
            var_name = source.upper()
            if self._is_literal(var_name) or var_name in self.COBOL_KEYWORDS:
                continue
            access = VariableAccess(
                variable_name=var_name,
                access_context=f"{stmt.statement_type}_SOURCE",
                line_number=stmt.line_number,
                statement_text=stmt.text,
                section_name=section_name,
                paragraph_name=paragraph_name,
            )
            accesses.append(access)
    return accesses
```

### Step 4: API - Update variable_index Structure

```python
def _build_variable_index(
    paragraph_variables: Dict[str, Any]
) -> Dict[str, Dict[str, Dict[str, Any]]]:
    # ... existing logic ...

    if pos_key not in index[defined_in_record]:
        index[defined_in_record][pos_key] = {
            "variable_name": var_name,
            "modifying_paragraphs": [],   # Renamed from "paragraphs"
            "accessing_paragraphs": []     # NEW
        }
```

## Verification Plan

1. Run existing tests to verify no regressions: `pytest`
2. Add new test file: `tests/test_variable_access.py`
3. Test with fixture that has various read operations
4. Verify output includes both modifying and accessing paragraphs

## Confirmed Decisions

1. **Breaking API change accepted**: Rename `paragraphs` → `modifying_paragraphs`
2. **Line number tracking**: Yes, track line numbers for accesses (like modifications)

## Final `variable_index` Structure

```python
{
    defined_in_record: {
        "start:end": {
            "variable_name": str,
            "modifying_paragraphs": [paragraph_names],  # Renamed from "paragraphs"
            "accessing_paragraphs": [paragraph_names]   # NEW
        }
    }
}
```

## `paragraph_variables` Output Changes

The output will include both modified and accessed variables per paragraph:

```python
{
    "paragraphs": {
        "PROCESS-DATA": {
            "WS-AMOUNT": {
                "base_record": "WS-DATA",
                "defined_in_record": "WS-DATA",
                "position": {"start": 1, "end": 10},
                "modification_lines": [45, 67],      # Where modified
                "access_lines": [42, 50],            # Where accessed (NEW)
                "explanation": "..."
            }
        }
    }
}
```
