# FILLER REDEFINES Enhancement Strategy

## Summary

Enhance the paragraph-variables-map output to include:
1. **Position information** (byte offsets relative to base_record) for all variables to enable extracting variable values from logged data
2. **Copybook source information** in `defined_in_record` when variables are under a FILLER that REDEFINEs another record

## Problem Statement

Current output for ORDERMGMT.cbl:
```json
"CUSTOMER-ID": {
  "base_record": "ORDER-BUFFER",
  "defined_in_record": "FILLER$1",
  "explanation": "direct modification: MOVE at line 27"
}
```

The user needs:
- **Position**: To extract CUSTOMER-ID's value from logged ORDER-BUFFER data
- **Copybook info**: To know CUSTOMER-ID is from CUSTINFO copybook under a FILLER REDEFINES

## Desired Output

```json
"CUSTOMER-ID": {
  "base_record": "ORDER-BUFFER",
  "defined_in_record": "FILLER (CUSTINFO copybook)",
  "position": {
    "start": 1,
    "end": 10
  },
  "explanation": "direct modification: MOVE at line 27"
}
```

## Design Decisions

### 1. Position Information Strategy

**Source**: The `memory_regions` in analysis output already contains:
- `start_offset` (0-indexed byte offset)
- `size` (variable size in bytes)
- `record_name` (the Level 01 record containing the variable)

**Key insight**: When `FILLER$1 REDEFINES ORDER-BUFFER`, the `start_offset` in `memory_regions` is already relative to ORDER-BUFFER because REDEFINES shares the same memory starting point.

**Implementation**:
- Add a `position` property with `start` and `end` (1-indexed, COBOL convention) to ALL variable entries
- Calculate from existing memory_regions: `start = start_offset + 1`, `end = start + size - 1`
- Position is always included when available, regardless of REDEFINES structure

### 2. Copybook Source Information Strategy

**Source**: The `_line_mapping` tracks copybook source for each expanded line:
```json
"10": {
  "is_copybook": true,
  "original_line": 10,
  "source_file": "CUSTINFO"
}
```

**Available data**: Each `DataItem` in `program.all_data_items` has a `line_number` field.

**Implementation approach**:
- Add `definition_line` to `memory_regions` output in `ImpactAnalyzer._build_memory_regions()`
- In `ParagraphVariablesMapper`, look up variable's definition line in `_line_mapping`
- If `is_copybook: true`, use `source_file` as the copybook name

### 3. Defined_in_record Format for FILLER REDEFINES

When a variable is:
- Under a Level 01 FILLER that REDEFINEs another record
- Defined in a copybook

Format `defined_in_record` as: `"FILLER ({copybook_name} copybook)"`

Example: `"FILLER (CUSTINFO copybook)"`

If no copybook (defined in main source): `"FILLER"` (keep current FILLER$N format hidden from user)

### 4. 77-Level Variable Detection

77-level variables must be detected by checking the actual COBOL level number (77), not by hierarchy length. This prevents level-01 elementary items from being incorrectly flagged as 77-level.

**Implementation**:
- Add `level` field to `memory_regions` in `ImpactAnalyzer._build_memory_regions()`
- Update `_is_77_level()` to check `region.get("level") == 77`

## Implementation Steps

### Step 1: Add definition_line and level to memory_regions output

**File**: `src/analyzers/impact_analyzer.py`

Modify `_build_memory_regions()` (lines 519-533) to include definition line and level:

```python
def _build_memory_regions(self) -> Dict[str, Dict[str, Any]]:
    """Build memory region info for all data items."""
    memory_regions = {}
    for var_name, region in self.data_analyzer._memory_regions.items():
        memory_regions[var_name] = {
            "start_offset": region.start_offset,
            "size": region.size,
            "record_name": region.record_name,
        }
        # Add definition line and level for copybook source tracking and 77-level detection
        item = self.program.all_data_items.get(var_name.upper())
        if item:
            if item.line_number:
                memory_regions[var_name]["definition_line"] = item.line_number
            if item.level:
                memory_regions[var_name]["level"] = item.level
    return memory_regions
```

### Step 2: Add copybook lookup method to ParagraphVariablesMapper

**File**: `src/output/paragraph_variables_map.py`

Add new method:
```python
def _get_copybook_source(self, var_name: str) -> Optional[str]:
    """Get copybook name if variable is defined in a copybook.

    Args:
        var_name: Name of the variable

    Returns:
        Copybook name if variable is from a copybook, None otherwise
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
```

### Step 3: Add helper method for FILLER formatting

**File**: `src/output/paragraph_variables_map.py`

Add new method:
```python
def _format_defined_in_record(self, raw_record: str, var_name: str) -> str:
    """Format defined_in_record, handling FILLER cases specially.

    For variables under a FILLER that REDEFINEs another record,
    replace the internal FILLER$N name with a more user-friendly format
    that includes the copybook source.

    Args:
        raw_record: The raw Level 01 record name (may be FILLER$N)
        var_name: The variable name (for copybook lookup)

    Returns:
        Formatted record name
    """
    if not raw_record.upper().startswith("FILLER$"):
        return raw_record  # Normal named record, no change

    # It's a FILLER - check for copybook source
    copybook = self._get_copybook_source(var_name)
    if copybook:
        return f"FILLER ({copybook} copybook)"
    return "FILLER"
```

### Step 4: Update 77-level detection

**File**: `src/output/paragraph_variables_map.py`

Update `_is_77_level()` method:
```python
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
```

### Step 5: Modify _variable_info_to_output to include position for all variables

**File**: `src/output/paragraph_variables_map.py`

Update `_variable_info_to_output()` (lines 407-426):
```python
def _variable_info_to_output(self, info: VariableChangeInfo) -> Dict[str, Any]:
    """Convert VariableChangeInfo to output dictionary format."""
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
```

### Step 6: Update documentation

**File**: `docs/json-output-reference.md`

Add new properties:
- `position`: Object with `start` and `end` (1-indexed byte positions) - included for all variables
- Update `defined_in_record` description to explain FILLER format

## Files to Modify

1. `src/analyzers/impact_analyzer.py` - Add definition_line and level to memory_regions
2. `src/output/paragraph_variables_map.py` - Main implementation
3. `src/parser/cobol_parser.py` - Fix line number calculation for data items
4. `src/preprocessor/copy_resolver.py` - Fix copybook source tracking
5. `docs/json-output-reference.md` - Documentation update
6. `tests/test_paragraph_variables_map.py` - Add tests

## Testing Strategy

### Manual Testing
```bash
cobol-analyzer paragraph-variables-map complex-cobol-source/ORDERMGMT.cbl \
  -c complex-cobol-source/copybooks -o output-x
```

Verify output-x/ORDERMGMT-paragraph-variables.json contains:
```json
"CUSTOMER-ID": {
  "base_record": "ORDER-BUFFER",
  "defined_in_record": "FILLER (CUSTINFO copybook)",
  "position": {
    "start": 1,
    "end": 10
  },
  "explanation": "direct modification: MOVE at line 27"
}
```

### Unit Tests

Add new test classes in `tests/test_paragraph_variables_map.py`:

1. `TestFillerRedefinesPattern` - Tests for FILLER REDEFINES with copybook source
   - `test_filler_formatted_with_copybook` - FILLER record shows copybook source
   - `test_position_included_for_filler_redefines` - Position included for FILLER REDEFINES
   - `test_base_record_remains_correct` - base_record still shows the actual redefined record

2. `TestNormalRedefinesUnchanged` - Tests ensuring normal REDEFINES keeps defined_in_record unchanged
   - `test_named_record_not_formatted_as_filler` - Named records NOT formatted as FILLER
   - `test_position_included_for_all_variables` - Position included for all variables

3. `TestFillerWithoutCopybook` - Tests for FILLER from main source
   - `test_filler_without_copybook_shows_filler_only` - Shows "FILLER" without suffix

4. `TestFillerEdgeCases` - Edge case tests
   - `test_filler_without_line_mapping` - Fallback when no line mapping exists
   - `test_filler_same_as_base_no_special_formatting` - FILLER without REDEFINES relationship

### Regression Testing

Run existing tests:
```bash
pytest tests/test_paragraph_variables_map.py -v
```

## Edge Cases

1. **Multiple FILLER levels**: CUSTOMER-ID is under FILLER$2 which is under FILLER$1
   - Handle: Use Level 01 FILLER$1 for positions, track copybook from definition line

2. **Variable not in a copybook**: Defined directly in main source under FILLER
   - Handle: Show `"FILLER"` without copybook suffix

3. **Normal REDEFINES (non-FILLER)**: PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD
   - Handle: Keep defined_in_record as the named record, still include position

4. **77-level variables**: Standalone variables not in any record
   - Handle: Include position (they still have byte positions), flag as 77-level

5. **Level-01 elementary items**: Variables like `01 ORDER-BUFFER PIC X(30000)`
   - Handle: Do NOT flag as 77-level (check actual level number == 77)

## Constraints

- Position should use 1-indexed COBOL convention
- Copybook names should match what's in the COPY statement (e.g., "CUSTINFO" not "CUSTINFO.cpy")
- 77-level detection must use actual level number, not hierarchy length
