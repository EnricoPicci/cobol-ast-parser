# Implementation Plan: `paragraph-variables-map` Command

## Overview

Create a new CLI command that analyzes a COBOL source file and produces a paragraph/section-centric view showing which variables may change within each code unit, along with their Level 01 record membership (including REDEFINES base record resolution).

The command works similarly to `analyze-and-filter`: it takes a COBOL source file as input, performs the analysis internally, and outputs both the analysis JSON and the paragraph-variables map JSON.

## IMPORTANT: Accurate Byte-Level REDEFINES Overlap Detection

### The Problem

When a variable like `TRAN-CUSTOMER-ID` is modified in `TRANSACTION-RECORD`, and `PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD`, the system should only report variables in `PAYMENT-DETAIL` that **actually overlap in memory** with `TRAN-CUSTOMER-ID` based on byte position.

**Current (incorrect) behavior:** Lists ALL variables from the REDEFINES record as affected.

**Correct behavior:** Only list variables whose byte ranges overlap with the modified variable's byte range.

### Solution: Byte-Level Overlap Filtering

The infrastructure already exists in `DataStructureAnalyzer`:
- `MemoryRegion` class tracks `start_offset`, `size`, and `record_name` for each variable
- `get_memory_region(item_name)` returns the memory region for any variable

**Implementation approach:**

When a variable is modified:
1. Get its `MemoryRegion` (byte offset and size within its record)
2. For each potential affected variable from REDEFINES records:
   a. Get its `MemoryRegion` in its own record
   b. Since REDEFINES records share the same memory layout (byte 0 in RECORD-A = byte 0 in RECORD-B REDEFINES RECORD-A), compare byte ranges directly
   c. Only include the variable if its byte range overlaps with the modified variable's byte range
3. Set `overlap_type` accurately:
   - `"full"` - Variables have identical byte ranges
   - `"partial"` - Variables overlap but neither fully contains the other
   - `"contains"` - Modified variable's bytes fully contain the affected variable
   - `"contained_by"` - Affected variable's bytes fully contain the modified variable

### Example

```
TRANSACTION-RECORD (100 bytes total):
  TRAN-CUSTOMER-ID    PIC X(10)  offset 0-9
  TRAN-DATE           PIC 9(8)   offset 10-17
  TRAN-AMOUNT         PIC 9(7)V99 offset 18-26
  ...

PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD:
  PAY-METHOD          PIC X(2)   offset 0-1     <- overlaps with TRAN-CUSTOMER-ID
  PAY-REFERENCE       PIC X(8)   offset 2-9     <- overlaps with TRAN-CUSTOMER-ID
  PAY-AUTH-CODE       PIC X(6)   offset 10-15   <- does NOT overlap
  PAY-BANK-CODE       PIC X(4)   offset 16-19   <- does NOT overlap
  ...
```

When `TRAN-CUSTOMER-ID` (offset 0-9) is modified:
- `PAY-METHOD` (offset 0-1) - **overlaps** (contained_by)
- `PAY-REFERENCE` (offset 2-9) - **overlaps** (contained_by)
- `PAY-AUTH-CODE` (offset 10-15) - **does NOT overlap** (starts at 10, modified ends at 9)
- `PAY-BANK-CODE` (offset 16-19) - **does NOT overlap**

### Files to Modify for Byte-Level Filtering

| File | Changes |
|------|---------|
| `src/analyzers/impact_analyzer.py` | Filter `affected_variables` by actual byte overlap |
| `src/analyzers/data_analyzer.py` | Add method to check cross-record byte overlap |

---

## Output JSON Structure

```json
{
  "program_name": "TRANPROC",
  "analysis_date": "2026-01-25T10:00:00Z",
  "execution_time_seconds": 0.0025,
  "paragraphs": {
    "3100-APPLY-PAYMENT": {
      "CUST-BALANCE": {
        "defined_in_record": "CUSTOMER-RECORD",
        "base_record": "CUSTOMER-RECORD"
      },
      "PAY-CASH": {
        "defined_in_record": "PAYMENT-DETAIL",
        "base_record": "TRANSACTION-RECORD"
      },
      "WS-STANDALONE": {
        "defined_in_record": "WS-STANDALONE",
        "base_record": "WS-STANDALONE",
        "77-level-var": true
      }
    }
  },
  "summary": {
    "total_paragraphs_with_changes": 15,
    "total_unique_variables": 42,
    "variables_in_redefines_records": 8,
    "variables_via_ancestor_modification": 5,
    "level_77_variables": 2
  }
}
```

## Files to Create/Modify

### New Files

| File | Purpose |
|------|---------|
| `src/output/paragraph_variables_map.py` | Core transformer class (~150 lines) |
| `tests/test_paragraph_variables_map.py` | Unit and CLI tests (~500 lines) |

### Modified Files

| File | Changes |
|------|---------|
| `src/main.py` | Add `create_paragraph_variables_map_parser()` and `handle_paragraph_variables_map()` |
| `src/output/__init__.py` | Add `ParagraphVariablesMapper` to exports |
| `src/analyzers/impact_analyzer.py` | Add byte-level overlap filtering for affected_variables |
| `src/analyzers/data_analyzer.py` | Add `get_overlapping_regions()` method for cross-record overlap |
| `docs/json-output-reference.md` | Add new output format documentation |

## Implementation Details

### 1. Byte-Level Overlap Filtering in `ImpactAnalyzer`

```python
def _analyze_modifications(self, modifications: List[VariableModification]) -> List[VariableImpact]:
    """Analyze a list of modifications to determine impact."""
    impacts = []

    for mod in modifications:
        record = self.data_analyzer.get_record_for_variable(mod.variable_name)

        if record:
            affected = self.redefines_analyzer.get_affected_records(record.name)
            affected_list = sorted(list(affected))

            # Get all potential affected variables through REDEFINES
            potential_affected = self.redefines_analyzer.get_overlapping_variables(mod.variable_name)

            # NEW: Filter by actual byte overlap
            affected_vars = self._filter_by_byte_overlap(mod.variable_name, potential_affected)
        else:
            affected_list = []
            affected_vars = []

        impact = VariableImpact(...)
        impacts.append(impact)

    return impacts

def _filter_by_byte_overlap(
    self,
    modified_var: str,
    potential_affected: List[AffectedVariable]
) -> List[AffectedVariable]:
    """Filter affected variables to only those with actual byte overlap.

    Args:
        modified_var: Name of the modified variable
        potential_affected: List of potentially affected variables from REDEFINES

    Returns:
        Filtered list with only variables that actually overlap in memory
    """
    modified_region = self.data_analyzer.get_memory_region(modified_var)
    if not modified_region:
        return potential_affected  # Can't filter without region info

    filtered = []
    for av in potential_affected:
        other_region = self.data_analyzer.get_memory_region(av.name)
        if not other_region:
            # Keep variable if we can't determine overlap
            filtered.append(av)
            continue

        # For REDEFINES records, byte positions are equivalent
        # (byte 0 in RECORD-A = byte 0 in RECORD-B REDEFINES RECORD-A)
        # So we compare offsets directly
        if self._regions_overlap(modified_region, other_region):
            # Calculate accurate overlap_type
            overlap_type = self._calculate_overlap_type(modified_region, other_region)
            filtered.append(AffectedVariable(
                name=av.name,
                overlap_type=overlap_type,
                redefines_chain=av.redefines_chain,
                redefines_level=av.redefines_level,
                redefining_ancestor=av.redefining_ancestor,
                redefined_ancestor=av.redefined_ancestor,
            ))

    return filtered

def _regions_overlap(self, r1: MemoryRegion, r2: MemoryRegion) -> bool:
    """Check if two memory regions overlap (ignoring record name for REDEFINES)."""
    return r1.start_offset < r2.end_offset and r2.start_offset < r1.end_offset

def _calculate_overlap_type(self, modified: MemoryRegion, affected: MemoryRegion) -> str:
    """Calculate the type of overlap between two regions."""
    if modified.start_offset == affected.start_offset and modified.size == affected.size:
        return "full"
    elif modified.start_offset <= affected.start_offset and modified.end_offset >= affected.end_offset:
        return "contains"
    elif affected.start_offset <= modified.start_offset and affected.end_offset >= modified.end_offset:
        return "contained_by"
    else:
        return "partial"
```

### 2. `src/output/paragraph_variables_map.py`

```python
class ParagraphVariablesMapper:
    """Transforms analysis to paragraph -> changed variables map."""

    def __init__(self, analysis_data: Dict[str, Any]):
        self.analysis_data = analysis_data
        self._data_hierarchy = analysis_data.get("data_hierarchy", {})
        self._redefines_graph = self._build_redefines_graph()

    def map(self, include_redefines=True, include_ancestor_mods=True) -> Dict:
        """Main entry point - returns the output dictionary."""
```

**Key Methods:**

1. `_build_redefines_graph()` - Parse `redefines_chain` strings from `affected_variables` to build `{redefining_record: redefined_record}` mapping

2. `_get_base_record(record_name)` - Follow REDEFINES chain to find ultimate non-REDEFINE root (with cycle detection)

3. `_get_record_for_variable(var_name)` - Use `data_hierarchy[var_name][0]` to get the 01 level

4. `_is_77_level(var_name)` - Check if `data_hierarchy[var_name]` has length 1 (only the var itself)

5. `_get_children_of_group(group_name)` - Find all variables where group appears in their hierarchy (for ancestor modifications)

6. `_collect_changed_variables(section_name)` - Collect from three sources:
   - Direct: `mod["variable"]`
   - REDEFINES: `mod["affected_variables"][*]["name"]`
   - Ancestor: children of modified groups

### 3. CLI Integration in `src/main.py`

**Parser:**
```python
def create_paragraph_variables_map_parser(subparsers):
    parser = subparsers.add_parser(
        "paragraph-variables-map",
        help="Analyze COBOL source and map paragraphs to changed variables"
    )
    # Required: COBOL source file
    parser.add_argument("source", type=Path, help="Path to the COBOL source file")

    # Output options
    parser.add_argument("-o", "--output-dir", type=Path)
    parser.add_argument("--analysis-filename", default="{program_name}-analysis.json")
    parser.add_argument("--output-filename", default="{program_name}-paragraph-variables.json")
    parser.add_argument("--no-redefines", action="store_true")
    parser.add_argument("--no-ancestor-mods", action="store_true")

    # Copybook options (same as analyze command)
    parser.add_argument("-c", "--copybook-path", type=Path, action="append", dest="copybook_paths")
    parser.add_argument("--no-copy-resolution", action="store_true")

    # Configuration
    parser.add_argument("--config", type=Path)
    parser.add_argument("--include-source-info", action="store_true")

    # Logging
    parser.add_argument("-v", "--verbose", action="store_true")
    parser.add_argument("-q", "--quiet", action="store_true")

    parser.set_defaults(func=handle_paragraph_variables_map)
```

**Handler pattern:** Follow `handle_analyze_and_filter()`:
1. Validate COBOL source file exists
2. Call `analyze_cobol_file()` to perform analysis
3. Optionally write analysis JSON to output directory
4. Create `ParagraphVariablesMapper` with analysis output
5. Call `mapper.map()` to generate paragraph-variables output
6. Write paragraph-variables JSON to output directory or stdout

## Test Strategy

### Unit Tests (`test_paragraph_variables_map.py`)

1. **Basic mapping** - direct modifications appear in output
2. **REDEFINES resolution** - `defined_in_record` vs `base_record` differ correctly
3. **REDEFINES chain** - A REDEFINES B REDEFINES C resolves to C
4. **77-level variables** - both properties equal var name, `77-level-var: true` present
5. **Ancestor modifications** - children of INITIALIZEd groups appear
6. **`--no-redefines` flag** - excludes REDEFINES-affected variables
7. **`--no-ancestor-mods` flag** - excludes ancestor-modified variables
8. **Empty paragraphs** - paragraphs with no changes excluded from output
9. **Case insensitivity** - uppercase normalization works

### Byte-Level Overlap Tests

1. **Partial overlap** - Modified variable partially overlaps affected variable
2. **Full overlap** - Modified variable has same byte range as affected variable
3. **Contains** - Modified variable fully contains affected variable
4. **Contained_by** - Affected variable fully contains modified variable
5. **No overlap** - Variables in same REDEFINES group but different byte ranges are excluded

### CLI Integration Tests

1. **Basic command from source** - `paragraph-variables-map source.cob` succeeds
2. **Output to directory** - produces both analysis.json and paragraph-variables.json
3. **Custom filenames** - `--analysis-filename` and `--output-filename` work
4. **Missing source file** - proper error handling
5. **Copybook resolution** - `-c copybooks/` works
6. **No copybook resolution** - `--no-copy-resolution` works
7. **Stdout output** - without `-o`, outputs paragraph-variables JSON to stdout
8. **Flags** - `--no-redefines` and `--no-ancestor-mods` work from CLI

## Verification

After implementation, verify with:

```bash
# Run tests
pytest tests/test_paragraph_variables_map.py -v
pytest tests/test_impact_analyzer.py -v -k "byte_overlap"

# Test with sample program (analyzes and maps in one step)
python -m src paragraph-variables-map tests/fixtures/simple_program.cob -o ./output

# Verify both files are created
ls ./output/
# SIMPLE-PROGRAM-analysis.json
# SIMPLE-PROGRAM-paragraph-variables.json

# Test with TRANPROC - verify byte-level filtering
python -m src paragraph-variables-map complex-cobol-source/TRANPROC.cbl \
    -c complex-cobol-source/copybooks -o ./output

# Check that TRAN-CUSTOMER-ID modification only lists overlapping variables
cat output/TRANPROC-analysis.json | python -c "
import json,sys
d=json.load(sys.stdin)
for section, mods in d['sections_and_paragraphs'].items():
    for m in mods:
        if m.get('variable')=='TRAN-CUSTOMER-ID':
            print(f'Section: {section}')
            print('Affected variables:', [av['name'] for av in m.get('affected_variables', [])])
"
```

## Edge Cases

1. **Circular REDEFINES** - detected via `visited` set, stops at cycle
2. **Missing data_hierarchy** - graceful fallback, variables without hierarchy skipped
3. **Variable not in hierarchy** - skip (external/undefined)
4. **Same variable in multiple paragraphs** - appears in each paragraph's entry
5. **No modifications in paragraph** - paragraph excluded from output
6. **Parse errors** - report error and exit with code 1
7. **Missing copybooks** - warning logged but continues
8. **Missing memory region info** - include variable with original overlap_type (fallback)
