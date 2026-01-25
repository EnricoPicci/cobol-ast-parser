# Variable Filter Feature - Implementation Strategy

## Executive Summary

This document outlines the strategy for extending the COBOL Analyzer CLI with a new command that filters analysis output by variable names. The feature transforms the existing section/paragraph-centric output into a variable-centric view, enabling users to quickly identify where specific variables are modified throughout a COBOL program.

---

## 1. Feature Requirements

### 1.1 Business Need

The current analyzer produces JSON grouped by SECTION/PARAGRAPH, listing all variable modifications within each. Users often need the inverse view: given a set of variable names, find all locations where those variables are modified (directly or via REDEFINES relationships).

### 1.2 Functional Requirements

1. **Two CLI Commands:**
   - `analyze` - Current functionality (analyze COBOL source, produce section-grouped output)
   - `filter-by-variable` - New functionality (transform analysis output to variable-grouped view)

2. **Variable Filtering Logic:**
   - Accept a list of variable names as input
   - Find all PARAGRAPHs/SECTIONs where each variable is modified
   - Include indirect modifications via REDEFINES (variable appears in `affected_variables`)

3. **Backwards Compatibility:**
   - Existing CLI usage (`cobol-analyzer source.cob ...`) must continue working

---

## 2. CLI Design

### 2.1 Command Structure

```
cobol-analyzer analyze <source.cob> [options]              # Current functionality
cobol-analyzer filter-by-variable <json-input> -v VAR1 VAR2 [options]  # New feature
```

### 2.2 Backwards Compatibility

When no subcommand is provided, default to `analyze`:
```bash
# These are equivalent:
cobol-analyzer source.cob -o ./output
cobol-analyzer analyze source.cob -o ./output
```

### 2.3 Command Arguments

**`analyze` Command** (existing, unchanged):
```
cobol-analyzer analyze <source> [options]

Positional:
  source              COBOL source file to analyze

Options:
  -o, --output-dir    Output directory
  --output-filename   Output filename pattern (default: {program_name}-analysis.json)
  --compact           Deduplicate variables per section
  --summary-only      Output only summary statistics
  --include-source-info  Include source file metadata
  -c, --copybook-path Path to search for copybooks (repeatable)
  --no-copy-resolution Skip COPY statement resolution
  --config            Path to YAML configuration
  -v, --verbose       Enable debug logging
  -q, --quiet         Suppress output except errors
```

**`filter-by-variable` Command** (new):
```
cobol-analyzer filter-by-variable <input-json> -v VAR1 [VAR2 ...] [options]

Positional:
  input-json          JSON output from 'analyze' command

Required (one of):
  -v, --variables     Variable names to filter (space-separated)
  --variables-file    File containing variable names (one per line)

Options:
  -o, --output-dir    Output directory (default: stdout)
  --output-filename   Output filename (default: {program_name}-variable-filter.json)
  --no-redefines      Exclude REDEFINES-related modifications
  -q, --quiet         Suppress output except errors
```

---

## 3. Output Format Specification

### 3.1 Variable-Centric JSON Structure

```json
{
  "program_name": "EXAMPLE-PROGRAM",
  "analysis_date": "2026-01-25T10:30:00Z",
  "execution_time_seconds": 0.0012,
  "filter_variables": ["WS-TOTAL", "CUST-BALANCE"],
  "variables": {
    "WS-TOTAL": {
      "direct_modifications": [
        {
          "section_or_paragraph": "3000-CALCULATE-TOTALS",
          "modification_type": "ADD",
          "line_number": 145,
          "affected_records": ["WS-TOTALS"]
        }
      ],
      "redefines_modifications": []
    },
    "CUST-BALANCE": {
      "direct_modifications": [
        {
          "section_or_paragraph": "4200-UPDATE-BALANCE",
          "modification_type": "COMPUTE",
          "line_number": 210,
          "affected_records": ["CUSTOMER-RECORD"]
        }
      ],
      "redefines_modifications": [
        {
          "section_or_paragraph": "5000-PROCESS-OVERLAY",
          "modification_type": "MOVE",
          "line_number": 285,
          "affected_records": ["CUSTOMER-OVERLAY"],
          "modified_variable": "CUST-OVERLAY-AMT",
          "overlap_type": "full",
          "redefines_chain": "CUSTOMER-OVERLAY REDEFINES CUSTOMER-RECORD"
        }
      ]
    }
  },
  "summary": {
    "variables_requested": 2,
    "variables_found": 2,
    "variables_not_found": [],
    "total_direct_modifications": 2,
    "total_redefines_modifications": 1
  }
}
```

### 3.2 Output Field Descriptions

| Field | Description |
|-------|-------------|
| `filter_variables` | Echo of input variables for traceability |
| `direct_modifications` | Locations where variable is directly modified |
| `redefines_modifications` | Locations where variable is indirectly modified via REDEFINES |
| `modified_variable` | The variable that was directly modified (for REDEFINES entries) |
| `overlap_type` | Memory overlap relationship (full, partial, contains, contained_by) |
| `redefines_chain` | Description of REDEFINES relationship |

---

## 4. Architecture

### 4.1 New Module: `VariableFilter`

**Location:** `/workspaces/cobol-ast-parser/src/output/variable_filter.py`

```python
from dataclasses import dataclass
from typing import List, Dict, Any, Optional
from datetime import datetime

@dataclass
class VariableModificationInfo:
    """Information about where a variable is modified."""
    section_or_paragraph: str
    modification_type: str
    line_number: int
    affected_records: List[str]
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

    def filter(
        self,
        variable_names: List[str],
        include_redefines: bool = True
    ) -> Dict[str, Any]:
        """
        Filter analysis by variable names.

        Args:
            variable_names: List of COBOL variable names to filter
            include_redefines: Include indirect modifications via REDEFINES

        Returns:
            Variable-centric output dictionary
        """
```

### 4.2 Integration with CLI

**Modifications to `/workspaces/cobol-ast-parser/src/main.py`:**

1. **Restructure argument parsing** to use subparsers
2. **Extract current logic** into `handle_analyze()` function
3. **Add new handler** `handle_filter_by_variable()` function
4. **Add backwards compatibility** shim for legacy CLI usage

### 4.3 Data Flow

```
                                  +-------------------+
                                  |  COBOL Source     |
                                  +--------+----------+
                                           |
                                           v
                              +------------------------+
                              |  analyze command       |
                              |  (existing pipeline)   |
                              +--------+---------------+
                                       |
                                       v
                              +------------------------+
                              |  Section-centric JSON  |
                              |  (existing output)     |
                              +--------+---------------+
                                       |
                                       v
                     +-----------------------------------+
                     |  filter-by-variable command       |
                     |  +-----------------------------+  |
                     |  |  VariableFilter.filter()    |  |
                     |  |  - Scan sections/paragraphs |  |
                     |  |  - Match variable names     |  |
                     |  |  - Check affected_variables |  |
                     |  +-----------------------------+  |
                     +--------+-------------------------+
                              |
                              v
                     +------------------------+
                     |  Variable-centric JSON |
                     |  (new output format)   |
                     +------------------------+
```

---

## 5. Implementation Algorithm

### 5.1 Variable Filter Logic

```python
def filter(self, variable_names: List[str], include_redefines: bool = True) -> dict:
    results = {}
    not_found = []

    for var_name in variable_names:
        var_upper = var_name.upper()  # COBOL is case-insensitive
        direct_mods = []
        redefines_mods = []

        # Scan all sections and paragraphs
        for section_name, modifications in self.analysis_data.get("sections_and_paragraphs", {}).items():
            for mod in modifications:
                # Check for direct modification
                if mod.get("variable", "").upper() == var_upper:
                    direct_mods.append({
                        "section_or_paragraph": section_name,
                        "modification_type": mod.get("modification_type"),
                        "line_number": mod.get("line_number"),
                        "affected_records": mod.get("affected_records", [])
                    })

                # Check for REDEFINES-related modification
                elif include_redefines:
                    for av in mod.get("affected_variables", []):
                        if av.get("name", "").upper() == var_upper:
                            redefines_mods.append({
                                "section_or_paragraph": section_name,
                                "modification_type": mod.get("modification_type"),
                                "line_number": mod.get("line_number"),
                                "affected_records": mod.get("affected_records", []),
                                "modified_variable": mod.get("variable"),
                                "overlap_type": av.get("overlap_type"),
                                "redefines_chain": av.get("redefines_chain")
                            })

        if direct_mods or redefines_mods:
            results[var_upper] = {
                "direct_modifications": direct_mods,
                "redefines_modifications": redefines_mods
            }
        else:
            not_found.append(var_upper)

    return self._build_output(variable_names, results, not_found)
```

---

## 6. Edge Cases and Error Handling

| Scenario | Handling |
|----------|----------|
| Variable not found in analysis | Include in `summary.variables_not_found`, log warning, continue |
| Empty variable list | Error: "At least one variable name required" |
| Invalid JSON input | Validate structure, error with helpful message |
| Case sensitivity | Normalize to uppercase (COBOL convention) |
| Variable in both direct and REDEFINES | Include in both sections |
| Same variable modified multiple times | Include each as separate entry |
| Missing `affected_variables` field | Skip REDEFINES check for that modification |

---

## 7. Testing Strategy

### 7.1 New Test File

**Location:** `/workspaces/cobol-ast-parser/tests/test_variable_filter.py`

### 7.2 Test Cases

1. **Basic filtering** - Single variable, direct modifications only
2. **Multiple variables** - Filter for 2+ variables simultaneously
3. **REDEFINES matching** - Variable appears in `affected_variables`
4. **Variable not found** - Verify correct summary and no crash
5. **Case insensitivity** - `ws-total` matches `WS-TOTAL`
6. **Empty sections** - Sections with no matching modifications
7. **No REDEFINES flag** - `--no-redefines` excludes indirect modifications
8. **CLI integration** - End-to-end test of new subcommand
9. **Backwards compatibility** - Old CLI syntax still works

---

## 8. Implementation Phases

### Phase 1: Core Filter Logic
- Create `src/output/variable_filter.py`
- Implement `VariableFilter` class
- Write unit tests

### Phase 2: CLI Restructure
- Modify `src/main.py` to use argparse subparsers
- Extract current logic to `handle_analyze()`
- Add backwards compatibility shim
- Update version to 1.1.0

### Phase 3: New Command Integration
- Implement `handle_filter_by_variable()`
- Connect filter logic to CLI
- Add `--variables-file` option

### Phase 4: Testing and Documentation
- Add integration tests
- Update CLI help text and examples
- Update README if needed

---

## 9. Files to Modify/Create

| File | Action | Purpose |
|------|--------|---------|
| `src/output/variable_filter.py` | CREATE | New VariableFilter class |
| `src/output/__init__.py` | MODIFY | Export VariableFilter |
| `src/main.py` | MODIFY | Add subparsers and new command handler |
| `tests/test_variable_filter.py` | CREATE | Unit and integration tests |

---

## 10. CLI Examples

```bash
# Analyze COBOL source (existing functionality)
cobol-analyzer analyze source.cob -o ./output

# Filter by variables
cobol-analyzer filter-by-variable ./output/PROGRAM-analysis.json -v WS-TOTAL CUST-BALANCE

# Filter with output to file
cobol-analyzer filter-by-variable ./output/PROGRAM-analysis.json -v WS-TOTAL -o ./filtered

# Filter from file containing variable names
cobol-analyzer filter-by-variable ./output/PROGRAM-analysis.json --variables-file vars.txt

# Exclude REDEFINES relationships
cobol-analyzer filter-by-variable ./output/PROGRAM-analysis.json -v WS-TOTAL --no-redefines

# Backwards compatible (no subcommand)
cobol-analyzer source.cob -o ./output  # Still works!
```

---

## 11. Future Enhancements

1. **`--analyze` convenience flag** - Run analysis and filter in one command
2. **Regex variable matching** - Filter by pattern (e.g., `WS-*`)
3. **Output format options** - CSV, table format for quick review
4. **Interactive mode** - Prompt for variables after showing available ones
