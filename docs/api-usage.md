# Using the API from Another Python Project

This guide explains how to import and use the COBOL Paragraph Variables Mapper API programmatically from another Python project.

## Installation

### Option 1: Install as editable package (recommended for development)

```bash
# In your project's virtual environment
pip install -e /path/to/cobol-ast-parser
```

### Option 2: Install from git

```bash
pip install git+https://github.com/your-org/cobol-ast-parser.git
```

### Option 3: Add to requirements.txt

```text
# For local development
-e /path/to/cobol-ast-parser

# Or from git
git+https://github.com/your-org/cobol-ast-parser.git
```

## Quick Start

```python
from pathlib import Path
from src import analyze_paragraph_variables

# Analyze a COBOL file (all features enabled by default)
result = analyze_paragraph_variables(Path("program.cob"))

# Access the results
print(result.program_name)
print(result.analysis)             # Full analysis JSON
print(result.paragraph_variables)  # Paragraph-variables map JSON
print(result.source_info)          # Source file metadata (included by default)
```

## Default Behavior

When called without options, `analyze_paragraph_variables` uses these defaults:

| Option | Default | Meaning |
|--------|---------|---------|
| Copybook search path | Source file's directory | Automatically searches the COBOL file's folder for copybooks |
| `resolve_copies` | `True` | COPY statements are resolved |
| `include_redefines` | `True` | Variables affected via REDEFINES are included |
| `include_ancestor_mods` | `True` | Variables affected via ancestor group modifications are included |
| `include_source_info` | `True` | Source file metadata is included in the output |

This means the simplest call provides the most complete analysis:

## API Reference

### Imports

```python
from src import (
    # Paragraph-variables analysis
    analyze_paragraph_variables,  # Main function for paragraph-variables analysis
    AnalysisOptions,              # Configuration options for analysis
    AnalysisResult,               # Return type for analysis

    # DATA DIVISION tree
    get_data_division_tree,       # Main function for DATA DIVISION tree
    TreeOptions,                  # Configuration options for tree
    DataDivisionTree,             # Return type for tree
    DataItemNode,                 # Tree node for data items
    DataDivisionSection,          # Section grouping for tree

    # Combined API (recommended when you need both outputs)
    analyze_with_tree,            # Combined analysis and tree generation
    CombinedOptions,              # Configuration options for combined API
    CombinedResult,               # Return type for combined API

    # Copybook resolution
    resolve_copybooks,            # Resolve COPY statements without parsing
    CopybookResolutionOptions,    # Configuration options for resolution
    CopybookResolutionResult,     # Return type for resolution
    LineMapping,                  # Line mapping entry (re-exported from preprocessor)

    # Paragraph analysis API
    analyze_for_paragraphs,              # Full analysis scoped to specific paragraphs
    ParagraphAnalysisOptions,            # Configuration options
    ParagraphAnalysisResult,             # Return type

    # Procedure division analysis API
    analyze_procedure_division,          # PROCEDURE DIVISION structural analysis
    ProcedureDivisionOptions,            # Configuration options
    ProcedureDivisionResult,             # Return type

    # Exceptions
    AnalysisError,                # Exception type
)
from parser import ParseError     # Parse error exception
```

### `analyze_paragraph_variables(source_path, options?)`

Main entry point for analyzing COBOL programs.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `source_path` | `Path` | Yes | Path to the COBOL source file |
| `options` | `AnalysisOptions` | No | Configuration options (uses defaults if not provided) |

**Returns:** `AnalysisResult`

**Raises:**
- `FileNotFoundError` - If source file doesn't exist or path is a directory
- `ParseError` - If COBOL source cannot be parsed
- `AnalysisError` - If analysis fails for other reasons

### `AnalysisOptions`

Configuration dataclass for analysis options.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `copybook_paths` | `List[Path]` | `None` | Additional paths to search for copybooks (source file's directory is always searched) |
| `resolve_copies` | `bool` | `True` | Whether to resolve COPY statements |
| `include_redefines` | `bool` | `True` | Include REDEFINES-affected variables in output |
| `include_ancestor_mods` | `bool` | `True` | Include ancestor-modified variables in output |
| `include_source_info` | `bool` | `True` | Include source file metadata in output |

**Note:** The source file's directory is always searched for copybooks by default, even if `copybook_paths` is `None`.

### `AnalysisResult`

Result dataclass containing both JSON outputs and a linking index.

| Field | Type | Description |
|-------|------|-------------|
| `program_name` | `str` | Name of the analyzed COBOL program |
| `analysis` | `dict` | Full analysis (same as `{program_name}-analysis.json`) |
| `paragraph_variables` | `dict` | Paragraph-variables map (same as `{program_name}-paragraph-variables.json`) |
| `variable_index` | `dict` | Inverted index for linking `DataDivisionTree` nodes to paragraphs (see below) |
| `execution_time_seconds` | `float` | Total processing time |
| `source_info` | `dict \| None` | Source file metadata (included by default, `None` if `include_source_info=False`) |
| `warnings` | `list[str]` | Warning messages from preprocessing (e.g., copybooks not found). Empty if no warnings. |

#### `variable_index` Structure

The `variable_index` provides O(1) lookup from a `DataDivisionTree` node to the list of paragraphs that may modify or access that variable:

```python
{
    "RECORD-NAME": {
        "start:end": {
            "variable_name": "VAR-NAME",
            "modifying_paragraphs": ["PARA-1", "PARA-2", ...],
            "accessing_paragraphs": ["PARA-3", "PARA-4", ...]
        }
    }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `variable_name` | `str` | Name of the variable at this position |
| `modifying_paragraphs` | `list[str]` | Paragraphs/sections that may modify (write to) this variable |
| `accessing_paragraphs` | `list[str]` | Paragraphs/sections that may access (read from) this variable |

**Lookup pattern:**
```python
# Given a DataItemNode from DataDivisionTree
node = selected_data_item_node

# Build the lookup key
key = f"{node.position['start']}:{node.position['end']}"

# Find modifying and accessing paragraphs
entry = result.variable_index.get(node.defined_in_record, {}).get(key)
if entry:
    modifiers = entry["modifying_paragraphs"]
    accessors = entry["accessing_paragraphs"]
```

### JSON Output Structure

The `AnalysisResult` contains two JSON outputs that provide different views of the analysis:

#### `*-analysis.json` (Full Analysis)

The `result.analysis` dict contains the complete analysis with all technical details. The key fields are:

**`sections_and_paragraphs`**: Maps each section/paragraph name to a list of variable modifications that occur within it.

Each modification object contains:
- `variable`: Name of the modified variable
- `affected_records`: List of Level 01 record names this variable belongs to
- `modification_type`: The COBOL statement type (MOVE, COMPUTE, ADD, etc.)
- `line_number`: Source line number (after COPY expansion)
- `affected_variables` (optional): List of variables affected via REDEFINES memory overlap, each with:
  - `name`: Variable name
  - `overlap_type`: "full", "contains", or "contained_by"
  - `redefines_chain`: Human-readable REDEFINES relationship
  - `redefines_level`: COBOL level number of the REDEFINES

**`data_hierarchy`**: Maps each variable name to its hierarchy path as a list from the Level 01 record down to the variable itself.

```json
"MS01-NOTA": ["AREA-MESSAGGI", "MS01-AREA-INTERFACCIA", "FILLER$2", "MS01-NOTA"]
```

**`memory_regions`**: Maps each variable to its memory layout within its record.

Each entry contains:
- `start_offset`: Byte offset from the start of the record (0-based)
- `size`: Size in bytes
- `record_name`: Name of the Level 01 record containing this variable
- `definition_line`: Source line number where the variable is defined
- `level`: COBOL level number (01-49, 66, 77, 88)

#### `*-paragraph-variables.json` (Paragraph-Variables Map)

The `result.paragraph_variables` dict provides a simplified, consumer-friendly view:

| Field | Type | Description |
|-------|------|-------------|
| `program_name` | `str` | Name of the COBOL program |
| `analysis_date` | `str` | ISO timestamp of when analysis was performed |
| `execution_time_seconds` | `float` | Processing time |
| `paragraphs` | `dict` | Maps paragraph names to their modified variables. Each variable entry contains: `base_record`, `defined_in_record`, optional `position` (start/end), and `explanation` (human-readable reason for modification) |
| `summary` | `dict` | Statistics: `total_paragraphs_with_changes`, `total_unique_variables`, `variables_in_redefines_records`, `variables_via_ancestor_modification`, `level_77_variables` |
| `source_info` | `dict \| None` | Source file metadata (if enabled) |

This output is useful when you need:
- A simple map of "which variables change in which paragraph"
- Human-readable explanations of why variables are affected
- Summary statistics for reporting

---

### `get_data_division_tree(source_path, options?)`

Returns a hierarchical tree view of all data items in the COBOL DATA DIVISION.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `source_path` | `Path` | Yes | Path to the COBOL source file |
| `options` | `TreeOptions` | No | Configuration options (uses defaults if not provided) |

**Returns:** `DataDivisionTree`

**Raises:**
- `FileNotFoundError` - If source file doesn't exist or path is a directory
- `ParseError` - If COBOL source cannot be parsed
- `AnalysisError` - If tree generation fails for other reasons

### `TreeOptions`

Configuration dataclass for tree generation options.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `copybook_paths` | `List[Path]` | `None` | Additional paths to search for copybooks (source file's directory is always searched) |
| `resolve_copies` | `bool` | `True` | Whether to resolve COPY statements |
| `include_filler` | `bool` | `True` | Include FILLER items in output |
| `include_88_levels` | `bool` | `True` | Include 88-level condition names in output |
| `include_source_info` | `bool` | `True` | Include source file metadata in output |

### `DataDivisionTree`

Result dataclass containing the hierarchical tree structure.

| Field | Type | Description |
|-------|------|-------------|
| `program_name` | `str` | Name of the COBOL program |
| `sections` | `List[DataDivisionSection]` | DATA DIVISION sections with their records |
| `all_records` | `List[DataItemNode]` | Flat list of all Level 01 records |
| `summary` | `dict` | Statistics (total_records, total_items, group_items, etc.) |
| `execution_time_seconds` | `float` | Total processing time |
| `source_info` | `dict \| None` | Source file metadata (if `include_source_info=True`) |
| `warnings` | `list[str]` | Warning messages from preprocessing (e.g., copybooks not found). Empty if no warnings. |

**Methods:**
- `to_dict()` - Convert to dictionary for JSON serialization (includes `warnings` if non-empty)

### `DataDivisionSection`

Represents a DATA DIVISION section (e.g., WORKING-STORAGE, FILE, LINKAGE).

| Field | Type | Description |
|-------|------|-------------|
| `name` | `str` | Section name (e.g., "WORKING-STORAGE") |
| `records` | `List[DataItemNode]` | Level 01 record nodes in this section |

**Methods:**
- `to_dict()` - Convert to dictionary for JSON serialization

### `DataItemNode`

Represents a COBOL data item in the tree structure.

| Field | Type | Description |
|-------|------|-------------|
| `name` | `str` | Data item name |
| `level` | `int` | COBOL level number (01-49, 66, 77, 88) |
| `picture` | `str \| None` | PICTURE clause if present |
| `usage` | `str \| None` | USAGE clause if present |
| `value` | `str \| None` | VALUE clause if present |
| `occurs` | `int \| None` | OCCURS count if present |
| `occurs_depending_on` | `str \| None` | OCCURS DEPENDING ON variable name if present |
| `redefines` | `str \| None` | Name of item being redefined if REDEFINES clause present |
| `is_group` | `bool` | True if this is a group item |
| `is_filler` | `bool` | True if this is a FILLER item |
| `line_number` | `int` | Line number in source (after COPY expansion) |
| `copybook_source` | `str \| None` | Name of copybook if item came from COPY statement |
| `position` | `dict \| None` | Memory position info (`start`, `end`, `size`) |
| `children` | `List[DataItemNode]` | Child data items |

**Methods:**
- `to_dict()` - Convert to dictionary for JSON serialization

---

### `analyze_with_tree(source_path, options?)`

Combined API that produces both `DataDivisionTree` and `AnalysisResult` in a single pass, avoiding duplicate parsing and preprocessing. **Recommended when you need both outputs** for approximately 40-50% efficiency gain.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `source_path` | `Path` | Yes | Path to the COBOL source file |
| `options` | `CombinedOptions` | No | Configuration options (uses defaults if not provided) |

**Returns:** `CombinedResult`

**Raises:**
- `FileNotFoundError` - If source file doesn't exist or path is a directory
- `ParseError` - If COBOL source cannot be parsed
- `AnalysisError` - If combined analysis fails for other reasons

### `CombinedOptions`

Configuration dataclass that combines options for both analysis and tree generation.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `copybook_paths` | `List[Path]` | `None` | Additional paths to search for copybooks |
| `resolve_copies` | `bool` | `True` | Whether to resolve COPY statements |
| `include_redefines` | `bool` | `True` | Include REDEFINES-affected variables in analysis output |
| `include_ancestor_mods` | `bool` | `True` | Include ancestor-modified variables in analysis output |
| `include_source_info` | `bool` | `True` | Include source file metadata in both outputs |
| `include_filler` | `bool` | `True` | Include FILLER items in tree output |
| `include_88_levels` | `bool` | `True` | Include 88-level condition names in tree output |

**Methods:**
- `to_analysis_options()` - Convert to `AnalysisOptions`
- `to_tree_options()` - Convert to `TreeOptions`

### `CombinedResult`

Result dataclass containing both outputs from a single processing pass.

| Field | Type | Description |
|-------|------|-------------|
| `program_name` | `str` | Name of the COBOL program |
| `data_division_tree` | `DataDivisionTree` | Hierarchical tree view of DATA DIVISION |
| `analysis_result` | `AnalysisResult` | Paragraph variables analysis with `variable_index` |
| `execution_time_seconds` | `float` | Total processing time for both outputs |
| `warnings` | `list[str]` | Warning messages from preprocessing (e.g., copybooks not found). Empty if no warnings. |

---

### `resolve_copybooks(source_path, options?)`

Resolves all COPY statements and `EXEC SQL INCLUDE` directives in a COBOL source file and returns the expanded source text with a line mapping. No format detection, normalization, or parsing is performed — this is a lightweight alternative when you only need copybook expansion.

**Note:** `EXEC SQL INCLUDE <member> END-EXEC` directives (commonly used in DB2 COBOL programs for SQLCA, DCLGEN copybooks, host variables, etc.) are treated as functionally equivalent to `COPY` statements and resolved using the same copybook search paths. Resolution comment markers use `* EXEC SQL INCLUDE <name> resolved` to distinguish them from `* COPY <name> resolved` markers.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `source_path` | `Path` | Yes | Path to the COBOL source file |
| `options` | `CopybookResolutionOptions` | No | Configuration options (uses defaults if not provided) |

**Returns:** `CopybookResolutionResult`

**Raises:**
- `FileNotFoundError` - If source file doesn't exist or path is a directory
- `AnalysisError` - If resolution fails for other reasons

### `CopybookResolutionOptions`

Configuration dataclass for copybook resolution.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `copybook_paths` | `List[Path]` | `None` | Additional paths to search for copybooks (source file's directory is always searched) |

### `CopybookResolutionResult`

Result dataclass containing the resolved source and line mapping.

| Field | Type | Description |
|-------|------|-------------|
| `resolved_source` | `str` | COBOL source text with all COPY statements expanded |
| `line_mapping` | `Dict[int, LineMapping]` | Maps each line in the resolved source to its original location |
| `execution_time_seconds` | `float` | Time taken for resolution |
| `warnings` | `list[str]` | Warning messages (e.g., copybooks not found). Empty if no warnings. |

### `LineMapping`

Dataclass representing the origin of a single line in the resolved source. Re-exported from `preprocessor` for convenience.

| Field | Type | Description |
|-------|------|-------------|
| `expanded_line` | `int` | Line number in the expanded source |
| `original_line` | `int` | Line number in the original file |
| `source_file` | `str` | `"<main>"` for the main source, or copybook name |
| `is_copybook` | `bool` | `True` if this line came from a copybook |

---

## Usage Examples

### Basic Usage

```python
from pathlib import Path
from src import analyze_paragraph_variables

result = analyze_paragraph_variables(Path("myprogram.cob"))

print(f"Program: {result.program_name}")
print(f"Execution time: {result.execution_time_seconds:.4f}s")
```

### With Custom Options

```python
from pathlib import Path
from src import analyze_paragraph_variables, AnalysisOptions

# Add extra copybook paths (source directory is always searched by default)
options = AnalysisOptions(
    copybook_paths=[Path("./copybooks"), Path("./shared/copy")],
)

result = analyze_paragraph_variables(Path("program.cob"), options)

# Source info is included by default
print(result.source_info)
# Output: {'file_path': '/full/path/program.cob', 'file_name': 'program.cob',
#          'source_format': 'fixed', 'lines_count': 500}
```

### Disabling Default Features

```python
from pathlib import Path
from src import analyze_paragraph_variables, AnalysisOptions

# Disable REDEFINES tracking and source info
options = AnalysisOptions(
    include_redefines=False,
    include_ancestor_mods=False,
    include_source_info=False,
)

result = analyze_paragraph_variables(Path("program.cob"), options)

# Only direct modifications will be included
# result.source_info will be None
```

### Iterating Over Results

```python
result = analyze_paragraph_variables(Path("program.cob"))

# Iterate over paragraphs and their modified variables
for para_name, variables in result.paragraph_variables["paragraphs"].items():
    print(f"\n{para_name}:")
    for var_name, info in variables.items():
        print(f"  {var_name}")
        print(f"    Record: {info['base_record']}")
        print(f"    Reason: {info['explanation']}")
```

### Getting Summary Statistics

```python
result = analyze_paragraph_variables(Path("program.cob"))

summary = result.paragraph_variables["summary"]
print(f"Paragraphs with changes: {summary['total_paragraphs_with_changes']}")
print(f"Unique variables modified: {summary['total_unique_variables']}")
print(f"Variables in REDEFINES records: {summary['variables_in_redefines_records']}")
print(f"Variables via ancestor modification: {summary['variables_via_ancestor_modification']}")
print(f"Level 77 variables: {summary['level_77_variables']}")
```

### Working with the Full Analysis

```python
result = analyze_paragraph_variables(Path("program.cob"))

# Access data hierarchy
for var_name, hierarchy in result.analysis["data_hierarchy"].items():
    print(f"{var_name}: {' > '.join(hierarchy)}")

# Access memory regions
for var_name, region in result.analysis["memory_regions"].items():
    print(f"{var_name}: offset={region['start_offset']}, size={region['size']}")

# Access modifications by section/paragraph
for section, mods in result.analysis["sections_and_paragraphs"].items():
    print(f"{section}: {len(mods)} modifications")
```

### Error Handling

```python
from pathlib import Path
from src import analyze_paragraph_variables, AnalysisError
from parser import ParseError

def analyze_safely(source_path: Path) -> dict | None:
    """Analyze a COBOL file with proper error handling."""
    try:
        result = analyze_paragraph_variables(source_path)
        return result.paragraph_variables

    except FileNotFoundError as e:
        print(f"File error: {e}")
        return None

    except ParseError as e:
        print(f"COBOL syntax error: {e}")
        return None

    except AnalysisError as e:
        print(f"Analysis failed: {e}")
        return None
```

### Handling Warnings

When copybooks cannot be found, analysis continues but warnings are generated. The `warnings` field on result objects contains these messages:

```python
from pathlib import Path
from src import analyze_paragraph_variables, AnalysisOptions

result = analyze_paragraph_variables(
    Path("program.cob"),
    AnalysisOptions(copybook_paths=[Path("./copybooks")])
)

# Check for warnings
if result.warnings:
    print("Warnings during analysis:")
    for warning in result.warnings:
        print(f"  - {warning}")
else:
    print("Analysis completed with no warnings")

# Warnings contain details about missing copybooks
# Example warning: "Copybook 'MYCOPY' not found. Searched: /path1, /path2"
```

**Note:** When a copybook is not found, the corresponding COPY statement is left as-is in the source (not expanded). Variables defined in missing copybooks will not be included in the analysis.

### Writing Results to Files

```python
import json
from pathlib import Path
from src import analyze_paragraph_variables

result = analyze_paragraph_variables(Path("program.cob"))

# Create output directory
output_dir = Path("./output")
output_dir.mkdir(exist_ok=True)

# Write both JSON outputs (same as CLI does)
analysis_path = output_dir / f"{result.program_name}-analysis.json"
analysis_path.write_text(json.dumps(result.analysis, indent=2))

paragraph_vars_path = output_dir / f"{result.program_name}-paragraph-variables.json"
paragraph_vars_path.write_text(json.dumps(result.paragraph_variables, indent=2))

print(f"Analysis written to: {analysis_path}")
print(f"Paragraph variables written to: {paragraph_vars_path}")
```

### Batch Processing Multiple Files

```python
from pathlib import Path
from src import analyze_paragraph_variables
from parser import ParseError

def analyze_directory(source_dir: Path, output_dir: Path):
    """Analyze all COBOL files in a directory."""
    import json

    output_dir.mkdir(exist_ok=True)

    results = []
    for cob_file in source_dir.glob("*.cob"):
        try:
            result = analyze_paragraph_variables(cob_file)

            # Write individual output
            out_path = output_dir / f"{result.program_name}-paragraph-variables.json"
            out_path.write_text(json.dumps(result.paragraph_variables, indent=2))

            results.append({
                "file": cob_file.name,
                "program": result.program_name,
                "status": "success",
                "paragraphs": result.paragraph_variables["summary"]["total_paragraphs_with_changes"],
                "variables": result.paragraph_variables["summary"]["total_unique_variables"],
            })

        except (ParseError, Exception) as e:
            results.append({
                "file": cob_file.name,
                "status": "error",
                "error": str(e),
            })

    # Write summary
    summary_path = output_dir / "batch-summary.json"
    summary_path.write_text(json.dumps(results, indent=2))

    return results

# Usage
results = analyze_directory(Path("./cobol-sources"), Path("./output"))
for r in results:
    status = "OK" if r["status"] == "success" else "FAILED"
    print(f"{r['file']}: {status}")
```

---

## DATA DIVISION Tree Examples

### Basic Tree Usage

```python
from pathlib import Path
from src import get_data_division_tree

tree = get_data_division_tree(Path("myprogram.cob"))

print(f"Program: {tree.program_name}")
print(f"Records: {tree.summary['total_records']}")
print(f"Total items: {tree.summary['total_items']}")
```

### With Custom Options

```python
from pathlib import Path
from src import get_data_division_tree, TreeOptions

# Exclude FILLER items and 88-level condition names
options = TreeOptions(
    copybook_paths=[Path("./copybooks")],
    include_filler=False,
    include_88_levels=False,
)

tree = get_data_division_tree(Path("program.cob"), options)
```

### Traversing the Tree

```python
from src import get_data_division_tree, DataItemNode

def print_tree(node: DataItemNode, indent: int = 0):
    """Recursively print the tree structure."""
    prefix = "  " * indent
    level_str = f"{node.level:02d}"
    pic_str = f" PIC {node.picture}" if node.picture else ""
    print(f"{prefix}{level_str} {node.name}{pic_str}")
    for child in node.children:
        print_tree(child, indent + 1)

tree = get_data_division_tree(Path("program.cob"))

for record in tree.all_records:
    print_tree(record)
    print()
```

### Filtering by Section

```python
tree = get_data_division_tree(Path("program.cob"))

# Get only WORKING-STORAGE records
for section in tree.sections:
    if section.name == "WORKING-STORAGE":
        for record in section.records:
            print(f"  {record.name}: {record.position}")
```

### Finding Variables from Copybooks

```python
from src import get_data_division_tree, TreeOptions, DataItemNode

def find_copybook_items(node: DataItemNode, items: list):
    """Find all items that came from copybooks."""
    if node.copybook_source:
        items.append((node.name, node.copybook_source))
    for child in node.children:
        find_copybook_items(child, items)

tree = get_data_division_tree(
    Path("program.cob"),
    TreeOptions(copybook_paths=[Path("./copybooks")])
)

copybook_items = []
for record in tree.all_records:
    find_copybook_items(record, copybook_items)

for name, source in copybook_items:
    print(f"{name} from {source}")
```

### JSON Serialization

```python
import json
from pathlib import Path
from src import get_data_division_tree

tree = get_data_division_tree(Path("program.cob"))

# Convert to dict and serialize
tree_dict = tree.to_dict()
json_output = json.dumps(tree_dict, indent=2)

# Write to file
Path("data-division-tree.json").write_text(json_output)
```

---

## Combined API Examples

The `analyze_with_tree()` function is the recommended approach when you need both `DataDivisionTree` and `AnalysisResult`. It performs all parsing and preprocessing only once, providing approximately 40-50% efficiency gain over calling both APIs separately.

### Basic Combined Usage

```python
from pathlib import Path
from src import analyze_with_tree

# Get both outputs in a single pass
result = analyze_with_tree(Path("program.cob"))

# Access both outputs
print(f"Program: {result.program_name}")
print(f"Records: {result.data_division_tree.summary['total_records']}")
print(f"Paragraphs: {len(result.analysis_result.paragraph_variables['paragraphs'])}")
print(f"Total time: {result.execution_time_seconds:.4f}s")
```

### With Custom Options

```python
from pathlib import Path
from src import analyze_with_tree, CombinedOptions

options = CombinedOptions(
    copybook_paths=[Path("./copybooks")],
    include_filler=False,        # Exclude FILLER from tree
    include_88_levels=False,     # Exclude 88-levels from tree
    include_redefines=True,      # Include REDEFINES in analysis
)

result = analyze_with_tree(Path("program.cob"), options)
```

### When to Use Combined API vs Separate APIs

| Scenario | Recommendation |
|----------|----------------|
| Need both tree and analysis | Use `analyze_with_tree()` |
| Need only DATA DIVISION structure | Use `get_data_division_tree()` |
| Need only paragraph-variables | Use `analyze_paragraph_variables()` |
| Performance-critical batch processing | Use `analyze_with_tree()` |

### Complete Workflow with Combined API

```python
import json
from pathlib import Path
from src import analyze_with_tree, CombinedOptions, DataItemNode

# Configure options
options = CombinedOptions(
    copybook_paths=[Path("./copybooks")],
)

# Single pass for both outputs
result = analyze_with_tree(Path("program.cob"), options)

# Use the tree for display/navigation
def print_tree(node: DataItemNode, indent: int = 0):
    prefix = "  " * indent
    print(f"{prefix}{node.level:02d} {node.name}")
    for child in node.children:
        print_tree(child, indent + 1)

for record in result.data_division_tree.all_records:
    print_tree(record)

# Use variable_index for linking
def find_paragraphs(node: DataItemNode) -> dict:
    if not node.defined_in_record or not node.position:
        return {"modifying": [], "accessing": []}
    key = f"{node.position['start']}:{node.position['end']}"
    entry = result.analysis_result.variable_index.get(node.defined_in_record, {}).get(key)
    if entry:
        return {
            "modifying": entry["modifying_paragraphs"],
            "accessing": entry["accessing_paragraphs"]
        }
    return {"modifying": [], "accessing": []}

# Write outputs
output_dir = Path("./output")
output_dir.mkdir(exist_ok=True)

Path(output_dir / "tree.json").write_text(
    json.dumps(result.data_division_tree.to_dict(), indent=2)
)
Path(output_dir / "analysis.json").write_text(
    json.dumps(result.analysis_result.analysis, indent=2)
)
Path(output_dir / "paragraph-variables.json").write_text(
    json.dumps(result.analysis_result.paragraph_variables, indent=2)
)
```

---

### `analyze_for_paragraphs(source_path, paragraph_names, options?)`

Analyzes a COBOL program scoped to specific paragraphs. This is a **superset** of `analyze_with_tree()`: it returns the full analysis result, the full DATA DIVISION tree, **and** a filtered view containing only the 01-level groups referenced by the specified paragraphs. Clients that currently call `analyze_with_tree()` and need paragraph-scoped filtering can switch to this single call.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `source_path` | `Path` | Yes | Path to the COBOL source file |
| `paragraph_names` | `List[str]` | Yes | Paragraph/section names to filter by (case-insensitive, handles SECTION suffix) |
| `options` | `ParagraphAnalysisOptions` | No | Configuration options (uses defaults if not provided) |

**Returns:** `ParagraphAnalysisResult`

**Raises:**
- `FileNotFoundError` - If source file doesn't exist or path is a directory
- `ParseError` - If COBOL source cannot be parsed
- `AnalysisError` - If analysis fails for other reasons

### `ParagraphAnalysisOptions`

Configuration dataclass combining analysis, tree, and filter options.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `copybook_paths` | `List[Path]` | `None` | Additional paths to search for copybooks (source file's directory is always searched) |
| `resolve_copies` | `bool` | `True` | Whether to resolve COPY statements |
| `include_filler` | `bool` | `True` | Include FILLER items in tree output |
| `include_88_levels` | `bool` | `True` | Include 88-level condition names in tree output |
| `include_redefines` | `bool` | `True` | Include REDEFINES-affected variables in analysis output |
| `include_ancestor_mods` | `bool` | `True` | Include ancestor-modified variables in analysis output |
| `include_source_info` | `bool` | `True` | Include source file metadata in output |
| `include_filtered_data` | `bool` | `True` | Compute filtered DATA DIVISION tree. When `False`, filtering is skipped and filter-related fields are returned empty/zeroed. |

### `ParagraphAnalysisResult`

Result dataclass containing full analysis, full tree, and filtered view.

| Field | Type | Description |
|-------|------|-------------|
| `program_name` | `str` | Name of the COBOL program |
| `data_division_tree` | `DataDivisionTree` | Full (unfiltered) DATA DIVISION tree |
| `analysis_result` | `AnalysisResult` | Full analysis result with `variable_index`, `paragraph_variables`, etc. |
| `filtered_sections` | `List[DataDivisionSection]` | Filtered sections (only groups with referenced variables) |
| `filtered_records` | `List[DataItemNode]` | Flat list of included Level 01 records after filtering |
| `filter_summary` | `dict` | Statistics: `total_records_before`, `total_records_after`, `records_removed`, `reduction_percentage` |
| `paragraph_names_used` | `List[str]` | Paragraph names that were actually matched |
| `execution_time_seconds` | `float` | Total processing time |
| `warnings` | `list[str]` | Warning messages from preprocessing |

**Methods:**
- `to_dict()` - Convert to dictionary for JSON serialization (includes full tree, analysis, and filtered data)
- `to_text()` - Render **filtered** DATA DIVISION as COBOL-like text for prompt inclusion

---

## Paragraph Analysis Examples

### Basic Usage

```python
from pathlib import Path
from src import analyze_for_paragraphs, ParagraphAnalysisOptions

result = analyze_for_paragraphs(
    source_path=Path("program.cob"),
    paragraph_names=["MAIN", "INIT-PARA", "PROCESS-PARA"],
    options=ParagraphAnalysisOptions(
        copybook_paths=[Path("./copybooks")],
    ),
)

# Filtered DATA DIVISION text for LLM prompts
filtered_text = result.to_text()

# Full analysis data
variable_index = result.analysis_result.variable_index
paragraph_variables = result.analysis_result.paragraph_variables

# Full (unfiltered) tree
all_records = result.data_division_tree.all_records

# Filtering statistics
print(f"Reduced from {result.filter_summary['total_records_before']} to "
      f"{result.filter_summary['total_records_after']} records "
      f"({result.filter_summary['reduction_percentage']}% reduction)")
```

### Skipping Filtered Data

When you only need the full tree and analysis (equivalent to `analyze_with_tree()`) without the filtering overhead, set `include_filtered_data=False`:

```python
from pathlib import Path
from src import analyze_for_paragraphs, ParagraphAnalysisOptions

result = analyze_for_paragraphs(
    source_path=Path("program.cob"),
    paragraph_names=["MAIN", "INIT-PARA"],
    options=ParagraphAnalysisOptions(include_filtered_data=False),
)

# Full tree and analysis are still available
all_records = result.data_division_tree.all_records
variable_index = result.analysis_result.variable_index

# Filter-related fields are empty/zeroed
assert result.filtered_sections == []
assert result.filtered_records == []
assert result.to_text() == ""
```

### When to Use Which API

| Scenario | Recommendation |
|----------|----------------|
| Need filtered DATA DIVISION + full analysis (e.g., for LLM prompts) | Use `analyze_for_paragraphs()` |
| Need full tree and analysis without filtering | Use `analyze_for_paragraphs(include_filtered_data=False)` or `analyze_with_tree()` |
| Need the full unfiltered DATA DIVISION only | Use `get_data_division_tree()` |
| Need paragraph-variables analysis only | Use `analyze_paragraph_variables()` |
| Need PROCEDURE DIVISION structure (control flow, calls, conditions) | Use `analyze_procedure_division()` |

---

## Procedure Division Analysis

### `analyze_procedure_division(source_path, options?)`

Analyzes the PROCEDURE DIVISION of a COBOL source file and returns structural information including paragraph inventory, PERFORM/GO TO/CALL graphs, conditional branches, and field references. Designed for LLM agents that need a deterministic procedure skeleton.

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `source_path` | `Path` | Yes | Path to the COBOL source file |
| `options` | `ProcedureDivisionOptions` | No | Configuration options (uses defaults if not provided) |

**Returns:** `ProcedureDivisionResult`

**Raises:**
- `FileNotFoundError` - If source file doesn't exist or path is a directory
- `ParseError` - If COBOL source cannot be parsed
- `AnalysisError` - If analysis fails for other reasons

### `ProcedureDivisionOptions`

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `copybook_paths` | `List[Path]` | `None` | Additional paths to search for copybooks |
| `resolve_copies` | `bool` | `True` | Whether to resolve COPY statements |
| `include_source_info` | `bool` | `True` | Include source file metadata in output |

### `ProcedureDivisionResult`

| Field | Type | Description |
|-------|------|-------------|
| `program_name` | `str` | Name of the COBOL program |
| `paragraphs` | `list[dict]` | Ordered paragraph inventory (name, type, parent_section, line_start, line_end, line_count) |
| `perform_graph` | `dict` | Maps paragraph name to list of PERFORM targets (target, type, thru_target, thru_includes, condition, times, line) |
| `goto_graph` | `dict` | Maps paragraph name to list of GO TO targets (target, line, conditional, condition_text) |
| `call_graph` | `dict` | Maps paragraph name to list of CALL entries (target, line, using_fields, is_dynamic) |
| `conditions` | `dict` | Maps paragraph name to list of conditional branches (IF/EVALUATE with nesting) |
| `field_references` | `dict` | Maps paragraph name to field reference aggregation (reads, writes, conditions_tested) |
| `execution_time_seconds` | `float` | Total processing time |
| `source_info` | `dict \| None` | Source file metadata |
| `warnings` | `list[str]` | Warning messages from preprocessing |

**Methods:**
- `to_dict()` - Convert to JSON-serializable dictionary
- `to_text()` - Render as compact LLM-friendly text format
- `for_paragraphs(names)` - Return filtered result for specified paragraphs only (PERFORM targets preserved)

### Basic Usage

```python
from pathlib import Path
from src import analyze_procedure_division

result = analyze_procedure_division(Path("program.cob"))

# Compact text for LLM prompt inclusion
print(result.to_text())

# Access individual views
for para in result.paragraphs:
    print(f"{para['name']} lines {para['line_start']}-{para['line_end']}")

# Check PERFORM graph
for para_name, performs in result.perform_graph.items():
    for p in performs:
        print(f"{para_name} -> {p['target']} ({p['type']})")
```

### With Copybooks

```python
from pathlib import Path
from src import analyze_procedure_division, ProcedureDivisionOptions

options = ProcedureDivisionOptions(
    copybook_paths=[Path("./copybooks")],
)
result = analyze_procedure_division(Path("program.cob"), options)
```

### Filtering to Specific Paragraphs

```python
result = analyze_procedure_division(Path("program.cob"))

# Get only MAIN-PARA and INIT-PARA views
filtered = result.for_paragraphs(["MAIN-PARA", "INIT-PARA"])
print(filtered.to_text())
```

### JSON Serialization

```python
import json
from pathlib import Path
from src import analyze_procedure_division

result = analyze_procedure_division(Path("program.cob"))
json_output = json.dumps(result.to_dict(), indent=2)
Path("procedure-division.json").write_text(json_output)
```

---

## Copybook Resolution Examples

### Basic Copybook Resolution

```python
from pathlib import Path
from src import resolve_copybooks

# Resolve COPY statements (source directory searched by default)
result = resolve_copybooks(Path("program.cob"))

print(result.resolved_source)  # Full source with COPYs expanded
print(f"Resolved in {result.execution_time_seconds:.4f}s")
```

### With Extra Search Paths

```python
from pathlib import Path
from src import resolve_copybooks, CopybookResolutionOptions

options = CopybookResolutionOptions(
    copybook_paths=[Path("./copybooks"), Path("./shared/copy")],
)

result = resolve_copybooks(Path("program.cob"), options)

# Check for warnings about missing copybooks
if result.warnings:
    for warning in result.warnings:
        print(f"Warning: {warning}")
```

### Inspecting Line Mapping

```python
from pathlib import Path
from src import resolve_copybooks, CopybookResolutionOptions

result = resolve_copybooks(
    Path("program.cob"),
    CopybookResolutionOptions(copybook_paths=[Path("./copybooks")]),
)

# Iterate over the line mapping
for line_num, mapping in result.line_mapping.items():
    origin = "copybook" if mapping.is_copybook else "main"
    print(f"Line {line_num}: from {mapping.source_file} "
          f"(original line {mapping.original_line}, {origin})")
```

### When to Use `resolve_copybooks` vs Other APIs

| Scenario | Recommendation |
|----------|----------------|
| Need only the expanded source text | Use `resolve_copybooks()` |
| Need DATA DIVISION structure | Use `get_data_division_tree()` |
| Need paragraph-variables analysis | Use `analyze_paragraph_variables()` |
| Need both tree and analysis | Use `analyze_with_tree()` |

`resolve_copybooks()` is the lightest-weight API — it only reads the source and resolves COPY statements without parsing or analysis.

---

## Linking DataDivisionTree to Paragraphs

A common use case is selecting a variable from the `DataDivisionTree` and finding which paragraphs may modify it. The `variable_index` in `AnalysisResult` enables this with O(1) lookup.

**Tip:** Use `analyze_with_tree()` when you need both outputs. It's more efficient than calling both APIs separately.

### Complete Linking Example

```python
from pathlib import Path
from src import analyze_with_tree, DataItemNode

# Use combined API for efficiency (recommended)
source_path = Path("program.cob")
combined = analyze_with_tree(source_path)

# Access both outputs
tree = combined.data_division_tree
variable_index = combined.analysis_result.variable_index

def find_modifying_paragraphs(node: DataItemNode) -> list[str]:
    """Find all paragraphs that may modify this variable."""
    if not node.defined_in_record or not node.position:
        return []

    # Build the lookup key using defined_in_record and position
    pos_key = f"{node.position['start']}:{node.position['end']}"

    # Look up in variable_index
    entry = variable_index.get(node.defined_in_record, {}).get(pos_key)
    return entry["modifying_paragraphs"] if entry else []

def find_accessing_paragraphs(node: DataItemNode) -> list[str]:
    """Find all paragraphs that may access (read) this variable."""
    if not node.defined_in_record or not node.position:
        return []

    pos_key = f"{node.position['start']}:{node.position['end']}"
    entry = variable_index.get(node.defined_in_record, {}).get(pos_key)
    return entry["accessing_paragraphs"] if entry else []

# Example: Find paragraphs that modify or access variables in WS-EMPLOYEE-RECORD
for record in tree.all_records:
    if record.name == "WS-EMPLOYEE-RECORD":
        for child in record.children:
            modifiers = find_modifying_paragraphs(child)
            accessors = find_accessing_paragraphs(child)
            if modifiers:
                print(f"{child.name}: modified in {', '.join(modifiers)}")
            if accessors:
                print(f"{child.name}: accessed in {', '.join(accessors)}")
```

### Why Use `defined_in_record`?

When two Level 01 records REDEFINE each other, they share the same memory but have different variables:

```cobol
01 CLIENT.
   05 CLIENT-NAME  PIC X(30).    *> Position 1-30

01 CLIENT-CHINA REDEFINES CLIENT.
   05 MIDDLE-NAME  PIC X(30).    *> Same position 1-30!
```

Using `defined_in_record` ensures you get only the explicit modifications to the variable you selected:

- `CLIENT-NAME` in `CLIENT` → paragraphs that modify `CLIENT-NAME`
- `MIDDLE-NAME` in `CLIENT-CHINA` → paragraphs that modify `MIDDLE-NAME`

Without `defined_in_record`, position alone would match both (since they share memory).

### Building a UI with Tree Selection

```python
from pathlib import Path
from src import analyze_with_tree

# Load both data structures in a single pass (recommended)
combined = analyze_with_tree(Path("program.cob"))
tree = combined.data_division_tree
variable_index = combined.analysis_result.variable_index

def on_variable_selected(node):
    """Called when user selects a variable in the DataDivisionTree UI."""
    if not node.position or not node.defined_in_record:
        return {"variable": node.name, "modifying_paragraphs": [], "accessing_paragraphs": [], "message": "No data"}

    key = f"{node.position['start']}:{node.position['end']}"
    entry = variable_index.get(node.defined_in_record, {}).get(key)

    if entry:
        return {
            "variable": entry["variable_name"],
            "record": node.defined_in_record,
            "position": f"{node.position['start']}-{node.position['end']}",
            "modifying_paragraphs": entry["modifying_paragraphs"],
            "accessing_paragraphs": entry["accessing_paragraphs"]
        }
    else:
        return {"variable": node.name, "modifying_paragraphs": [], "accessing_paragraphs": [], "message": "Not referenced"}
```

## Comparison: CLI vs API

| Feature | CLI | API |
|---------|-----|-----|
| Input | File path as argument | `Path` object |
| Output | JSON files written to disk | `AnalysisResult` object in memory |
| Both outputs | Written to separate files | Available as `result.analysis` and `result.paragraph_variables` |
| Error handling | Exit codes (0/1) | Exceptions (`FileNotFoundError`, `ParseError`, `AnalysisError`) |
| Configuration | Command-line flags | `AnalysisOptions` dataclass |

### Equivalent Operations

**CLI (with non-default options):**
```bash
cobol-analyzer paragraph-variables-map program.cob \
    -o ./output \
    -c ./copybooks \
    --no-redefines
```

**API (equivalent):**
```python
from pathlib import Path
from src import analyze_paragraph_variables, AnalysisOptions
import json

options = AnalysisOptions(
    copybook_paths=[Path("./copybooks")],
    include_redefines=False,
)

result = analyze_paragraph_variables(Path("program.cob"), options)

# Write outputs like CLI does
Path("./output").mkdir(exist_ok=True)
Path(f"./output/{result.program_name}-analysis.json").write_text(
    json.dumps(result.analysis, indent=2)
)
Path(f"./output/{result.program_name}-paragraph-variables.json").write_text(
    json.dumps(result.paragraph_variables, indent=2)
)
```

### Default Behavior (simplest usage)

**CLI:**
```bash
cobol-analyzer paragraph-variables-map program.cob -o ./output
```

**API:**
```python
from pathlib import Path
from src import analyze_paragraph_variables
import json

result = analyze_paragraph_variables(Path("program.cob"))

# All features enabled by default: REDEFINES, ancestor mods, source info
Path("./output").mkdir(exist_ok=True)
Path(f"./output/{result.program_name}-analysis.json").write_text(
    json.dumps(result.analysis, indent=2)
)
Path(f"./output/{result.program_name}-paragraph-variables.json").write_text(
    json.dumps(result.paragraph_variables, indent=2)
)
```

## See Also

- [CLI Reference](cli-reference.md) - Command-line interface documentation
- [JSON Output Reference](json-output-reference.md) - Structure of the JSON outputs
- [Quick Start](quickstart.md) - Getting started guide
