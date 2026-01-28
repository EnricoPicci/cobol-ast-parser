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

Result dataclass containing both JSON outputs.

| Field | Type | Description |
|-------|------|-------------|
| `program_name` | `str` | Name of the analyzed COBOL program |
| `analysis` | `dict` | Full analysis (same as `{program_name}-analysis.json`) |
| `paragraph_variables` | `dict` | Paragraph-variables map (same as `{program_name}-paragraph-variables.json`) |
| `execution_time_seconds` | `float` | Total processing time |
| `source_info` | `dict \| None` | Source file metadata (included by default, `None` if `include_source_info=False`) |

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

**Methods:**
- `to_dict()` - Convert to dictionary for JSON serialization

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
