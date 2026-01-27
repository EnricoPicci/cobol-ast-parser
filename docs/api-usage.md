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

# Analyze a COBOL file
result = analyze_paragraph_variables(Path("program.cob"))

# Access the results
print(result.program_name)
print(result.analysis)             # Full analysis JSON
print(result.paragraph_variables)  # Paragraph-variables map JSON
```

## API Reference

### Imports

```python
from src import (
    analyze_paragraph_variables,  # Main function
    AnalysisOptions,              # Configuration options
    AnalysisResult,               # Return type
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
| `copybook_paths` | `List[Path]` | `None` | Additional paths to search for copybooks |
| `resolve_copies` | `bool` | `True` | Whether to resolve COPY statements |
| `include_redefines` | `bool` | `True` | Include REDEFINES-affected variables in output |
| `include_ancestor_mods` | `bool` | `True` | Include ancestor-modified variables in output |
| `include_source_info` | `bool` | `False` | Include source file metadata in output |

### `AnalysisResult`

Result dataclass containing both JSON outputs.

| Field | Type | Description |
|-------|------|-------------|
| `program_name` | `str` | Name of the analyzed COBOL program |
| `analysis` | `dict` | Full analysis (same as `{program_name}-analysis.json`) |
| `paragraph_variables` | `dict` | Paragraph-variables map (same as `{program_name}-paragraph-variables.json`) |
| `execution_time_seconds` | `float` | Total processing time |
| `source_info` | `dict \| None` | Source file metadata (if `include_source_info=True`) |

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

options = AnalysisOptions(
    copybook_paths=[Path("./copybooks"), Path("./shared/copy")],
    resolve_copies=True,
    include_redefines=True,
    include_ancestor_mods=True,
    include_source_info=True,
)

result = analyze_paragraph_variables(Path("program.cob"), options)

# Source info is now available
print(result.source_info)
# Output: {'file_path': '/full/path/program.cob', 'file_name': 'program.cob',
#          'source_format': 'fixed', 'lines_count': 500}
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
from src import analyze_paragraph_variables, AnalysisOptions
from parser import ParseError

def analyze_directory(source_dir: Path, output_dir: Path):
    """Analyze all COBOL files in a directory."""
    import json

    output_dir.mkdir(exist_ok=True)
    options = AnalysisOptions(include_source_info=True)

    results = []
    for cob_file in source_dir.glob("*.cob"):
        try:
            result = analyze_paragraph_variables(cob_file, options)

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

## Comparison: CLI vs API

| Feature | CLI | API |
|---------|-----|-----|
| Input | File path as argument | `Path` object |
| Output | JSON files written to disk | `AnalysisResult` object in memory |
| Both outputs | Written to separate files | Available as `result.analysis` and `result.paragraph_variables` |
| Error handling | Exit codes (0/1) | Exceptions (`FileNotFoundError`, `ParseError`, `AnalysisError`) |
| Configuration | Command-line flags | `AnalysisOptions` dataclass |

### Equivalent Operations

**CLI:**
```bash
cobol-analyzer paragraph-variables-map program.cob \
    -o ./output \
    -c ./copybooks \
    --include-source-info \
    --no-redefines
```

**API:**
```python
from pathlib import Path
from src import analyze_paragraph_variables, AnalysisOptions
import json

options = AnalysisOptions(
    copybook_paths=[Path("./copybooks")],
    include_source_info=True,
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

## See Also

- [CLI Reference](cli-reference.md) - Command-line interface documentation
- [JSON Output Reference](json-output-reference.md) - Structure of the JSON outputs
- [Quick Start](quickstart.md) - Getting started guide
