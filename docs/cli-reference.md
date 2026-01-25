# COBOL Analyzer - CLI Reference

Complete command-line interface reference for the COBOL Analyzer.

## Synopsis

```
cobol-analyzer <command> [OPTIONS]
```

Alternative invocation methods:

```bash
# Using Python module syntax
python -m src <command> [OPTIONS]

# Running directly from src directory
cd src && python main.py <command> [OPTIONS]
```

## Commands

The COBOL Analyzer provides three commands:

| Command | Description |
|---------|-------------|
| `analyze` | Analyze COBOL source file (default) |
| `filter-by-variable` | Filter analysis output by variable names |
| `analyze-and-filter` | Analyze and filter in one step (produces both JSON files) |

**Backwards Compatibility:** If no command is specified, `analyze` is assumed:
```bash
# These are equivalent:
cobol-analyzer source.cob -o ./output
cobol-analyzer analyze source.cob -o ./output
```

## Description

The COBOL Analyzer parses COBOL source files and generates JSON reports identifying:
- Variable modifications within SECTIONs and PARAGRAPHs
- Associated Level 01 record descriptions
- REDEFINES relationships and affected variables
- Execution timing metrics

---

## Command: `analyze`

Analyze a COBOL source file to identify variable modifications.

### Synopsis

```
cobol-analyzer analyze SOURCE [-o OUTPUT_DIR] [OPTIONS]
```

### Arguments

#### SOURCE (required)

Path to the COBOL source file to analyze.

```bash
cobol-analyzer analyze /path/to/program.cob
cobol-analyzer analyze some-cobol-source/COBCALC.cbl
```

Supported file extensions: `.cob`, `.cbl`, `.cobol`

### Options

#### Output Options

##### `-o, --output-dir PATH`

Output directory for analysis results. The directory will be created if it doesn't exist.

```bash
cobol-analyzer analyze program.cob -o ./output
cobol-analyzer analyze program.cob --output-dir /tmp/analysis
```

If not specified, output is written to stdout.

##### `--output-filename PATTERN`

Customize the output filename. Use `{program_name}` as a placeholder for the COBOL program name.

**Default:** `{program_name}-analysis.json`

```bash
# Default: creates MYPROGRAM-analysis.json
cobol-analyzer analyze MYPROGRAM.cob -o ./output

# Custom: creates MYPROGRAM-report.json
cobol-analyzer analyze MYPROGRAM.cob -o ./output --output-filename "{program_name}-report.json"

# Custom: creates analysis-MYPROGRAM.json
cobol-analyzer analyze MYPROGRAM.cob -o ./output --output-filename "analysis-{program_name}.json"
```

##### `--compact`

Generate compact output with deduplicated variables per section. Each variable appears only once per section/paragraph, with all affected records combined.

```bash
cobol-analyzer analyze program.cob -o ./output --compact
```

**Standard output:**
```json
{
  "INIT-SECTION": [
    {"variable": "WS-COUNT", "affected_records": ["WS-REC"], "line_number": 10, "modification_type": "MOVE"},
    {"variable": "WS-COUNT", "affected_records": ["WS-REC"], "line_number": 15, "modification_type": "ADD"}
  ]
}
```

**Compact output:**
```json
{
  "INIT-SECTION": [
    {"variable": "WS-COUNT", "affected_records": ["WS-REC"]}
  ]
}
```

##### `--summary-only`

Output only the summary section, excluding detailed modification data. Useful for quick statistics.

```bash
cobol-analyzer analyze program.cob -o ./output --summary-only
```

**Output:**
```json
{
  "analysis_date": "2026-01-24T12:00:00.000000",
  "program_name": "MYPROGRAM",
  "execution_time_seconds": 0.0234,
  "summary": {
    "total_sections": 3,
    "total_paragraphs": 12,
    "total_modifications": 45,
    "unique_modified_variables": 15,
    "records_with_redefines": ["REC-A", "REC-B"]
  }
}
```

##### `--include-source-info`

Include source file metadata in the output.

```bash
cobol-analyzer analyze program.cob -o ./output --include-source-info
```

**Added fields:**
```json
{
  "source_info": {
    "file_path": "/absolute/path/to/program.cob",
    "file_name": "program.cob",
    "source_format": "fixed",
    "lines_count": 500
  }
}
```

#### Copybook Options

##### `-c, --copybook-path PATH`

Add a path to search for copybooks (COPY statements). Can be specified multiple times.

```bash
# Single copybook path
cobol-analyzer analyze program.cob -o ./output -c ./copybooks

# Multiple copybook paths (searched in order)
cobol-analyzer analyze program.cob -o ./output -c ./copybooks -c ./shared/copy -c /usr/share/cobol/copy
```

The source file's directory is always searched first, before any specified paths.

##### `--no-copy-resolution`

Skip COPY statement resolution. Use this when:
- Copybooks are not available
- You want to analyze only the main source file
- Copybook resolution is causing errors

```bash
cobol-analyzer analyze program.cob -o ./output --no-copy-resolution
```

#### Configuration

##### `--config FILE`

Path to a YAML configuration file. See [Configuration](#configuration-file) section for details.

```bash
cobol-analyzer analyze program.cob -o ./output --config ./my-config.yaml
```

#### Logging Options

##### `-v, --verbose`

Enable verbose output with debug-level logging. Shows detailed information about:
- File reading
- Format detection
- COPY resolution
- Parsing progress
- AST building

```bash
cobol-analyzer analyze program.cob -o ./output -v
```

##### `-q, --quiet`

Suppress all output except errors. Useful for scripting and automation.

```bash
cobol-analyzer analyze program.cob -o ./output -q
```

**Note:** `--verbose` and `--quiet` cannot be used together.

---

## Command: `filter-by-variable`

Transform section-centric analysis output into a variable-centric view.

### Synopsis

```
cobol-analyzer filter-by-variable INPUT_JSON (-v VAR [VAR ...] | --variables-file FILE) [OPTIONS]
```

### Description

This command takes the JSON output from the `analyze` command and filters it to show only modifications related to specific variables. It produces a variable-centric view where you can see all locations where each variable is modified, including indirect modifications via REDEFINES relationships.

### Arguments

#### INPUT_JSON (required)

Path to the JSON output file from the `analyze` command.

```bash
cobol-analyzer filter-by-variable ./output/MYPROGRAM-analysis.json -v WS-TOTAL
```

### Variable Specification (one required)

#### `-v, --variables VAR [VAR ...]`

Variable names to filter (space-separated).

```bash
cobol-analyzer filter-by-variable analysis.json -v WS-TOTAL
cobol-analyzer filter-by-variable analysis.json -v WS-TOTAL CUST-BALANCE WS-COUNT
```

#### `--variables-file FILE`

Path to a file containing variable names (one per line).

```bash
cobol-analyzer filter-by-variable analysis.json --variables-file variables.txt
```

**variables.txt:**
```
WS-TOTAL
CUST-BALANCE
WS-COUNT
```

### Options

#### Output Options

##### `-o, --output-dir PATH`

Output directory for filtered results. If not specified, output is written to stdout.

```bash
cobol-analyzer filter-by-variable analysis.json -v WS-TOTAL -o ./filtered
```

##### `--output-filename PATTERN`

Customize the output filename. Use `{program_name}` as a placeholder.

**Default:** `{program_name}-variable-filter.json`

```bash
cobol-analyzer filter-by-variable analysis.json -v WS-TOTAL -o ./output --output-filename "{program_name}-filtered.json"
```

##### `--no-redefines`

Exclude REDEFINES-related modifications from the output. Only show direct variable modifications.

```bash
cobol-analyzer filter-by-variable analysis.json -v WS-TOTAL --no-redefines
```

##### `--no-ancestor-mods`

Exclude ancestor group modifications from the output. By default, when a parent group is modified (e.g., via INITIALIZE), it is shown as an ancestor modification for child variables.

```bash
cobol-analyzer filter-by-variable analysis.json -v CUST-ID --no-ancestor-mods
```

#### Logging Options

##### `-q, --quiet`

Suppress all output except errors.

```bash
cobol-analyzer filter-by-variable analysis.json -v WS-TOTAL -o ./output -q
```

### Output Format

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
      "redefines_modifications": [],
      "ancestor_modifications": []
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
      ],
      "ancestor_modifications": [
        {
          "section_or_paragraph": "1000-INIT-CUSTOMER",
          "modification_type": "INITIALIZE",
          "line_number": 50,
          "affected_records": ["CUSTOMER-RECORD"],
          "ancestor_variable": "CUST-DETAILS",
          "ancestor_level": 2
        }
      ]
    }
  },
  "summary": {
    "variables_requested": 2,
    "variables_found": 2,
    "variables_not_found": [],
    "total_direct_modifications": 2,
    "total_redefines_modifications": 1,
    "total_ancestor_modifications": 1
  }
}
```

### Output Field Descriptions

| Field | Description |
|-------|-------------|
| `filter_variables` | Echo of input variables for traceability |
| `direct_modifications` | Locations where variable is directly modified |
| `redefines_modifications` | Locations where variable is indirectly modified via REDEFINES |
| `ancestor_modifications` | Locations where a parent group of the variable is modified |
| `modified_variable` | The variable that was directly modified (for REDEFINES entries) |
| `overlap_type` | Memory overlap relationship (full, partial, contains, contained_by) |
| `redefines_chain` | Description of REDEFINES relationship |
| `ancestor_variable` | The ancestor group that was modified (for ancestor entries) |
| `ancestor_level` | Hierarchy depth of the ancestor (1 = root record, 2 = first child, etc.) |

---

## Command: `analyze-and-filter`

Analyze a COBOL source file and filter by variables in one step. Produces both the full analysis JSON and the variable-filtered JSON.

### Synopsis

```
cobol-analyzer analyze-and-filter SOURCE (-v VAR [VAR ...] | --variables-file FILE) [OPTIONS]
```

### Description

This command combines `analyze` and `filter-by-variable` into a single operation. It:
1. Analyzes the COBOL source file
2. Filters the results by the specified variables
3. Produces both output files (when `-o` is specified)

This is more efficient than running the two commands separately when you know which variables you want to filter for.

### Arguments

#### SOURCE (required)

Path to the COBOL source file to analyze.

```bash
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL -o ./output
```

### Variable Specification (one required)

#### `-v, --variables VAR [VAR ...]`

Variable names to filter (space-separated).

```bash
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL CUST-BALANCE
```

#### `--variables-file FILE`

Path to a file containing variable names (one per line).

```bash
cobol-analyzer analyze-and-filter program.cob --variables-file variables.txt
```

### Options

#### Output Options

##### `-o, --output-dir PATH`

Output directory for both JSON files. If not specified, only the filter result is written to stdout.

```bash
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL -o ./output
```

**Produces:**
- `{program_name}-analysis.json` - Full section-centric analysis
- `{program_name}-variable-filter.json` - Variable-centric filtered view

##### `--analysis-filename PATTERN`

Customize the analysis output filename.

**Default:** `{program_name}-analysis.json`

##### `--filter-filename PATTERN`

Customize the filter output filename.

**Default:** `{program_name}-variable-filter.json`

```bash
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL -o ./output \
  --analysis-filename "{program_name}-full.json" \
  --filter-filename "{program_name}-vars.json"
```

##### `--no-redefines`

Exclude REDEFINES-related modifications from the filter output.

```bash
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL --no-redefines
```

##### `--no-ancestor-mods`

Exclude ancestor group modifications from the filter output.

```bash
cobol-analyzer analyze-and-filter program.cob -v CUST-ID --no-ancestor-mods
```

##### `--include-source-info`

Include source file metadata in the analysis output.

```bash
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL --include-source-info
```

#### Copybook Options

##### `-c, --copybook-path PATH`

Add a path to search for copybooks. Can be specified multiple times.

```bash
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL -c ./copybooks -c ./shared
```

##### `--no-copy-resolution`

Skip COPY statement resolution.

#### Configuration

##### `--config FILE`

Path to a YAML configuration file.

#### Logging Options

##### `-V, --verbose`

Enable verbose output with debug-level logging.

**Note:** Uses `-V` (uppercase) to avoid conflict with `-v` for variables.

##### `-q, --quiet`

Suppress all output except errors.

### Output

When run with `-o`, prints a summary:
```
Analysis written to: ./output/MYPROGRAM-analysis.json
Filter output written to: ./output/MYPROGRAM-variable-filter.json
Analysis execution time: 0.0028 seconds
Filter execution time: 0.0001 seconds
Total execution time: 0.0029 seconds
Variables requested: 2
Variables found: 2
```

When run without `-o`, prints only the filter JSON to stdout.

---

## Information Options

### `--version`

Display version information and exit.

```bash
cobol-analyzer --version
# Output: cobol-analyzer 1.3.0
```

### `-h, --help`

Display help message and exit.

```bash
cobol-analyzer --help
cobol-analyzer analyze --help
cobol-analyzer filter-by-variable --help
cobol-analyzer analyze-and-filter --help
```

---

## Configuration File

The analyzer can be configured via a YAML file. Default configuration is in `config/settings.yaml`.

### Example Configuration

```yaml
# Copybook search paths
copybook_paths:
  - ./copybooks
  - ./copy
  - /usr/share/cobol/copybooks

# Recognized COBOL file extensions
cobol_extensions:
  - .cob
  - .cbl
  - .cobol
  - .cpy

# Parser settings
parser:
  max_copy_depth: 16          # Maximum nested COPY depth
  default_format: fixed        # fixed or free
  auto_detect_format: true     # Automatically detect source format

# Output settings
output:
  include_line_numbers: true
  include_modification_type: true
  pretty_print: true
  indent_size: 2

# Logging settings
logging:
  level: INFO    # DEBUG, INFO, WARNING, ERROR
```

---

## Analysis Output Format

### Full Output Structure

```json
{
  "analysis_date": "2026-01-24T12:00:00.000000",
  "program_name": "PROGRAM-ID",
  "execution_time_seconds": 0.0234,
  "source_info": {
    "file_path": "/path/to/source.cob",
    "file_name": "source.cob",
    "source_format": "fixed",
    "lines_count": 500
  },
  "sections_and_paragraphs": {
    "SECTION-OR-PARAGRAPH-NAME": [
      {
        "variable": "VARIABLE-NAME",
        "affected_records": ["RECORD-A", "RECORD-B"],
        "modification_type": "MOVE",
        "line_number": 123,
        "affected_variables": [
          {
            "name": "RELATED-VAR",
            "overlap_type": "full",
            "redefines_chain": "REC-B REDEFINES REC-A",
            "redefines_level": 1
          }
        ]
      }
    ]
  },
  "summary": {
    "total_sections": 5,
    "total_paragraphs": 20,
    "total_modifications": 100,
    "unique_modified_variables": 30,
    "records_with_redefines": ["REC-A", "REC-B"]
  }
}
```

### Modification Types

The analyzer recognizes the following modification types:

| Type | COBOL Statements |
|------|------------------|
| `MOVE` | MOVE, MOVE CORRESPONDING |
| `COMPUTE` | COMPUTE |
| `ADD` | ADD |
| `SUBTRACT` | SUBTRACT |
| `MULTIPLY` | MULTIPLY |
| `DIVIDE` | DIVIDE |
| `STRING` | STRING |
| `UNSTRING` | UNSTRING |
| `INSPECT` | INSPECT |
| `ACCEPT` | ACCEPT |
| `READ` | READ |
| `RETURN` | RETURN |
| `INITIALIZE` | INITIALIZE |
| `SET` | SET |
| `SEARCH` | SEARCH |

---

## Exit Codes

| Code | Description |
|------|-------------|
| 0 | Success |
| 1 | Error (parse error, file not found, etc.) |

---

## Examples

### Basic Analysis

```bash
# Analyze and output to directory
cobol-analyzer analyze program.cob -o ./output

# Analyze and print to stdout (backwards compatible)
cobol-analyzer program.cob
```

### Filter by Variables

```bash
# Filter existing analysis by variables
cobol-analyzer filter-by-variable ./output/PROGRAM-analysis.json -v WS-TOTAL CUST-BAL

# Filter and save to file
cobol-analyzer filter-by-variable ./output/PROGRAM-analysis.json -v WS-TOTAL -o ./filtered

# Filter using variables from file
cobol-analyzer filter-by-variable ./output/PROGRAM-analysis.json --variables-file vars.txt

# Filter without REDEFINES relationships
cobol-analyzer filter-by-variable ./output/PROGRAM-analysis.json -v WS-TOTAL --no-redefines
```

### Analyze and Filter Combined

```bash
# Analyze and filter in one step
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL CUST-BAL -o ./output

# With copybook paths
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL -c ./copybooks -o ./output

# Filter to stdout only
cobol-analyzer analyze-and-filter program.cob -v WS-TOTAL
```

### Production Usage

```bash
# Full analysis with all options
cobol-analyzer analyze /app/cobol/MAINPROG.cob \
  -o /app/output \
  -c /app/copybooks \
  -c /app/shared/copy \
  --include-source-info \
  --config /app/config/analyzer.yaml \
  -q
```

### CI/CD Pipeline

```bash
#!/bin/bash
# analyze-cobol.sh

SOURCE_DIR="/app/cobol"
OUTPUT_DIR="/app/reports"
VARS_FILE="/app/config/tracked-variables.txt"

for file in "$SOURCE_DIR"/*.cob; do
    # Analyze and filter for tracked variables
    cobol-analyzer analyze-and-filter "$file" \
      --variables-file "$VARS_FILE" \
      -o "$OUTPUT_DIR" \
      -q
done

echo "Analysis complete. Results in $OUTPUT_DIR"
```

### Quick Statistics

```bash
# Get only summary statistics
cobol-analyzer analyze program.cob --summary-only | jq '.summary'
```

### Debug Mode

```bash
# Verbose output for troubleshooting
cobol-analyzer analyze program.cob -o ./output -v 2>&1 | tee analysis.log
```

---

## See Also

- [Quickstart Guide](quickstart.md) - Get started quickly
- [Configuration](../config/settings.yaml) - Default configuration file
- [Strategy Document](../claude_generated_docs/cobol-analyzer-strategy.md) - Technical implementation details
- [Variable Filter Strategy](../claude_generated_docs/variable-filter-strategy.md) - Variable filtering implementation
