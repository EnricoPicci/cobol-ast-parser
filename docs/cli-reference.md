# COBOL Analyzer - CLI Reference

Complete command-line interface reference for the COBOL Analyzer.

## Synopsis

```
./cobol-analyzer SOURCE [-o OUTPUT_DIR] [OPTIONS]
```

Alternative invocation methods:

```bash
# Using Python module syntax
python -m src SOURCE [-o OUTPUT_DIR] [OPTIONS]

# Running directly from src directory
cd src && python main.py SOURCE [-o OUTPUT_DIR] [OPTIONS]
```

## Description

The COBOL Analyzer parses COBOL source files and generates a JSON report identifying:
- Variable modifications within SECTIONs and PARAGRAPHs
- Associated Level 01 record descriptions
- REDEFINES relationships and affected variables
- Execution timing metrics

## Arguments

### SOURCE (required)

Path to the COBOL source file to analyze.

```bash
./cobol-analyzer /path/to/program.cob
./cobol-analyzer some-cobol-source/COBCALC.cbl
```

Supported file extensions: `.cob`, `.cbl`, `.cobol`

## Options

### Output Options

#### `-o, --output-dir PATH`

Output directory for analysis results. The directory will be created if it doesn't exist.

```bash
./cobol-analyzer program.cob -o ./output
./cobol-analyzer program.cob --output-dir /tmp/analysis
```

If not specified, output is written to stdout.

#### `--output-filename PATTERN`

Customize the output filename. Use `{program_name}` as a placeholder for the COBOL program name.

**Default:** `{program_name}-analysis.json`

```bash
# Default: creates MYPROGRAM-analysis.json
./cobol-analyzer MYPROGRAM.cob -o ./output

# Custom: creates MYPROGRAM-report.json
./cobol-analyzer MYPROGRAM.cob -o ./output --output-filename "{program_name}-report.json"

# Custom: creates analysis-MYPROGRAM.json
./cobol-analyzer MYPROGRAM.cob -o ./output --output-filename "analysis-{program_name}.json"
```

#### `--compact`

Generate compact output with deduplicated variables per section. Each variable appears only once per section/paragraph, with all affected records combined.

```bash
./cobol-analyzer program.cob -o ./output --compact
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

#### `--summary-only`

Output only the summary section, excluding detailed modification data. Useful for quick statistics.

```bash
./cobol-analyzer program.cob -o ./output --summary-only
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

#### `--include-source-info`

Include source file metadata in the output.

```bash
./cobol-analyzer program.cob -o ./output --include-source-info
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

### Copybook Options

#### `-c, --copybook-path PATH`

Add a path to search for copybooks (COPY statements). Can be specified multiple times.

```bash
# Single copybook path
./cobol-analyzer program.cob -o ./output -c ./copybooks

# Multiple copybook paths (searched in order)
./cobol-analyzer program.cob -o ./output -c ./copybooks -c ./shared/copy -c /usr/share/cobol/copy
```

The source file's directory is always searched first, before any specified paths.

#### `--no-copy-resolution`

Skip COPY statement resolution. Use this when:
- Copybooks are not available
- You want to analyze only the main source file
- Copybook resolution is causing errors

```bash
./cobol-analyzer program.cob -o ./output --no-copy-resolution
```

### Configuration

#### `--config FILE`

Path to a YAML configuration file. See [Configuration](#configuration-file) section for details.

```bash
./cobol-analyzer program.cob -o ./output --config ./my-config.yaml
```

### Logging Options

#### `-v, --verbose`

Enable verbose output with debug-level logging. Shows detailed information about:
- File reading
- Format detection
- COPY resolution
- Parsing progress
- AST building

```bash
./cobol-analyzer program.cob -o ./output -v
```

#### `-q, --quiet`

Suppress all output except errors. Useful for scripting and automation.

```bash
./cobol-analyzer program.cob -o ./output -q
```

**Note:** `--verbose` and `--quiet` cannot be used together.

### Information Options

#### `--version`

Display version information and exit.

```bash
./cobol-analyzer --version
# Output: cobol-analyzer 1.0.0
```

#### `-h, --help`

Display help message and exit.

```bash
./cobol-analyzer --help
```

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

## Output Format

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
        "line_number": 123
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

## Exit Codes

| Code | Description |
|------|-------------|
| 0 | Success |
| 1 | Error (parse error, file not found, etc.) |

## Examples

### Basic Analysis

```bash
# Analyze and output to directory
./cobol-analyzer program.cob -o ./output

# Analyze and print to stdout
./cobol-analyzer program.cob
```

### Production Usage

```bash
# Full analysis with all options
./cobol-analyzer /app/cobol/MAINPROG.cob \
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

for file in "$SOURCE_DIR"/*.cob; do
    ./cobol-analyzer "$file" -o "$OUTPUT_DIR" --compact -q
done

echo "Analysis complete. Results in $OUTPUT_DIR"
```

### Quick Statistics

```bash
# Get only summary statistics
./cobol-analyzer program.cob --summary-only | jq '.summary'
```

### Debug Mode

```bash
# Verbose output for troubleshooting
./cobol-analyzer program.cob -o ./output -v 2>&1 | tee analysis.log
```

## See Also

- [Quickstart Guide](quickstart.md) - Get started quickly
- [Configuration](../config/settings.yaml) - Default configuration file
- [Strategy Document](../claude_generated_docs/cobol-analyzer-strategy.md) - Technical implementation details
