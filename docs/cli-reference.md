# COBOL Analyzer - CLI Reference

Complete command-line interface reference for the COBOL Paragraph Variables Mapper.

## Synopsis

```
cobol-analyzer paragraph-variables-map SOURCE [OPTIONS]
```

**Note:** The `cobol-analyzer` command requires installation: `pip install -e .`

Alternative invocation methods:

```bash
# Using the wrapper script from project root (no installation required)
./cobol-analyzer paragraph-variables-map SOURCE [OPTIONS]

# Using Python module syntax (no installation required)
python -m src paragraph-variables-map SOURCE [OPTIONS]
```

## Description

The COBOL Paragraph Variables Mapper analyzes COBOL source files and generates a paragraph-centric view showing which variables may be modified within each SECTION or PARAGRAPH. For each variable, it identifies:
- The Level 01 record where the variable is defined
- The base record (following REDEFINES chains to the ultimate non-REDEFINE root)
- Whether it's a 77-level standalone variable

The command produces two output files:
1. Full analysis JSON (detailed modification data)
2. Paragraph-variables map JSON (paragraph-centric view)

---

## Command: `paragraph-variables-map`

Analyze a COBOL source file and produce a paragraph-centric view showing which variables may change within each SECTION or PARAGRAPH.

### Synopsis

```
cobol-analyzer paragraph-variables-map SOURCE [OPTIONS]
```

### Arguments

#### SOURCE (required)

Path to the COBOL source file to analyze.

```bash
cobol-analyzer paragraph-variables-map /path/to/program.cob
cobol-analyzer paragraph-variables-map some-cobol-source/COBCALC.cbl
```

Supported file extensions: `.cob`, `.cbl`, `.cobol`

### Options

#### Output Options

##### `-o, --output-dir PATH`

Output directory for both JSON files. If not specified, only the paragraph-variables map is written to stdout.

```bash
cobol-analyzer paragraph-variables-map program.cob -o ./output
```

**Produces:**
- `{program_name}-analysis.json` - Full section-centric analysis
- `{program_name}-paragraph-variables.json` - Paragraph-to-variables mapping

##### `--analysis-filename PATTERN`

Customize the analysis output filename.

**Default:** `{program_name}-analysis.json`

##### `--output-filename PATTERN`

Customize the paragraph-variables map output filename.

**Default:** `{program_name}-paragraph-variables.json`

```bash
cobol-analyzer paragraph-variables-map program.cob -o ./output \
  --analysis-filename "{program_name}-full.json" \
  --output-filename "{program_name}-para-vars.json"
```

##### `--no-redefines`

Exclude variables that are only affected via REDEFINES relationships.

```bash
cobol-analyzer paragraph-variables-map program.cob --no-redefines -o ./output
```

##### `--no-ancestor-mods`

Exclude variables that are only affected via ancestor group modifications.

```bash
cobol-analyzer paragraph-variables-map program.cob --no-ancestor-mods -o ./output
```

##### `--include-source-info`

Include source file metadata in the analysis output.

```bash
cobol-analyzer paragraph-variables-map program.cob --include-source-info -o ./output
```

#### Copybook Options

##### `-c, --copybook-path PATH`

Add a path to search for copybooks. Can be specified multiple times.

```bash
cobol-analyzer paragraph-variables-map program.cob -c ./copybooks -c ./shared -o ./output
```

The source file's directory is always searched first, before any specified paths.

##### `--no-copy-resolution`

Skip COPY statement resolution. Use this when:
- Copybooks are not available
- You want to analyze only the main source file
- Copybook resolution is causing errors

```bash
cobol-analyzer paragraph-variables-map program.cob --no-copy-resolution -o ./output
```

#### Configuration

##### `--config FILE`

Path to a YAML configuration file. See [Configuration File](#configuration-file) section for details.

```bash
cobol-analyzer paragraph-variables-map program.cob --config ./my-config.yaml -o ./output
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
cobol-analyzer paragraph-variables-map program.cob -o ./output -v
```

##### `-q, --quiet`

Suppress all output except errors. Useful for scripting and automation.

```bash
cobol-analyzer paragraph-variables-map program.cob -o ./output -q
```

**Note:** `--verbose` and `--quiet` cannot be used together.

### Output Format

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

### Output Field Descriptions

| Field | Description |
|-------|-------------|
| `paragraphs` | Mapping of paragraph/section names to changed variables |
| `defined_in_record` | The Level 01 record where the variable is defined |
| `base_record` | The ultimate Level 01 record (follows REDEFINES chain) |
| `77-level-var` | Present and `true` if variable is a 77-level item |

---

## Information Options

### `--version`

Display version information and exit.

```bash
cobol-analyzer --version
# Output: cobol-analyzer 2.0.0
```

### `-h, --help`

Display help message and exit.

```bash
cobol-analyzer --help
cobol-analyzer paragraph-variables-map --help
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

## Examples

### Basic Usage

```bash
# Map paragraphs to changed variables
cobol-analyzer paragraph-variables-map program.cob -o ./output

# Output to stdout
cobol-analyzer paragraph-variables-map program.cob
```

### With Copybooks

```bash
# Single copybook path
cobol-analyzer paragraph-variables-map program.cob -c ./copybooks -o ./output

# Multiple copybook paths
cobol-analyzer paragraph-variables-map program.cob -c ./copybooks -c ./shared -o ./output
```

### Filtering Options

```bash
# Exclude REDEFINES-affected variables
cobol-analyzer paragraph-variables-map program.cob --no-redefines -o ./output

# Exclude ancestor-modified variables
cobol-analyzer paragraph-variables-map program.cob --no-ancestor-mods -o ./output

# Exclude both
cobol-analyzer paragraph-variables-map program.cob --no-redefines --no-ancestor-mods -o ./output
```

### Production Usage

```bash
# Full analysis with all options
cobol-analyzer paragraph-variables-map /app/cobol/MAINPROG.cob \
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
    cobol-analyzer paragraph-variables-map "$file" \
      -o "$OUTPUT_DIR" \
      -q
done

echo "Analysis complete. Results in $OUTPUT_DIR"
```

### Debug Mode

```bash
# Verbose output for troubleshooting
cobol-analyzer paragraph-variables-map program.cob -o ./output -v 2>&1 | tee analysis.log
```

---

## Exit Codes

| Code | Description |
|------|-------------|
| 0 | Success |
| 1 | Error (parse error, file not found, etc.) |

---

## See Also

- [Quickstart Guide](quickstart.md) - Get started quickly
- [Strategy Document](../claude_generated_docs/strategy.md) - Technical implementation details
- [Configuration](../config/settings.yaml) - Default configuration file
