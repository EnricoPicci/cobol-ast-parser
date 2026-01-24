# COBOL Analyzer - Quickstart Guide

This guide will help you get started with the COBOL Analyzer in under 5 minutes.

## Prerequisites

- Python 3.9 or higher
- pip (Python package manager)

## Installation

### 1. Clone or download the project

```bash
cd /path/to/cobol-ast-parser
```

### 2. Install dependencies

```bash
pip install -r requirements.txt
```

### 3. Verify installation

```bash
./cobol-analyzer --version
```

You should see: `cobol-analyzer 1.0.0`

## Basic Usage

### Analyze a COBOL file

The simplest way to analyze a COBOL file (run from project root):

```bash
./cobol-analyzer /path/to/your/program.cob -o ./output
```

This will:
1. Parse the COBOL source file
2. Analyze variable modifications
3. Create the output directory if it doesn't exist
4. Write the analysis results to `./output/{PROGRAM-NAME}-analysis.json`

### Alternative invocation methods

```bash
# Using the wrapper script (recommended)
./cobol-analyzer program.cob -o ./output

# Using Python module syntax
python -m src program.cob -o ./output

# Running directly from src directory
cd src && python main.py ../program.cob -o ../output
```

### View results on screen (no file output)

```bash
./cobol-analyzer /path/to/your/program.cob
```

The JSON analysis will be printed to stdout.

## Example

Using the sample COBOL files included in the project:

```bash
# Analyze COBCALC sample
./cobol-analyzer some-cobol-source/COBCALC.cbl -o ./my-output

# View the output
cat ./my-output/COBCALC-analysis.json
```

### Sample Output

```json
{
  "analysis_date": "2026-01-24T12:18:17.181659",
  "program_name": "COBCALC",
  "execution_time_seconds": 0.0234,
  "sections_and_paragraphs": {
    "ACCEPT-INPUT": [
      {
        "variable": "INPUT-1",
        "affected_records": ["FIELDS"],
        "modification_type": "MOVE",
        "line_number": 2
      }
    ]
  },
  "summary": {
    "total_sections": 0,
    "total_paragraphs": 5,
    "total_modifications": 2,
    "unique_modified_variables": 1,
    "records_with_redefines": []
  }
}
```

## Common Options

### Compact output (deduplicated)

```bash
./cobol-analyzer program.cob -o ./output --compact
```

### Summary only (minimal output)

```bash
./cobol-analyzer program.cob -o ./output --summary-only
```

### With copybook paths

```bash
./cobol-analyzer program.cob -o ./output -c ./copybooks -c ./shared/copy
```

### Verbose mode (for debugging)

```bash
./cobol-analyzer program.cob -o ./output -v
```

### Quiet mode (errors only)

```bash
./cobol-analyzer program.cob -o ./output -q
```

## Understanding the Output

The analysis output contains:

| Field | Description |
|-------|-------------|
| `program_name` | Name of the COBOL program (from PROGRAM-ID) |
| `analysis_date` | ISO timestamp when analysis was performed |
| `execution_time_seconds` | How long the analysis took |
| `sections_and_paragraphs` | Variable modifications grouped by section/paragraph |
| `summary` | Statistics about the analysis |

### Modification Entry Fields

Each modification entry includes:
- `variable`: The variable being modified
- `affected_records`: Level 01 records that contain this variable (including REDEFINES relationships)
- `modification_type`: Type of statement (MOVE, COMPUTE, ADD, etc.)
- `line_number`: Source line number

## Next Steps

- See [CLI Reference](cli-reference.md) for all available options
- Check `config/settings.yaml` for configuration options
- Explore the sample COBOL files in `some-cobol-source/` and `complex-cobol-source/`

## Troubleshooting

### "Source file not found"

Ensure you're providing the correct path to the COBOL file:
```bash
# Use absolute path
./cobol-analyzer /full/path/to/program.cob -o ./output

# Or relative path from project root
./cobol-analyzer some-cobol-source/COBCALC.cbl -o ./output
```

### Parse errors

If you encounter parse errors:
1. Try with `--no-copy-resolution` to skip copybook processing
2. Use `-v` (verbose) mode to see detailed error messages
3. Check that the COBOL file uses standard COBOL-85 syntax

### Permission errors

If you get permission errors when creating the output directory:
```bash
# Ensure you have write permissions
mkdir -p ./output
./cobol-analyzer program.cob -o ./output
```
