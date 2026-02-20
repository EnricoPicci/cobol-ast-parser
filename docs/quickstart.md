# COBOL Paragraph Variables Mapper - Quickstart Guide

This guide will help you get started with the COBOL Paragraph Variables Mapper.

## Prerequisites

- Python 3.10 or higher
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

### 3. Enable git hooks

```bash
git config core.hooksPath hooks
```

This activates a pre-commit hook that prevents accidental commits of corporate COBOL references. Required for all contributors.

### 4. (Optional) Install as command

To use `cobol-analyzer` as a system command from anywhere:

```bash
pip install -e .
```

### 5. Verify installation

```bash
# Using wrapper script (from project root)
./cobol-analyzer --version

# Or if installed as command
cobol-analyzer --version
```

You should see: `cobol-analyzer 2.0.0`

## Basic Usage

### Map paragraphs to variables

The simplest way to analyze a COBOL file (run from project root):

```bash
./cobol-analyzer paragraph-variables-map /path/to/your/program.cob -o ./output
```

This will:
1. Parse the COBOL source file
2. Analyze variable modifications
3. Create the output directory if it doesn't exist
4. Write the analysis results to:
   - `./output/{PROGRAM-NAME}-analysis.json` - Full analysis
   - `./output/{PROGRAM-NAME}-paragraph-variables.json` - Paragraph-to-variables map

### Alternative invocation methods

```bash
# Using the wrapper script from project root
./cobol-analyzer paragraph-variables-map program.cob -o ./output

# Using Python module syntax
python -m src paragraph-variables-map program.cob -o ./output

# Using installed command (requires: pip install -e .)
cobol-analyzer paragraph-variables-map program.cob -o ./output
```

### View results on screen (no file output)

```bash
./cobol-analyzer paragraph-variables-map /path/to/your/program.cob
```

The JSON paragraph-variables map will be printed to stdout.

## Example

Using the sample COBOL files included in the project:

```bash
# Analyze COBCALC sample
./cobol-analyzer paragraph-variables-map some-cobol-source/COBCALC.cbl -o ./my-output

# View the output
cat ./my-output/COBCALC-paragraph-variables.json
```

### Sample Output

```json
{
  "program_name": "COBCALC",
  "analysis_date": "2026-01-26T12:00:00.000000",
  "execution_time_seconds": 0.0012,
  "paragraphs": {
    "ACCEPT-INPUT": {
      "INPUT-1": {
        "defined_in_record": "FIELDS",
        "base_record": "FIELDS"
      }
    },
    "CALCULATE-RESULT": {
      "RESULT": {
        "defined_in_record": "FIELDS",
        "base_record": "FIELDS"
      }
    }
  },
  "summary": {
    "total_paragraphs_with_changes": 2,
    "total_unique_variables": 2,
    "variables_in_redefines_records": 0,
    "variables_via_ancestor_modification": 0,
    "level_77_variables": 0
  }
}
```

## Common Options

### With copybook paths

```bash
./cobol-analyzer paragraph-variables-map program.cob -o ./output -c ./copybooks -c ./shared/copy
```

### Exclude REDEFINES-affected variables

```bash
./cobol-analyzer paragraph-variables-map program.cob -o ./output --no-redefines
```

### Exclude ancestor-modified variables

```bash
./cobol-analyzer paragraph-variables-map program.cob -o ./output --no-ancestor-mods
```

### Verbose mode (for debugging)

```bash
./cobol-analyzer paragraph-variables-map program.cob -o ./output -v
```

### Quiet mode (errors only)

```bash
./cobol-analyzer paragraph-variables-map program.cob -o ./output -q
```

## Understanding the Output

The paragraph-variables map output contains:

| Field | Description |
|-------|-------------|
| `program_name` | Name of the COBOL program (from PROGRAM-ID) |
| `analysis_date` | ISO timestamp when analysis was performed |
| `execution_time_seconds` | How long the analysis took |
| `paragraphs` | Map of paragraph/section names to variables they may modify |
| `summary` | Statistics about the analysis |

### Variable Entry Fields

Each variable entry includes:
- `defined_in_record`: The Level 01 record where the variable is defined
- `base_record`: The ultimate Level 01 record (follows REDEFINES chains)
- `77-level-var`: Present and `true` if the variable is a 77-level item

## Next Steps

- See [CLI Reference](cli-reference.md) for all available options
- Read the [Strategy Document](../claude_generated_docs/strategy.md) for technical details
- Check `config/settings.yaml` for configuration options
- Explore the sample COBOL files in `some-cobol-source/` and `complex-cobol-source/`

## Troubleshooting

### "Source file not found"

Ensure you're providing the correct path to the COBOL file:
```bash
# Use absolute path
./cobol-analyzer paragraph-variables-map /full/path/to/program.cob -o ./output

# Or relative path from project root
./cobol-analyzer paragraph-variables-map some-cobol-source/COBCALC.cbl -o ./output
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
./cobol-analyzer paragraph-variables-map program.cob -o ./output
```
