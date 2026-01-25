# COBOL AST Parser

A static analysis tool for COBOL programs that parses source code and generates JSON reports identifying variable modifications, data structure relationships, and REDEFINES impacts.

## Features

- **Variable Modification Tracking**: Identifies where variables are modified within SECTIONs and PARAGRAPHs
- **REDEFINES Analysis**: Tracks memory overlaps and affected variables through REDEFINES relationships
- **Ancestor Modification Detection**: Detects when parent group modifications affect child variables
- **Data Hierarchy Mapping**: Maps variables to their Level 01 record descriptions
- **Variable-Centric Filtering**: Transform section-centric analysis into variable-centric views
- **COPY Statement Resolution**: Resolves copybook includes for complete analysis

## Quick Example

```bash
# Analyze a COBOL program
python -m src analyze program.cob -o ./output

# Analyze and filter by specific variables
python -m src analyze-and-filter program.cob -v WS-TOTAL CUST-BALANCE -o ./output

# Filter existing analysis by variables
python -m src filter-by-variable analysis.json -v WS-TOTAL
```

## Documentation

| Document | Description |
|----------|-------------|
| [Quickstart Guide](docs/quickstart.md) | Get up and running quickly |
| [CLI Reference](docs/cli-reference.md) | Complete command-line interface reference |

## Requirements

- Python 3.10+
- Dependencies listed in `requirements.txt`

## Installation

```bash
git clone <repository-url>
cd cobol-ast-parser
pip install -r requirements.txt
```

## License

See LICENSE file for details.
