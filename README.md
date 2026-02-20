# COBOL Paragraph Variables Mapper

A static analysis tool for COBOL programs that parses source code and generates a paragraph-centric view showing which variables may be modified within each SECTION or PARAGRAPH.

## Features

- **Paragraph-to-Variable Mapping**: Maps each SECTION/PARAGRAPH to the variables it may modify
- **REDEFINES Analysis**: Tracks memory overlaps and affected variables through REDEFINES relationships
- **Ancestor Modification Detection**: Detects when parent group modifications affect child variables
- **Level 01 Record Mapping**: Maps variables to their containing Level 01 record descriptions
- **COPY Statement Resolution**: Resolves copybook includes for complete analysis

## Quick Example

```bash
# Map paragraphs to the variables they may modify
python -m src paragraph-variables-map program.cob -o ./output

# With copybook paths
python -m src paragraph-variables-map program.cob -c ./copybooks -o ./output

# Exclude REDEFINES-affected variables
python -m src paragraph-variables-map program.cob --no-redefines -o ./output
```

## Output Example

```json
{
  "program_name": "TRANPROC",
  "paragraphs": {
    "3100-APPLY-PAYMENT": {
      "CUST-BALANCE": {
        "defined_in_record": "CUSTOMER-RECORD",
        "base_record": "CUSTOMER-RECORD"
      },
      "PAY-AMOUNT": {
        "defined_in_record": "PAYMENT-DETAIL",
        "base_record": "TRANSACTION-RECORD"
      }
    }
  },
  "summary": {
    "total_paragraphs_with_changes": 15,
    "total_unique_variables": 42
  }
}
```

## Documentation

| Document | Description |
|----------|-------------|
| [Quickstart Guide](docs/quickstart.md) | Get up and running quickly |
| [CLI Reference](docs/cli-reference.md) | Complete command-line interface reference |
| [Strategy Document](claude_generated_docs/strategy.md) | Technical implementation details |

## Requirements

- Python 3.10+
- Dependencies listed in `requirements.txt`

## Installation

```bash
git clone <repository-url>
cd cobol-ast-parser
pip install -r requirements.txt
```

### Optional: Install as command

To use `cobol-analyzer` as a system command (instead of `./cobol-analyzer` or `python -m src`):

```bash
pip install -e .
```

Then you can run from anywhere:

```bash
cobol-analyzer paragraph-variables-map source.cob -o ./output
```

## Contributing

After cloning, enable the project's git hooks:

```bash
git config core.hooksPath hooks
```

This activates a pre-commit hook that prevents accidental commits of corporate COBOL code or proprietary references. All examples in documentation, prompts, and comments must use generic, English-language names. See [CLAUDE.md](CLAUDE.md) for full guidelines.

## License

See LICENSE file for details.
