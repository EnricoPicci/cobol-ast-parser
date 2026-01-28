# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

COBOL Paragraph Variables Mapper - A static analysis tool that parses COBOL source code and generates a paragraph-centric view mapping which variables may be modified within each SECTION or PARAGRAPH.

## Build and Test Commands

```bash
# Install dependencies
pip install -r requirements.txt

# Install as editable package (enables `cobol-analyzer` command)
pip install -e .

# Run all tests
pytest

# Run a single test file
pytest tests/test_api.py

# Run a specific test
pytest tests/test_api.py::TestAnalyzePararagraphVariables::test_basic_analysis

# Run tests with coverage
pytest --cov=src

# Type checking
mypy src/

# Code formatting
black src/ tests/
```

## Running the Analyzer

```bash
# Via module (always works)
python -m src paragraph-variables-map tests/fixtures/simple_program.cob -o ./output

# Via installed command (after pip install -e .)
cobol-analyzer paragraph-variables-map tests/fixtures/simple_program.cob -o ./output

# With copybook paths
python -m src paragraph-variables-map program.cob -c ./copybooks -o ./output
```

## Architecture

### Pipeline Flow

```
COBOL Source → Preprocessor → Parser → AST Builder → Analyzers → Output Mapper
```

1. **Preprocessor** (`src/preprocessor/`): Normalizes source, detects format (fixed/free), resolves COPY statements
2. **Parser** (`src/parser/`): ANTLR4-based COBOL parser producing parse trees
3. **AST Builder** (`src/cobol_ast/builder.py`): Converts parse trees to domain-specific AST nodes
4. **Analyzers** (`src/analyzers/`): Three-stage analysis pipeline
5. **Output** (`src/output/`): Transforms analysis to JSON output

### AST Nodes (`src/cobol_ast/nodes.py`)

Core domain types:
- `CobolProgram`: Top-level container with record descriptions, sections, paragraphs
- `DataItem`: Hierarchical data structure (levels 01-88, supports REDEFINES, OCCURS)
- `RecordDescription`: Level 01 record with all subordinates
- `Section`/`Paragraph`: PROCEDURE DIVISION structure
- `VariableModification`: Records what/where/how variables are modified

### Analyzer Pipeline (`src/analyzers/`)

Three analyzers combined in `ImpactAnalyzer`:
1. `DataStructureAnalyzer`: Maps variables to Level 01 records, calculates memory regions
2. `RedefinesAnalyzer`: Builds REDEFINES graph, finds affected variables through memory overlap
3. `ProcedureAnalyzer`: Extracts modifications per section/paragraph

### Public API (`src/api.py`)

Programmatic interface via `analyze_paragraph_variables()`:
- Takes `Path` and `AnalysisOptions`
- Returns `AnalysisResult` containing both JSON outputs (analysis + paragraph-variables map)
- This is the preferred way to integrate programmatically rather than calling CLI

## COBOL Terminology

Use consistent naming that matches COBOL terminology:

| Term | Usage |
|------|-------|
| `record_description` | Level 01 data item and its subordinates |
| `data_item` | Any item in DATA DIVISION (any level) |
| `section` | PROCEDURE DIVISION section |
| `paragraph` | PROCEDURE DIVISION paragraph |
| `copybook` | External source file included via COPY |
| `redefines` | Memory overlay relationship |

## Key Design Patterns

- **REDEFINES tracking**: Memory overlap detection via NetworkX graph (undirected, using connected components)
- **Ancestor modification detection**: When a group item is modified, all children are affected
- **Orphan modifications**: PROCEDURE DIVISION statements before any paragraph/section are captured separately
- **Line mapping**: After COPY expansion, tracks original line numbers back to source/copybooks

### Variable Modification Tracking

Track all COBOL statements that modify variables:

```python
MODIFYING_STATEMENTS = [
    'MOVE', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
    'STRING', 'UNSTRING', 'INSPECT', 'ACCEPT', 'READ', 'RETURN',
    'INITIALIZE', 'SET', 'SEARCH'
]
```

For each statement type, identify the TARGET variables (not source):
- `MOVE A TO B` - B is modified
- `ADD A TO B` - B is modified
- `COMPUTE C = A + B` - C is modified

## Test Fixtures

COBOL test files in `tests/fixtures/`:
- `simple_program.cob`: Basic data/procedure structure
- `redefines_program.cob`: REDEFINES relationships
- `copybook_main.cob` + `copybooks/`: COPY statement resolution
- `all_modifications.cob`: All modification types (MOVE, COMPUTE, ADD, etc.)

## Development Guidelines

### Testing
- Use pytest for all tests
- Always add relevant tests when changing logic - any bug fix or feature change should include tests that verify the new behavior

### Documentation
- Always update documentation when changing logic - keep docs in sync with code changes
- Update docstrings when function behavior changes
- Always update `docs/api-usage.md` when adding or changing API functions, options, or return types
- Always update `docs/json-output-reference.md` when JSON output structure changes

### Code Style
- Use type hints for function signatures
- Prefer `@dataclass` for data containers
- Use Google-style docstrings
- All AST nodes should be dataclasses with `Optional` for nullable fields
