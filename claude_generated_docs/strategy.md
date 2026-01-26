# COBOL Paragraph Variables Mapper - Strategy Document

## Purpose

The COBOL Paragraph Variables Mapper is a static analysis tool that parses COBOL source code and produces a **paragraph-centric view** showing which variables may be modified within each SECTION or PARAGRAPH. For each variable, it identifies:

1. The Level 01 record where the variable is defined (`defined_in_record`)
2. The base record after following REDEFINES chains (`base_record`)
3. Whether it's a 77-level standalone variable

This information is essential for understanding the data flow and side effects of COBOL procedure divisions.

## Architecture Overview

```
┌─────────────────┐
│  COBOL Source   │
│     File        │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Preprocessor   │  ← COPY resolution, format detection, normalization
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│     Parser      │  ← Regex-based COBOL parser
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   AST Builder   │  ← Parse tree to domain AST
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Impact Analyzer │  ← Orchestrates all analysis components
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   Paragraph     │  ← Transforms to paragraph-centric view
│Variables Mapper │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   JSON Output   │
└─────────────────┘
```

## Key Components

### 1. Preprocessor (`src/preprocessor/`)

The preprocessor prepares COBOL source for parsing:

- **CopyResolver**: Resolves COPY statements by inlining copybook contents. Handles nested COPY statements up to a configurable depth (default: 16 levels).

- **Format Detector**: Automatically detects whether the source uses fixed-format (columns 1-6 sequence, 7 indicator, 8-72 code) or free-format COBOL.

- **Normalizer**: Normalizes source code by handling continuation lines, removing sequence numbers, and standardizing whitespace.

### 2. Parser (`src/parser/`)

A simplified regex-based COBOL parser that extracts:

- **IDENTIFICATION DIVISION**: Program name
- **DATA DIVISION**: Variable definitions with levels, names, PIC clauses, REDEFINES clauses
- **PROCEDURE DIVISION**: Sections, paragraphs, and statements

The parser produces a parse tree structure that is then converted to a domain-specific AST.

### 3. AST Builder (`src/cobol_ast/`)

Converts the parse tree into a clean domain AST with these node types:

- **Program**: Root node containing divisions
- **DataItem**: Variable definitions with level, name, picture, redefines target
- **Section**: Procedure division sections
- **Paragraph**: Named procedure blocks
- **Statement**: Individual COBOL statements

### 4. Impact Analyzer (`src/analyzers/`)

The Impact Analyzer orchestrates three sub-analyzers:

#### DataStructureAnalyzer
- Maps each variable to its containing Level 01 record
- Builds the complete data hierarchy
- Identifies group items vs elementary items

#### RedefinesAnalyzer
- Builds a graph of REDEFINES relationships
- Identifies memory overlap between records
- Tracks the "base record" (ultimate non-REDEFINES root)

#### ProcedureAnalyzer
- Scans the PROCEDURE DIVISION for variable modifications
- Identifies modification types: MOVE, COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE, STRING, UNSTRING, INSPECT, ACCEPT, READ, RETURN, INITIALIZE, SET, SEARCH
- Tracks which section/paragraph contains each modification
- Detects ancestor modifications (when a group item is modified, affecting all children)

### 5. Paragraph Variables Mapper (`src/output/paragraph_variables_map.py`)

Transforms the section-centric analysis output into a paragraph-centric view:

**Input**: Analysis data with `sections_and_paragraphs` (modifications by section/paragraph) and `data_hierarchy` (variable-to-record mapping)

**Output**: A map where each paragraph/section key contains the variables that may change:

```json
{
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
  }
}
```

## REDEFINES Handling

COBOL's REDEFINES clause allows multiple record definitions to share the same memory. When a variable in one record is modified, variables in overlapping records may also change.

### Example

```cobol
01  CUSTOMER-RECORD.
    05  CUST-ID        PIC X(10).
    05  CUST-BALANCE   PIC 9(7)V99.

01  CUSTOMER-OVERLAY REDEFINES CUSTOMER-RECORD.
    05  CUST-RAW-DATA  PIC X(19).
```

If code modifies `CUST-RAW-DATA`, both `CUST-ID` and `CUST-BALANCE` may be affected due to memory overlap.

The mapper handles this by:
1. Building a REDEFINES graph from the data hierarchy
2. For each modification, checking if it affects a REDEFINES-related record
3. Including affected variables in the output with the `base_record` pointing to the ultimate non-REDEFINES root

### Filtering REDEFINES

Use `--no-redefines` to exclude variables that are only affected via REDEFINES relationships.

## Ancestor Modification Tracking

When a group item is modified (e.g., via INITIALIZE or MOVE to a group), all subordinate variables are affected.

### Example

```cobol
01  WS-TOTALS.
    05  WS-TOTAL-SALES    PIC 9(9)V99.
    05  WS-TOTAL-TAX      PIC 9(7)V99.
    05  WS-GRAND-TOTAL    PIC 9(9)V99.

INITIALIZE WS-TOTALS.
```

The INITIALIZE affects `WS-TOTAL-SALES`, `WS-TOTAL-TAX`, and `WS-GRAND-TOTAL` even though they're not directly named.

The mapper tracks this by:
1. Identifying modifications to group items
2. Including all subordinate variables in the affected paragraph's output
3. Marking them appropriately in the summary statistics

### Filtering Ancestor Modifications

Use `--no-ancestor-mods` to exclude variables that are only affected via ancestor group modifications.

## Output Format

### Paragraph Variables Map JSON

```json
{
  "program_name": "TRANPROC",
  "analysis_date": "2026-01-25T10:00:00Z",
  "execution_time_seconds": 0.0025,
  "paragraphs": {
    "SECTION-OR-PARAGRAPH-NAME": {
      "VARIABLE-NAME": {
        "defined_in_record": "RECORD-NAME",
        "base_record": "BASE-RECORD-NAME",
        "77-level-var": true  // optional, only present for 77-level items
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

### Field Descriptions

| Field | Description |
|-------|-------------|
| `program_name` | COBOL program name from PROGRAM-ID |
| `analysis_date` | ISO timestamp when analysis was performed |
| `execution_time_seconds` | Processing time |
| `paragraphs` | Map of paragraph/section names to changed variables |
| `defined_in_record` | The Level 01 record where the variable is defined |
| `base_record` | The ultimate Level 01 record after following REDEFINES chains |
| `77-level-var` | Present and `true` if the variable is a 77-level item |
| `summary` | Aggregate statistics about the analysis |

## CLI Usage

```bash
# Basic usage
cobol-analyzer paragraph-variables-map source.cob -o ./output

# With copybook paths
cobol-analyzer paragraph-variables-map source.cob -c ./copybooks -o ./output

# Exclude REDEFINES-affected variables
cobol-analyzer paragraph-variables-map source.cob --no-redefines -o ./output

# Exclude ancestor-modified variables
cobol-analyzer paragraph-variables-map source.cob --no-ancestor-mods -o ./output

# Output to stdout only
cobol-analyzer paragraph-variables-map source.cob
```

## Design Decisions

### Why Paragraph-Centric?

The paragraph-centric view answers the question: "If I execute this paragraph, which variables might change?" This is valuable for:

- **Impact analysis**: Understanding the scope of changes when modifying a paragraph
- **Testing**: Identifying variables to verify after executing a code path
- **Documentation**: Generating data flow documentation
- **Refactoring**: Understanding dependencies before restructuring code

### Why Include REDEFINES?

REDEFINES creates implicit data dependencies that are easy to miss during code review. By including REDEFINES-affected variables, the tool reveals these hidden relationships.

### Why Track Ancestor Modifications?

Group-level operations like INITIALIZE affect all subordinates. Without tracking ancestors, the tool would miss significant data changes.

## Limitations

1. **Static Analysis Only**: The tool analyzes code structure, not runtime behavior. Conditional branches are not evaluated.

2. **PERFORM Not Traced**: When a paragraph PERFORMs another paragraph, the callee's modifications are not included in the caller's entry.

3. **Pointer/Reference Semantics**: Dynamic references via pointer manipulation or reference modification are not tracked.

4. **External Programs**: CALL statements to external programs are not analyzed for their effects.
