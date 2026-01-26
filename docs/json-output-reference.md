# JSON Output Reference

This document describes all properties in the JSON files produced by the COBOL Paragraph Variables Mapper.

## Overview

The COBOL Paragraph Variables Mapper produces two types of JSON output files:

| Output Type | Default Filename | Description |
|-------------|------------------|-------------|
| Analysis Output | `{program_name}-analysis.json` | Section-centric view of all variable modifications |
| Paragraph Variables Map | `{program_name}-paragraph-variables.json` | Paragraph-centric view of all changed variables with record membership |

---

## Analysis Output

The analysis output provides a comprehensive, section-centric view of all variable modifications in the COBOL program.

### Top-Level Properties

| Property | Type | Description |
|----------|------|-------------|
| `program_name` | string | The PROGRAM-ID from the COBOL source |
| `analysis_date` | string | ISO 8601 timestamp when the analysis was performed |
| `execution_time_seconds` | number | Time taken to perform the analysis |
| `sections_and_paragraphs` | object | Main analysis data (see below) |
| `data_hierarchy` | object | Variable hierarchy mappings (see below) |
| `summary` | object | Statistical summary of the analysis |
| `source_info` | object | *(Optional)* Source file metadata (when `--include-source-info` is used) |

### Example Structure

```json
{
  "program_name": "TRANPROC",
  "analysis_date": "2026-01-25T14:02:58.528677",
  "execution_time_seconds": 0.0116,
  "sections_and_paragraphs": { ... },
  "data_hierarchy": { ... },
  "summary": { ... },
  "source_info": { ... }
}
```

---

### `sections_and_paragraphs`

An object where each key is a SECTION or PARAGRAPH name, and the value is an array of modification entries.

```json
{
  "sections_and_paragraphs": {
    "3000-PROCESS-PAYMENT": [
      { /* modification entry */ },
      { /* modification entry */ }
    ],
    "4200-UPDATE-BALANCE": [
      { /* modification entry */ }
    ]
  }
}
```

#### Modification Entry Properties

| Property | Type | Description |
|----------|------|-------------|
| `variable` | string | Name of the variable being modified |
| `modification_type` | string | Type of COBOL statement (see [Modification Types](#modification-types)) |
| `line_number` | integer | Line number in the source file (after COPY expansion) |
| `affected_records` | array | List of Level 01 record names that contain this variable, including records linked via REDEFINES |
| `affected_variables` | array | *(Optional)* Variables indirectly affected via REDEFINES (see below) |

#### Example Modification Entry

```json
{
  "variable": "CUST-BALANCE",
  "modification_type": "ADD",
  "line_number": 401,
  "affected_records": ["CUSTOMER-RECORD"]
}
```

#### Modification Entry with REDEFINES Impact

When a modification affects variables in records that REDEFINE other records, the `affected_variables` array is included:

```json
{
  "variable": "TRAN-CUSTOMER-ID",
  "modification_type": "MOVE",
  "line_number": 327,
  "affected_records": [
    "PAYMENT-DETAIL",
    "PURCHASE-DETAIL",
    "REFUND-DETAIL",
    "TRANSACTION-RECORD"
  ],
  "affected_variables": [
    {
      "name": "PAY-CASH",
      "overlap_type": "direct_redefines_group",
      "redefines_chain": "PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD",
      "redefines_level": 1
    }
  ]
}
```

##### `affected_variables` Entry Properties

| Property | Type | Description |
|----------|------|-------------|
| `name` | string | Name of the affected variable |
| `overlap_type` | string | Type of memory overlap (see [Overlap Types](#overlap-types)) |
| `redefines_chain` | string | Human-readable description of the REDEFINES relationship |
| `redefines_level` | integer | Depth in the REDEFINES chain (1 = direct REDEFINES) |

---

### `data_hierarchy`

An object mapping each variable name to its ancestry chain, from the root Level 01 record down to the variable itself.

```json
{
  "data_hierarchy": {
    "CUST-BALANCE": [
      "CUSTOMER-RECORD",
      "CUST-FINANCIAL",
      "CUST-BALANCE"
    ],
    "CUST-CITY": [
      "CUSTOMER-RECORD",
      "CUST-ADDRESS",
      "CUST-CITY"
    ]
  }
}
```

| Property | Type | Description |
|----------|------|-------------|
| *(variable name)* | array | Ordered list of ancestor names from root record to the variable |

The array always:
- Starts with the Level 01 record name
- Ends with the variable itself
- Lists intermediate group items in hierarchical order

**Use case:** This enables identification of ancestor modifications - when a parent group is modified, all its children are indirectly affected.

---

### `summary`

Statistical overview of the analysis results.

| Property | Type | Description |
|----------|------|-------------|
| `total_sections` | integer | Number of SECTION entries in the PROCEDURE DIVISION |
| `total_paragraphs` | integer | Number of PARAGRAPH entries (including those within sections) |
| `total_modifications` | integer | Total count of variable modification statements found |
| `unique_modified_variables` | integer | Count of distinct variable names that are modified |
| `records_with_redefines` | array | List of Level 01 record names involved in REDEFINES relationships |

#### Example

```json
{
  "summary": {
    "total_sections": 9,
    "total_paragraphs": 22,
    "total_modifications": 55,
    "unique_modified_variables": 28,
    "records_with_redefines": [
      "TRANSACTION-RECORD",
      "PAYMENT-DETAIL",
      "PURCHASE-DETAIL",
      "REFUND-DETAIL"
    ]
  }
}
```

---

### `source_info`

*(Optional - included when `--include-source-info` flag is used)*

Metadata about the source file that was analyzed.

| Property | Type | Description |
|----------|------|-------------|
| `file_path` | string | Absolute path to the source file |
| `file_name` | string | Base name of the source file |
| `source_format` | string | Detected COBOL format: `"fixed"` or `"free"` |
| `lines_count` | integer | Total number of lines in the source (after COPY expansion) |

#### Example

```json
{
  "source_info": {
    "file_path": "/app/cobol/TRANPROC.cbl",
    "file_name": "TRANPROC.cbl",
    "source_format": "fixed",
    "lines_count": 465
  }
}
```

---

## Paragraph Variables Map Output

The paragraph variables map output provides a paragraph-centric view showing which variables may change within each SECTION or PARAGRAPH, with Level 01 record membership including REDEFINES resolution.

### Top-Level Properties

| Property | Type | Description |
|----------|------|-------------|
| `program_name` | string | The PROGRAM-ID from the COBOL source |
| `analysis_date` | string | ISO 8601 timestamp (inherited from the analysis) |
| `execution_time_seconds` | number | Time taken to perform the mapping operation |
| `paragraphs` | object | Mapping of paragraph/section names to changed variables |
| `summary` | object | Statistical summary of the mapping results |

### Example Structure

```json
{
  "program_name": "TRANPROC",
  "analysis_date": "2026-01-25T14:02:58.528677",
  "execution_time_seconds": 0.0025,
  "paragraphs": { ... },
  "summary": { ... }
}
```

---

### `paragraphs`

An object where each key is a SECTION or PARAGRAPH name, and the value is an object mapping variable names to their record information.

```json
{
  "paragraphs": {
    "3100-APPLY-PAYMENT": {
      "CUST-BALANCE": {
        "defined_in_record": "CUSTOMER-RECORD",
        "base_record": "CUSTOMER-RECORD"
      },
      "PAY-CASH": {
        "defined_in_record": "PAYMENT-DETAIL",
        "base_record": "TRANSACTION-RECORD"
      }
    },
    "4200-UPDATE-TOTALS": {
      "WS-TOTAL-PAYMENTS": { ... }
    }
  }
}
```

**Note:** Paragraphs with no variable changes are excluded from the output.

#### Variable Entry Properties

| Property | Type | Description |
|----------|------|-------------|
| `defined_in_record` | string | Level 01 record that directly contains this variable |
| `base_record` | string | Ultimate Level 01 record (follows REDEFINES chain to root) |
| `77-level-var` | boolean | *(Only present if true)* Variable is a 77-level standalone item |

##### Example Variable Entries

**Standard variable:**
```json
{
  "CUST-BALANCE": {
    "defined_in_record": "CUSTOMER-RECORD",
    "base_record": "CUSTOMER-RECORD"
  }
}
```

**Variable in REDEFINES record:**
```json
{
  "PAY-CASH": {
    "defined_in_record": "PAYMENT-DETAIL",
    "base_record": "TRANSACTION-RECORD"
  }
}
```

The `defined_in_record` shows that `PAY-CASH` is structurally within `PAYMENT-DETAIL`, while `base_record` shows that `PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD`, so the ultimate memory allocation is under `TRANSACTION-RECORD`.

**77-level variable:**
```json
{
  "WS-STANDALONE": {
    "defined_in_record": "WS-STANDALONE",
    "base_record": "WS-STANDALONE",
    "77-level-var": true
  }
}
```

---

### `summary` (Paragraph Variables Map)

Statistical overview of the mapping results.

| Property | Type | Description |
|----------|------|-------------|
| `total_paragraphs_with_changes` | integer | Number of paragraphs/sections that have variable changes |
| `total_unique_variables` | integer | Count of distinct variable names across all paragraphs |
| `variables_in_redefines_records` | integer | Count of variables where `defined_in_record` differs from `base_record` |
| `variables_via_ancestor_modification` | integer | Count of variables included because an ancestor group was modified |
| `level_77_variables` | integer | Count of 77-level standalone variables |

#### Example

```json
{
  "summary": {
    "total_paragraphs_with_changes": 15,
    "total_unique_variables": 42,
    "variables_in_redefines_records": 8,
    "variables_via_ancestor_modification": 5,
    "level_77_variables": 2
  }
}
```

---

### Variable Collection Sources

The paragraph variables map collects variables from three sources:

1. **Direct modifications** - Variables explicitly named in modification statements (e.g., `MOVE X TO VAR-A`)

2. **REDEFINES-affected variables** - When a variable in one record is modified, variables in REDEFINES-related records that share the same memory are also included (can be disabled with `--no-redefines`)

3. **Ancestor modifications** - When a group item is modified (e.g., `INITIALIZE CUSTOMER-RECORD`), all subordinate items are indirectly affected and included (can be disabled with `--no-ancestor-mods`)

---

## Reference Tables

### Modification Types

The `modification_type` field indicates which COBOL statement modifies the variable.

| Type | COBOL Statements | Description |
|------|------------------|-------------|
| `MOVE` | `MOVE`, `MOVE CORRESPONDING` | Data movement |
| `COMPUTE` | `COMPUTE` | Arithmetic expression assignment |
| `ADD` | `ADD` | Addition |
| `SUBTRACT` | `SUBTRACT` | Subtraction |
| `MULTIPLY` | `MULTIPLY` | Multiplication |
| `DIVIDE` | `DIVIDE` | Division |
| `STRING` | `STRING` | String concatenation |
| `UNSTRING` | `UNSTRING` | String parsing |
| `INSPECT` | `INSPECT` | Character inspection/replacement |
| `ACCEPT` | `ACCEPT` | User/system input |
| `READ` | `READ` | File input |
| `RETURN` | `RETURN` | Sort/merge file return |
| `INITIALIZE` | `INITIALIZE` | Reset to default values |
| `SET` | `SET` | Condition/index setting |
| `SEARCH` | `SEARCH` | Table search with SET |

---

### Overlap Types

The `overlap_type` field describes the memory relationship between variables in REDEFINES scenarios.

| Type | Description |
|------|-------------|
| `direct_redefines_group` | Variable is inside a record that directly REDEFINES another record |
| `full` | Variables occupy exactly the same memory region |
| `partial` | Variables share some but not all memory bytes |
| `contains` | The modified variable's memory fully contains the affected variable |
| `contained_by` | The affected variable's memory fully contains the modified variable |

---

## Complete Examples

### Analysis Output Example

```json
{
  "program_name": "TRANPROC",
  "analysis_date": "2026-01-25T14:02:58.528677",
  "execution_time_seconds": 0.0116,
  "sections_and_paragraphs": {
    "1000-INIT": [
      {
        "variable": "WS-TOTALS",
        "modification_type": "INITIALIZE",
        "line_number": 292,
        "affected_records": ["WS-TOTALS"]
      }
    ],
    "4200-UPDATE-BALANCE": [
      {
        "variable": "CUST-BALANCE",
        "modification_type": "ADD",
        "line_number": 401,
        "affected_records": ["CUSTOMER-RECORD"]
      }
    ]
  },
  "data_hierarchy": {
    "CUST-BALANCE": ["CUSTOMER-RECORD", "CUST-FINANCIAL", "CUST-BALANCE"],
    "WS-TOTAL-PAYMENTS": ["WS-TOTALS", "WS-TOTAL-PAYMENTS"]
  },
  "summary": {
    "total_sections": 9,
    "total_paragraphs": 22,
    "total_modifications": 55,
    "unique_modified_variables": 28,
    "records_with_redefines": ["TRANSACTION-RECORD", "PAYMENT-DETAIL"]
  }
}
```

### Paragraph Variables Map Example

```json
{
  "program_name": "TRANPROC",
  "analysis_date": "2026-01-25T14:02:58.528677",
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
    },
    "1000-INIT": {
      "WS-TOTALS": {
        "defined_in_record": "WS-TOTALS",
        "base_record": "WS-TOTALS"
      },
      "WS-TOTAL-PAYMENTS": {
        "defined_in_record": "WS-TOTALS",
        "base_record": "WS-TOTALS"
      },
      "WS-TOTAL-REFUNDS": {
        "defined_in_record": "WS-TOTALS",
        "base_record": "WS-TOTALS"
      }
    }
  },
  "summary": {
    "total_paragraphs_with_changes": 2,
    "total_unique_variables": 5,
    "variables_in_redefines_records": 1,
    "variables_via_ancestor_modification": 2,
    "level_77_variables": 1
  }
}
```

---

## See Also

- [CLI Reference](cli-reference.md) - Command-line options and usage
- [Quickstart Guide](quickstart.md) - Getting started with the analyzer
- [Strategy Document](../claude_generated_docs/strategy.md) - Technical implementation details
