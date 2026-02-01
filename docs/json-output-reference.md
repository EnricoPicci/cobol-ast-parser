# JSON Output Reference

This document describes all properties in the JSON files produced by the COBOL Paragraph Variables Mapper.

## Overview

The COBOL Paragraph Variables Mapper produces several types of JSON output:

| Output Type | API Return Type | Description |
|-------------|-----------------|-------------|
| Analysis Output | `AnalysisResult.analysis` | Section-centric view of all variable modifications |
| Paragraph Variables Map | `AnalysisResult.paragraph_variables` | Paragraph-centric view of all changed variables with record membership |
| DATA DIVISION Tree | `DataDivisionTree.to_dict()` | Hierarchical tree view of DATA DIVISION structure |

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

**Special key: `PROCEDURE DIVISION`**

If the COBOL program contains statements in the PROCEDURE DIVISION that are outside any PARAGRAPH or SECTION (often initialization code at the start), these modifications are captured under the special key `"PROCEDURE DIVISION"`.

```json
{
  "sections_and_paragraphs": {
    "PROCEDURE DIVISION": [
      { /* orphan modification entry - outside any paragraph */ }
    ],
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
| `source_info` | object | *(Optional)* Source file metadata (when `--include-source-info` is used) |

### Example Structure

```json
{
  "program_name": "TRANPROC",
  "analysis_date": "2026-01-25T14:02:58.528677",
  "execution_time_seconds": 0.0025,
  "paragraphs": { ... },
  "summary": { ... },
  "source_info": { ... }
}
```

---

### `paragraphs`

An object where each key is a SECTION or PARAGRAPH name, and the value is an object mapping variable names to their record information and explanation.

**Special key: `PROCEDURE DIVISION`**

If the COBOL program contains statements in the PROCEDURE DIVISION that are outside any PARAGRAPH or SECTION (often initialization code at the start), these modifications are captured under the special key `"PROCEDURE DIVISION"`. This ensures all variable modifications are tracked, regardless of where they occur in the code.

```json
{
  "paragraphs": {
    "PROCEDURE DIVISION": {
      "INIT-FLAG": {
        "defined_in_record": "WS-FLAGS",
        "base_record": "WS-FLAGS",
        "explanation": "direct modification: MOVE at line 24"
      }
    },
    "3100-APPLY-PAYMENT": {
      "CUST-BALANCE": {
        "defined_in_record": "CUSTOMER-RECORD",
        "base_record": "CUSTOMER-RECORD",
        "explanation": "directly affected by ADD at line 185"
      },
      "PAY-CASH": {
        "defined_in_record": "PAYMENT-DETAIL",
        "base_record": "TRANSACTION-RECORD",
        "explanation": "directly affected by MOVE at line 201"
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
| `defined_in_record` | string | Level 01 record that directly contains this variable. For FILLER records, uses raw format (e.g., `FILLER$1`) for consistent linking with `DataDivisionTree`. |
| `base_record` | string | Ultimate Level 01 record (follows REDEFINES chain to root) |
| `position` | object | Byte position info with `start` and `end` (1-indexed, inclusive) |
| `explanation` | string | Human-readable explanation of why this variable may change (see [Explanation Formats](#explanation-formats)) |
| `77-level-var` | boolean | *(Only present if true)* Variable is a 77-level standalone item |

**Example: Variable in FILLER record:**
```json
{
  "CUSTOMER-ID": {
    "base_record": "ORDER-BUFFER",
    "defined_in_record": "FILLER$1",
    "position": { "start": 1, "end": 10 },
    "explanation": "direct modification: MOVE at line 27"
  }
}
```

##### Example Variable Entries

**Standard variable with direct modification:**
```json
{
  "CUST-BALANCE": {
    "defined_in_record": "CUSTOMER-RECORD",
    "base_record": "CUSTOMER-RECORD",
    "position": { "start": 45, "end": 54 },
    "explanation": "directly affected by ADD at line 185"
  }
}
```

**Variable affected by multiple modifications:**
```json
{
  "WS-TOTAL-AMOUNT": {
    "defined_in_record": "WS-TOTALS",
    "base_record": "WS-TOTALS",
    "position": { "start": 1, "end": 15 },
    "explanation": "affected by multiple modifications at lines 142, 156, 189"
  }
}
```

**Variable in REDEFINES record:**
```json
{
  "PAY-CASH": {
    "defined_in_record": "PAYMENT-DETAIL",
    "base_record": "TRANSACTION-RECORD",
    "position": { "start": 25, "end": 34 },
    "explanation": "directly affected by MOVE at line 201; some modifications are due to REDEFINE (e.g., PAY-BANK-CODE at positions 15-24 overlaps TRAN-AMOUNT at positions 11-20)"
  }
}
```

The `defined_in_record` shows that `PAY-CASH` is structurally within `PAYMENT-DETAIL`, while `base_record` shows that `PAYMENT-DETAIL REDEFINES TRANSACTION-RECORD`, so the ultimate memory allocation is under `TRANSACTION-RECORD`.

**Variable affected via ancestor modification:**
```json
{
  "WS-TOTAL-PAYMENTS": {
    "defined_in_record": "WS-TOTALS",
    "base_record": "WS-TOTALS",
    "position": { "start": 16, "end": 30 },
    "explanation": "indirectly affected via ancestor WS-TOTALS modification at line 120"
  }
}
```

**77-level variable:**
```json
{
  "WS-STANDALONE": {
    "defined_in_record": "WS-STANDALONE",
    "base_record": "WS-STANDALONE",
    "position": { "start": 1, "end": 10 },
    "77-level-var": true,
    "explanation": "directly affected by MOVE at line 95"
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

### Explanation Formats

The `explanation` property provides a human-readable description of why each variable may change. The format varies based on the type of modification:

#### Direct Modification (Single)

When a variable is directly modified by a single COBOL statement:

```
directly affected by {MODIFICATION_TYPE} at line {LINE}
```

**Example:** `"directly affected by MOVE at line 142"`

#### Direct Modification (Multiple)

When a variable is directly modified by multiple COBOL statements:

```
affected by multiple modifications at lines {LINE1}, {LINE2}, {LINE3}
```

**Example:** `"affected by multiple modifications at lines 142, 156, 189"`

#### REDEFINES Modification

When modifications include REDEFINES-related memory overlaps, the explanation includes an example with byte positions (1-indexed, COBOL convention):

```
{base explanation}; some modifications are due to REDEFINE (e.g., {VAR1} at positions {START1}-{END1} overlaps {VAR2} at positions {START2}-{END2})
```

**Example:** `"directly affected by MOVE at line 201; some modifications are due to REDEFINE (e.g., PAY-BANK-CODE at positions 15-24 overlaps TRAN-AMOUNT at positions 11-20)"`

#### Ancestor Modification

When a variable is affected because its parent group was modified:

```
indirectly affected via ancestor {ANCESTOR_NAME} modification at line {LINE}
```

**Example:** `"indirectly affected via ancestor WS-TOTALS modification at line 120"`

#### Combined Explanations

Variables may have explanations that combine multiple types:

```
affected by multiple modifications at lines 142, 156; some modifications are due to REDEFINE (e.g., PAY-AMOUNT at positions 5-12 overlaps TRAN-CODE at positions 1-8)
```

---

## Variable Index (API Only)

The `variable_index` is an inverted index available in the `AnalysisResult` returned by the `analyze_paragraph_variables()` API. It enables O(1) lookup from a `DataDivisionTree` node to the list of paragraphs that may modify that variable.

**Note:** This index is only available via the API, not in the JSON file output.

### Structure

```python
{
    "defined_in_record": {
        "start:end": {
            "variable_name": "VAR-NAME",
            "paragraphs": ["PARA-1", "PARA-2", ...]
        }
    }
}
```

| Key Level | Type | Description |
|-----------|------|-------------|
| First level | string | The `defined_in_record` value (Level 01 record containing the variable) |
| Second level | string | Position key in format `"start:end"` (1-indexed byte positions) |
| `variable_name` | string | Name of the variable at this position |
| `paragraphs` | array | List of paragraph/section names that may modify this variable |

### Example

```json
{
  "WS-EMPLOYEE-RECORD": {
    "1:5": {
      "variable_name": "WS-EMP-ID",
      "paragraphs": ["INIT-PARA", "PROCESS-PARA"]
    },
    "6:35": {
      "variable_name": "WS-EMP-NAME",
      "paragraphs": ["UPDATE-NAME-PARA"]
    }
  },
  "CUSTOMER-RECORD": {
    "1:10": {
      "variable_name": "CUST-ID",
      "paragraphs": ["3100-APPLY-PAYMENT"]
    }
  }
}
```

### Usage

```python
from pathlib import Path
from src import get_data_division_tree, analyze_paragraph_variables

# Get both data structures
tree = get_data_division_tree(Path("program.cob"))
result = analyze_paragraph_variables(Path("program.cob"))

# Select a node from the tree (e.g., user clicks on WS-EMP-ID)
node = selected_data_item_node

# Look up modifying paragraphs
if node.defined_in_record and node.position:
    key = f"{node.position['start']}:{node.position['end']}"
    entry = result.variable_index.get(node.defined_in_record, {}).get(key)
    if entry:
        print(f"Paragraphs that modify {entry['variable_name']}: {entry['paragraphs']}")
```

### Why `defined_in_record` Matters

When two Level 01 records REDEFINE each other, they share memory but have different variables. Using `defined_in_record` ensures you only get modifications to the specific variable you selected:

| Record | Variable | Position | Paragraphs |
|--------|----------|----------|------------|
| `CLIENT` | `CLIENT-NAME` | 1:30 | `["UPDATE-CLIENT"]` |
| `CLIENT-CHINA` (REDEFINES CLIENT) | `MIDDLE-NAME` | 1:30 | `["PROCESS-CHINA"]` |

Without `defined_in_record`, position alone (1:30) would be ambiguous.

---

## DATA DIVISION Tree Output

The DATA DIVISION tree output provides a hierarchical view of all data items in the COBOL DATA DIVISION, organized by section.

### Top-Level Properties

| Property | Type | Description |
|----------|------|-------------|
| `program_name` | string | The PROGRAM-ID from the COBOL source |
| `sections` | array | List of DATA DIVISION sections with their records |
| `all_records` | array | Flat list of all Level 01 records (regardless of section) |
| `summary` | object | Statistical summary of the data structure |
| `execution_time_seconds` | number | Time taken to generate the tree |
| `source_info` | object | *(Optional)* Source file metadata (when `include_source_info=True`) |

### Example Structure

```json
{
  "program_name": "SIMPLE-PROGRAM",
  "sections": [ ... ],
  "all_records": [ ... ],
  "summary": { ... },
  "execution_time_seconds": 0.0023,
  "source_info": { ... }
}
```

---

### `sections`

An array of DATA DIVISION section objects. Sections are ordered: FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE, then any others.

#### Section Object Properties

| Property | Type | Description |
|----------|------|-------------|
| `name` | string | Section name (e.g., "WORKING-STORAGE", "FILE", "LINKAGE") |
| `records` | array | List of Level 01 record nodes in this section |

```json
{
  "sections": [
    {
      "name": "WORKING-STORAGE",
      "records": [
        { /* DataItemNode */ },
        { /* DataItemNode */ }
      ]
    }
  ]
}
```

---

### DataItemNode Properties

Each data item in the tree (including records and their subordinates) has these properties:

| Property | Type | Required | Description |
|----------|------|----------|-------------|
| `name` | string | Yes | Data item name (e.g., "WS-EMPLOYEE-RECORD") |
| `level` | integer | Yes | COBOL level number (01-49, 66, 77, 88) |
| `picture` | string | No | PICTURE clause if present |
| `usage` | string | No | USAGE clause if present |
| `value` | string | No | VALUE clause if present |
| `occurs` | integer | No | OCCURS count if present |
| `occurs_depending_on` | string | No | OCCURS DEPENDING ON variable name if present |
| `redefines` | string | No | Name of item being redefined if REDEFINES clause present |
| `is_group` | boolean | No | Present and `true` if this is a group item |
| `is_filler` | boolean | No | Present and `true` if this is a FILLER item |
| `line_number` | integer | No | Line number in source (after COPY expansion) |
| `copybook_source` | string | No | Name of copybook if item came from COPY statement |
| `position` | object | No | Memory position info (see below) |
| `defined_in_record` | string | No | Name of the Level 01 record containing this item (see below) |
| `children` | array | No | Child DataItemNode objects (only present if non-empty) |

#### defined_in_record Field

The `defined_in_record` field provides the name of the Level 01 record that directly contains this data item. This field enables linking between `DataDivisionTree` and `paragraph_variables` output:

- For Level 01 records: equals the record's own name
- For nested items: equals the parent Level 01 record's name
- For items in REDEFINES records: equals the REDEFINING record name (not the base record)

This field is particularly useful when you have two Level 01 records that REDEFINE each other (e.g., `CLIENT` and `CLIENT-CHINA REDEFINES CLIENT`). Each item's `defined_in_record` indicates which specific record structure it belongs to, allowing you to look up only the explicit modifications to that variable in `paragraph_variables`.

**Example:** To find paragraphs that modify a variable from `DataDivisionTree`:
```python
# Given a DataItemNode from the tree
node = selected_data_item_node

# Build lookup key
key = f"{node.position['start']}:{node.position['end']}"

# Look up in paragraph_variables index (keyed by defined_in_record)
paragraphs = variable_index[node.defined_in_record][key]["paragraphs"]
```

#### Position Object

| Property | Type | Description |
|----------|------|-------------|
| `start` | integer | Start byte position (1-indexed, COBOL convention) |
| `end` | integer | End byte position (inclusive) |
| `size` | integer | Size in bytes |

#### Example DataItemNode

```json
{
  "name": "WS-EMPLOYEE-RECORD",
  "level": 1,
  "is_group": true,
  "line_number": 5,
  "position": {
    "start": 1,
    "end": 54,
    "size": 54
  },
  "defined_in_record": "WS-EMPLOYEE-RECORD",
  "children": [
    {
      "name": "WS-EMP-ID",
      "level": 5,
      "picture": "9(5)",
      "line_number": 8,
      "position": {
        "start": 1,
        "end": 5,
        "size": 5
      },
      "defined_in_record": "WS-EMPLOYEE-RECORD"
    },
    {
      "name": "WS-EMP-NAME",
      "level": 5,
      "picture": "X(30)",
      "line_number": 9,
      "position": {
        "start": 6,
        "end": 35,
        "size": 30
      },
      "defined_in_record": "WS-EMPLOYEE-RECORD"
    }
  ]
}
```

#### Example with 88-Level Condition Names

```json
{
  "name": "WS-EOF-FLAG",
  "level": 5,
  "picture": "X",
  "value": "'N'",
  "line_number": 18,
  "position": {
    "start": 1,
    "end": 1,
    "size": 1
  },
  "children": [
    {
      "name": "EOF-REACHED",
      "level": 88,
      "value": "'Y'",
      "line_number": 19
    },
    {
      "name": "NOT-EOF",
      "level": 88,
      "value": "'N'",
      "line_number": 20
    }
  ]
}
```

#### Example with REDEFINES

```json
{
  "name": "EMPLOYEE-RECORD",
  "level": 1,
  "redefines": "INPUT-RECORD",
  "is_group": true,
  "line_number": 10,
  "position": {
    "start": 1,
    "end": 100,
    "size": 100
  },
  "defined_in_record": "EMPLOYEE-RECORD",
  "children": [ ... ]
}
```

#### Example with Copybook Source

```json
{
  "name": "WS-EMPLOYEE",
  "level": 1,
  "is_group": true,
  "line_number": 8,
  "copybook_source": "EMPLOYEE-CPY",
  "position": {
    "start": 1,
    "end": 35,
    "size": 35
  },
  "defined_in_record": "WS-EMPLOYEE",
  "children": [
    {
      "name": "WS-EMP-ID",
      "level": 5,
      "picture": "9(5)",
      "line_number": 9,
      "copybook_source": "EMPLOYEE-CPY",
      "position": {
        "start": 1,
        "end": 5,
        "size": 5
      },
      "defined_in_record": "WS-EMPLOYEE"
    }
  ]
}
```

---

### `summary` (DATA DIVISION Tree)

Statistical overview of the data structure.

| Property | Type | Description |
|----------|------|-------------|
| `total_records` | integer | Number of Level 01 records |
| `total_items` | integer | Total count of all data items (including children) |
| `group_items` | integer | Count of group items (items with non-88-level children) |
| `elementary_items` | integer | Count of elementary items (items without children) |
| `filler_items` | integer | Count of FILLER items |
| `level_88_items` | integer | Count of 88-level condition names |

#### Example

```json
{
  "summary": {
    "total_records": 3,
    "total_items": 12,
    "group_items": 3,
    "elementary_items": 9,
    "filler_items": 0,
    "level_88_items": 2
  }
}
```

---

### Complete DATA DIVISION Tree Example

```json
{
  "program_name": "SIMPLE-PROGRAM",
  "sections": [
    {
      "name": "WORKING-STORAGE",
      "records": [
        {
          "name": "WS-EMPLOYEE-RECORD",
          "level": 1,
          "is_group": true,
          "line_number": 5,
          "position": { "start": 1, "end": 54, "size": 54 },
          "children": [
            {
              "name": "WS-EMP-ID",
              "level": 5,
              "picture": "9(5)",
              "line_number": 8,
              "position": { "start": 1, "end": 5, "size": 5 }
            },
            {
              "name": "WS-EMP-NAME",
              "level": 5,
              "picture": "X(30)",
              "line_number": 9,
              "position": { "start": 6, "end": 35, "size": 30 }
            }
          ]
        },
        {
          "name": "WS-FLAGS",
          "level": 1,
          "is_group": true,
          "line_number": 16,
          "position": { "start": 1, "end": 1, "size": 1 },
          "children": [
            {
              "name": "WS-EOF-FLAG",
              "level": 5,
              "picture": "X",
              "value": "'N'",
              "line_number": 18,
              "position": { "start": 1, "end": 1, "size": 1 },
              "children": [
                {
                  "name": "EOF-REACHED",
                  "level": 88,
                  "value": "'Y'",
                  "line_number": 19
                },
                {
                  "name": "NOT-EOF",
                  "level": 88,
                  "value": "'N'",
                  "line_number": 20
                }
              ]
            }
          ]
        }
      ]
    }
  ],
  "all_records": [
    { /* same records as above, flattened */ }
  ],
  "summary": {
    "total_records": 2,
    "total_items": 7,
    "group_items": 2,
    "elementary_items": 5,
    "filler_items": 0,
    "level_88_items": 2
  },
  "execution_time_seconds": 0.0023,
  "source_info": {
    "file_path": "/path/to/simple_program.cob",
    "file_name": "simple_program.cob",
    "source_format": "fixed",
    "lines_count": 49
  }
}
```

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
    "PROCEDURE DIVISION": {
      "WS-INIT-FLAG": {
        "defined_in_record": "WS-FLAGS",
        "base_record": "WS-FLAGS",
        "position": { "start": 1, "end": 1 },
        "explanation": "direct modification: MOVE at line 95"
      }
    },
    "3100-APPLY-PAYMENT": {
      "CUST-BALANCE": {
        "defined_in_record": "CUSTOMER-RECORD",
        "base_record": "CUSTOMER-RECORD",
        "position": { "start": 45, "end": 54 },
        "explanation": "directly affected by ADD at line 185"
      },
      "PAY-CASH": {
        "defined_in_record": "PAYMENT-DETAIL",
        "base_record": "TRANSACTION-RECORD",
        "position": { "start": 25, "end": 34 },
        "explanation": "directly affected by MOVE at line 201; some modifications are due to REDEFINE (e.g., PAY-BANK-CODE at positions 15-24 overlaps TRAN-AMOUNT at positions 11-20)"
      },
      "WS-STANDALONE": {
        "defined_in_record": "WS-STANDALONE",
        "base_record": "WS-STANDALONE",
        "position": { "start": 1, "end": 10 },
        "77-level-var": true,
        "explanation": "directly affected by MOVE at line 195"
      }
    },
    "1000-INIT": {
      "WS-TOTALS": {
        "defined_in_record": "WS-TOTALS",
        "base_record": "WS-TOTALS",
        "position": { "start": 1, "end": 50 },
        "explanation": "directly affected by INITIALIZE at line 120"
      },
      "WS-TOTAL-PAYMENTS": {
        "defined_in_record": "WS-TOTALS",
        "base_record": "WS-TOTALS",
        "position": { "start": 1, "end": 15 },
        "explanation": "indirectly affected via ancestor WS-TOTALS modification at line 120"
      },
      "WS-TOTAL-REFUNDS": {
        "defined_in_record": "WS-TOTALS",
        "base_record": "WS-TOTALS",
        "position": { "start": 16, "end": 30 },
        "explanation": "indirectly affected via ancestor WS-TOTALS modification at line 120"
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
