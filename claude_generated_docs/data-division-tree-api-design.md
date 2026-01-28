# DATA DIVISION Tree View API Design

## Overview

This document describes the design for a new API that returns a hierarchical tree view of COBOL DATA DIVISION variables, suitable for client-side visualization.

## Requirements

The API must:
1. Accept a COBOL program path and copybook folder paths as input
2. Parse and understand the hierarchical structure of DATA DIVISION variables
3. Return a tree structure that clients can use to visualize the variable hierarchy
4. Include relevant metadata per variable (level, picture, occurs, redefines, etc.)

## API Design

### Function Signature

```python
def get_data_division_tree(
    source_path: Path,
    options: Optional[TreeOptions] = None,
) -> DataDivisionTree:
```

### Why a New Function?

We recommend creating a **new function** rather than extending `analyze_paragraph_variables()` because:
- The concerns are distinct (paragraph-to-variable mappings vs. data structure visualization)
- Keeps each function focused (Single Responsibility Principle)
- Allows independent evolution of each API

### Options Dataclass

```python
@dataclass
class TreeOptions:
    """Options for DATA DIVISION tree generation.

    Attributes:
        copybook_paths: Additional paths to search for copybooks.
        resolve_copies: Whether to resolve COPY statements (default: True).
        include_filler: Include FILLER items in output (default: True).
        include_88_levels: Include 88-level condition names (default: True).
        include_source_info: Include source file metadata (default: True).
    """
    copybook_paths: Optional[List[Path]] = None
    resolve_copies: bool = True
    include_filler: bool = True
    include_88_levels: bool = True
    include_source_info: bool = True
```

### Node Dataclass

```python
@dataclass
class DataItemNode:
    """A node in the DATA DIVISION tree.

    Attributes:
        name: Variable name (uppercase).
        level: COBOL level number (01-49, 66, 77, 88).
        picture: PIC clause if present.
        usage: USAGE clause if present.
        value: VALUE clause if present.
        occurs: OCCURS count if present.
        occurs_depending_on: Variable name for OCCURS DEPENDING ON.
        redefines: Name of redefined item if present.
        is_group: True if this is a group item.
        is_filler: True if this is a FILLER item.
        line_number: Source line number.
        copybook_source: Copybook name if defined in a copybook.
        position: Memory position within record (start, end, size).
        children: List of child DataItemNode objects.
    """
    name: str
    level: int
    picture: Optional[str] = None
    usage: Optional[str] = None
    value: Optional[str] = None
    occurs: Optional[int] = None
    occurs_depending_on: Optional[str] = None
    redefines: Optional[str] = None
    is_group: bool = False
    is_filler: bool = False
    line_number: int = 0
    copybook_source: Optional[str] = None
    position: Optional[Dict[str, int]] = None
    children: List["DataItemNode"] = field(default_factory=list)
```

### Section Dataclass

```python
@dataclass
class DataDivisionSection:
    """A section within the DATA DIVISION.

    Attributes:
        name: Section name (e.g., "WORKING-STORAGE", "FILE", "LINKAGE").
        records: List of Level 01 records in this section.
    """
    name: str
    records: List[DataItemNode] = field(default_factory=list)
```

### Result Dataclass

```python
@dataclass
class DataDivisionTree:
    """Result of DATA DIVISION tree generation.

    Attributes:
        program_name: Name of the COBOL program.
        sections: List of DATA DIVISION sections with their records.
        all_records: Flat list of all Level 01 records (alternative view).
        summary: Statistics about the data structure.
        source_info: Source file metadata (if requested).
        execution_time_seconds: Time to generate the tree.
    """
    program_name: str
    sections: List[DataDivisionSection]
    all_records: List[DataItemNode]
    summary: Dict[str, Any]
    execution_time_seconds: float
    source_info: Optional[Dict[str, Any]] = None
```

## Output Structure

### Why Nested Children Arrays?

We chose **nested dict with `children` arrays** because:
- Natural representation of COBOL's hierarchical structure
- Easy to traverse in client-side tree components (React, Vue, etc.)
- Self-contained nodes (no need to resolve parent references)
- Standard tree representation used by UI component libraries

### Example JSON Output

```json
{
  "program_name": "SAMPLE-PROGRAM",
  "sections": [
    {
      "name": "WORKING-STORAGE",
      "records": [
        {
          "name": "WS-EMPLOYEE-RECORD",
          "level": 1,
          "is_group": true,
          "line_number": 7,
          "position": {"start": 1, "end": 52, "size": 52},
          "children": [
            {
              "name": "WS-EMP-ID",
              "level": 5,
              "is_group": false,
              "picture": "9(5)",
              "line_number": 8,
              "position": {"start": 1, "end": 5, "size": 5}
            },
            {
              "name": "WS-EMP-NAME",
              "level": 5,
              "is_group": false,
              "picture": "X(30)",
              "line_number": 9,
              "position": {"start": 6, "end": 35, "size": 30}
            },
            {
              "name": "WS-EMP-ADDRESS",
              "level": 5,
              "is_group": true,
              "line_number": 10,
              "position": {"start": 36, "end": 96, "size": 60},
              "children": [
                {
                  "name": "WS-STREET",
                  "level": 10,
                  "is_group": false,
                  "picture": "X(40)",
                  "line_number": 11,
                  "position": {"start": 36, "end": 75, "size": 40}
                },
                {
                  "name": "WS-CITY",
                  "level": 10,
                  "is_group": false,
                  "picture": "X(20)",
                  "line_number": 12,
                  "position": {"start": 76, "end": 95, "size": 20}
                }
              ]
            }
          ]
        },
        {
          "name": "WS-FLAGS",
          "level": 1,
          "is_group": true,
          "line_number": 15,
          "children": [
            {
              "name": "WS-EOF-FLAG",
              "level": 5,
              "is_group": false,
              "picture": "X",
              "value": "'N'",
              "line_number": 16,
              "children": [
                {
                  "name": "EOF-REACHED",
                  "level": 88,
                  "is_group": false,
                  "value": "'Y'",
                  "line_number": 17
                },
                {
                  "name": "NOT-EOF",
                  "level": 88,
                  "is_group": false,
                  "value": "'N'",
                  "line_number": 18
                }
              ]
            }
          ]
        }
      ]
    }
  ],
  "all_records": [
    {"name": "WS-EMPLOYEE-RECORD", "...": "..."},
    {"name": "WS-FLAGS", "...": "..."}
  ],
  "summary": {
    "total_records": 2,
    "total_data_items": 10,
    "group_items": 4,
    "elementary_items": 4,
    "condition_names_88": 2,
    "items_with_redefines": 0,
    "items_with_occurs": 0,
    "sections": {
      "WORKING-STORAGE": 2,
      "FILE": 0,
      "LINKAGE": 0
    }
  },
  "source_info": {
    "file_path": "/path/to/program.cob",
    "file_name": "program.cob",
    "source_format": "fixed",
    "lines_count": 100
  },
  "execution_time_seconds": 0.0123
}
```

## Usage Examples

### Basic Usage

```python
from api import get_data_division_tree
from pathlib import Path

tree = get_data_division_tree(Path("myprogram.cob"))
print(tree.program_name)

for section in tree.sections:
    print(f"Section: {section.name}")
    for record in section.records:
        print(f"  Record: {record.name}")
        for field in record.children:
            print(f"    Field: {field.name} - {field.picture}")
```

### With Options

```python
from api import get_data_division_tree, TreeOptions
from pathlib import Path

options = TreeOptions(
    copybook_paths=[Path("./copybooks"), Path("./includes")],
    include_filler=False,  # Exclude FILLER items from output
    include_88_levels=False,
)
tree = get_data_division_tree(Path("myprogram.cob"), options)
```

### JSON Serialization

```python
import json
from api import get_data_division_tree
from pathlib import Path

tree = get_data_division_tree(Path("myprogram.cob"))
json_output = json.dumps(tree.to_dict(), indent=2)
```

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| New function vs extend existing | New function | Single responsibility, independent evolution |
| Output structure | Nested children arrays | Natural tree, UI-friendly |
| Grouping by section | Yes, in `sections` list | Matches COBOL structure |
| Return type | Dataclass with `to_dict()` | Type safety + easy JSON serialization |
| Include 88-levels | Optional, default True | Useful for condition visualization |
| Include FILLER | Optional, default True | Required when fields are nested inside FILLER groups |
| Position format | Dict with start/end/size (1-indexed) | User-friendly, consistent with existing |

## Implementation Strategy

### Reusing Existing Infrastructure

This API reuses the existing parsing pipeline from `analyze_paragraph_variables()`. No new parser or AST infrastructure is needed.

#### Components to Reuse

| Component | Location | Purpose |
|-----------|----------|---------|
| `CopyResolver` | `src/preprocessor/` | Resolves COPY statements, tracks line mappings |
| `detect_format`, `normalize_source` | `src/preprocessor/` | Handles fixed/free format detection |
| `CobolParser` | `src/parser/` | ANTLR4-based COBOL parsing |
| `ASTBuilder` | `src/cobol_ast/builder.py` | Builds `CobolProgram` AST from parse tree |
| `DataItem` | `src/cobol_ast/nodes.py` | Existing node with all required attributes |
| `DataStructureAnalyzer` | `src/analyzers/` | Memory position calculations |

#### Existing `DataItem` Attributes (from `nodes.py`)

The `DataItem` class already has everything `DataItemNode` needs:

```python
@dataclass
class DataItem:
    name: str
    level: int
    parent: Optional["DataItem"] = None
    children: List["DataItem"] = field(default_factory=list)
    redefines: Optional[str] = None
    picture: Optional[str] = None
    usage: Optional[str] = None
    value: Optional[str] = None
    occurs: Optional[int] = None
    occurs_depending_on: Optional[str] = None
    line_number: int = 0
    is_filler: bool = False

    @property
    def is_group(self) -> bool: ...
```

#### New Code Required

Only a **tree view transformer** is needed:

```python
# New file: src/output/tree_builder.py

class DataDivisionTreeBuilder:
    """Transforms CobolProgram AST into DataDivisionTree output."""

    def __init__(self, program: CobolProgram, options: TreeOptions):
        self.program = program
        self.options = options

    def build(self) -> DataDivisionTree:
        """Build the tree view from the parsed program."""
        sections = self._build_sections()
        all_records = self._flatten_records(sections)
        summary = self._compute_summary(all_records)
        return DataDivisionTree(
            program_name=self.program.name,
            sections=sections,
            all_records=all_records,
            summary=summary,
            execution_time_seconds=0.0,  # Set by caller
        )

    def _transform_data_item(self, item: DataItem) -> Optional[DataItemNode]:
        """Transform a DataItem to DataItemNode, applying filters."""
        # Skip FILLER if not included
        if item.is_filler and not self.options.include_filler:
            return None
        # Skip 88-levels if not included
        if item.level == 88 and not self.options.include_88_levels:
            return None
        # Recursively transform children
        children = [
            self._transform_data_item(child)
            for child in item.children
        ]
        children = [c for c in children if c is not None]
        return DataItemNode(
            name=item.name,
            level=item.level,
            picture=item.picture,
            # ... map remaining fields
            children=children,
        )
```

#### Implementation Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                    get_data_division_tree()                     │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  REUSE: Preprocessing (CopyResolver, detect_format, normalize) │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  REUSE: CobolParser.parse() → parse_tree                        │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  REUSE: ASTBuilder.build() → CobolProgram                       │
│         (contains record_descriptions with DataItem hierarchy)  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  REUSE: DataStructureAnalyzer (for position calculations)       │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  NEW: DataDivisionTreeBuilder.build() → DataDivisionTree        │
│       - Transforms DataItem → DataItemNode                      │
│       - Applies include_filler / include_88_levels filters      │
│       - Groups by DATA DIVISION section                         │
│       - Computes summary statistics                             │
└─────────────────────────────────────────────────────────────────┘
```

### Code Estimate

| Type | Effort |
|------|--------|
| New dataclasses | ~50 lines (`TreeOptions`, `DataItemNode`, `DataDivisionSection`, `DataDivisionTree`) |
| Tree builder | ~100 lines (transformer + filtering logic) |
| API function | ~50 lines (reuses existing pipeline) |
| Tests | ~150 lines |
| **Total** | **~350 lines** |
