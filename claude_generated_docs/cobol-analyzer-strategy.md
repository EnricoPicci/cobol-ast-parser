# COBOL Source Code Analyzer - Implementation Strategy

## Executive Summary

This document outlines a strategy for building a COBOL source code analyzer that identifies variable modifications within SECTIONs/PARAGRAPHs and maps them to their associated Level 01 record descriptions, including REDEFINES relationships.

---

## 1. Technology Stack

### Recommended: Python with ANTLR4

**Rationale:**

1. **Python** - Excellent text processing, rich data structures, mature ecosystem
2. **ANTLR4** - Industry standard for COBOL parsing with:
   - Mature [COBOL 85 grammar](https://github.com/antlr/grammars-v4/blob/master/cobol85/Cobol85.g4)
   - Python 3 runtime (`antlr4-python3-runtime`)
   - Battle-tested by ProLeap COBOL Parser (passes 680 NIST tests)

### Dependencies

```
antlr4-python3-runtime>=4.13.0
networkx>=3.0          # For REDEFINES graph analysis
pyyaml>=6.0            # For configuration
pytest>=7.0            # For testing
```

---

## 2. High-Level Architecture

```
                    +------------------+
                    |   COBOL Source   |
                    |   + Copybooks    |
                    +--------+---------+
                             |
                             v
              +-----------------------------+
              |   Phase 1: Preprocessor     |
              |   - COPY Resolution         |
              |   - Format Normalization    |
              +-------------+---------------+
                            |
                            v
              +-----------------------------+
              |   Phase 2: ANTLR4 Parser    |
              |   - Lexer + Parser          |
              |   - Concrete Syntax Tree    |
              +-------------+---------------+
                            |
                            v
              +-----------------------------+
              |   Phase 3: AST Builder      |
              |   - Custom Visitor Pattern  |
              |   - Domain-specific AST     |
              +-------------+---------------+
                            |
            +---------------+---------------+
            |                               |
            v                               v
+---------------------+        +------------------------+
| Phase 4A: Data      |        | Phase 4B: Procedure    |
| Structure Analysis  |        | Division Analysis      |
| - Level hierarchy   |        | - Sections/Paragraphs  |
| - REDEFINES graph   |        | - Variable modifications|
+----------+----------+        +-----------+------------+
           |                               |
           +---------------+---------------+
                           |
                           v
              +-----------------------------+
              |   Phase 5: Impact Analysis  |
              |   - Combine data + procedure|
              |   - Generate output dict    |
              +-------------+---------------+
                            |
                            v
                    +------------------+
                    |   JSON Output    |
                    +------------------+
```

---

## 3. Implementation Phases

### Phase 1: Preprocessor Module (COPY Resolution)

**Goal:** Resolve all COPY statements recursively before parsing.

#### 1.1 Source Format Detection

COBOL has two primary formats:

**Fixed Format (traditional):**
- Columns 1-6: Sequence numbers (ignored)
- Column 7: Indicator (`*` = comment, `-` = continuation, `D` = debug)
- Columns 8-11: Area A (divisions, sections, paragraphs, 01/77 levels)
- Columns 12-72: Area B (statements, clauses)
- Columns 73-80: Identification (ignored)

**Free Format (modern):**
- No column restrictions
- `*>` for inline comments

```python
class SourceFormat(Enum):
    FIXED = "fixed"
    FREE = "free"

def detect_format(source_lines: List[str]) -> SourceFormat:
    """Detect source format by analyzing indicator column patterns."""
```

#### 1.2 COPY Statement Resolution

**COPY syntax:**
```cobol
COPY copybook-name [{OF|IN} library-name]
     [REPLACING string-1 BY string-2 ...]
```

**Key features:**
- Recursive resolution (up to 16 levels per COBOL standard)
- Circular dependency detection
- REPLACING clause application
- Support for both data structure and procedural COPYs

```python
class CopyResolver:
    def __init__(self, copybook_paths: List[Path]):
        self.copybook_paths = copybook_paths
        self.resolved_cache: Dict[str, str] = {}
        self.resolution_stack: List[str] = []  # Cycle detection

    def resolve(self, source: str) -> str:
        """Recursively resolve all COPY statements."""
```

#### 1.3 Line Continuation Handling

```python
def normalize_continuations(lines: List[str], format: SourceFormat) -> str:
    """Handle continuation lines (hyphen in column 7 for fixed format)."""
```

---

### Phase 2: ANTLR4 Parser Setup

**Goal:** Generate Python lexer/parser from COBOL grammar.

1. Download [Cobol85.g4](https://github.com/antlr/grammars-v4/blob/master/cobol85/Cobol85.g4)
2. Generate Python parser: `antlr4 -Dlanguage=Python3 -visitor Cobol85.g4`
3. Integrate with Python wrapper

```python
from antlr4 import CommonTokenStream, InputStream
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser

def parse_cobol(source: str) -> Cobol85Parser.StartRuleContext:
    """Parse COBOL source into parse tree."""
```

---

### Phase 3: AST Builder (Domain-Specific)

**Goal:** Transform ANTLR parse tree into cleaner, domain-specific AST.

#### AST Node Definitions

```python
@dataclass
class DataItem:
    """Represents a data item in DATA DIVISION."""
    name: str
    level: int
    parent: Optional['DataItem'] = None
    children: List['DataItem'] = field(default_factory=list)
    redefines: Optional[str] = None
    picture: Optional[str] = None

    @property
    def root_01(self) -> 'DataItem':
        """Get the Level 01 ancestor."""

@dataclass
class RecordDescription:
    """Level 01 record description with all subordinate items."""
    name: str
    items: Dict[str, DataItem] = field(default_factory=dict)
    redefines: Optional[str] = None

@dataclass
class VariableModification:
    """Records a variable modification in procedure code."""
    variable_name: str
    modification_type: ModificationType
    line_number: int

@dataclass
class Paragraph:
    name: str
    modifications: List[VariableModification] = field(default_factory=list)

@dataclass
class Section:
    name: str
    paragraphs: List[Paragraph] = field(default_factory=list)
    standalone_modifications: List[VariableModification] = field(default_factory=list)

@dataclass
class CobolProgram:
    """Complete parsed COBOL program."""
    name: str
    record_descriptions: Dict[str, RecordDescription]
    all_data_items: Dict[str, DataItem]
    sections: List[Section]
    paragraphs: List[Paragraph]
```

---

### Phase 4A: Data Structure Analysis

**Goal:** Build complete data item hierarchy and REDEFINES graph.

#### Variable-to-Level01 Mapping

```python
class DataStructureAnalyzer:
    def build_variable_mapping(self):
        """Map each variable to its Level 01 record."""
```

#### REDEFINES Graph Construction

Uses NetworkX for graph analysis:

```python
class RedefinesAnalyzer:
    def __init__(self, program: CobolProgram):
        self.redefines_graph = nx.Graph()

    def build_graph(self):
        """Build REDEFINES graph - nodes are records, edges are REDEFINES."""

    def get_affected_records(self, variable_name: str) -> Set[str]:
        """Get all Level 01 records affected via connected components."""
```

**Example:**
```cobol
01 RECORD-A.
   05 FIELD-A PIC X(10).
01 RECORD-B REDEFINES RECORD-A.
   05 FIELD-B PIC 9(10).
01 RECORD-C REDEFINES RECORD-A.
   05 FIELD-C PIC X(5).
```

Graph: `RECORD-A -- RECORD-B -- RECORD-C` (all in same connected component)

---

### Phase 4B: Procedure Division Analysis

**Goal:** Extract all variable modifications from SECTIONs/PARAGRAPHs.

#### Modification Statement Types

```python
MODIFYING_STATEMENTS = {
    # Data movement
    'MOVE', 'MOVE CORRESPONDING',

    # Arithmetic
    'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',

    # String operations
    'STRING', 'UNSTRING', 'INSPECT',

    # Input operations
    'ACCEPT', 'READ', 'RETURN',

    # Initialization
    'INITIALIZE', 'SET',

    # Table operations
    'SEARCH',
}
```

Each statement type requires specific visitor logic to extract target variables:

```python
def visitMoveStatement(self, ctx):
    """Track MOVE statement targets (the TO clause variables)."""

def visitComputeStatement(self, ctx):
    """Track COMPUTE statement targets."""

def visitAddStatement(self, ctx):
    """ADD a TO b [GIVING c] - b and c are modified."""
```

---

### Phase 5: Impact Analysis and Output Generation

**Goal:** Combine all analyses to produce final output.

```python
class ImpactAnalyzer:
    def analyze(self) -> Dict[str, List[Dict]]:
        """
        Produce the final output dictionary.

        Returns:
            {
                "SECTION-OR-PARAGRAPH-NAME": [
                    {
                        "variable": "VAR-NAME",
                        "affected_records": ["RECORD-A", "RECORD-B"]
                    },
                    ...
                ],
                ...
            }
        """
```

#### Output Format (JSON)

```json
{
  "program_name": "SAMPLE-PROGRAM",
  "analysis_date": "2024-01-24T10:30:00Z",
  "sections_and_paragraphs": {
    "INIT-SECTION": [
      {
        "variable": "WS-COUNTER",
        "affected_records": ["WORKING-STORAGE-REC"]
      },
      {
        "variable": "EMPLOYEE-NAME",
        "affected_records": ["EMPLOYEE-RECORD", "EMPLOYEE-OVERLAY"]
      }
    ],
    "PROCESS-DATA": [
      {
        "variable": "OUTPUT-FIELD",
        "affected_records": ["OUTPUT-RECORD", "OUTPUT-REDEF"]
      }
    ]
  },
  "summary": {
    "total_sections": 5,
    "total_paragraphs": 23,
    "total_modifications": 156,
    "records_with_redefines": ["EMPLOYEE-RECORD", "OUTPUT-RECORD"]
  }
}
```

---

## 4. Project Structure

```
cobol-ast-parser/
├── src/
│   ├── __init__.py
│   ├── main.py                 # Entry point
│   ├── preprocessor/
│   │   ├── __init__.py
│   │   ├── copy_resolver.py    # COPY statement resolution
│   │   ├── format_detector.py  # Fixed/Free format detection
│   │   └── normalizer.py       # Line continuation, comments
│   ├── parser/
│   │   ├── __init__.py
│   │   ├── generated/          # ANTLR-generated files
│   │   │   ├── Cobol85Lexer.py
│   │   │   ├── Cobol85Parser.py
│   │   │   └── Cobol85Visitor.py
│   │   └── cobol_parser.py     # Parser wrapper
│   ├── ast/
│   │   ├── __init__.py
│   │   ├── nodes.py            # AST node definitions
│   │   └── builder.py          # AST builder visitor
│   ├── analyzers/
│   │   ├── __init__.py
│   │   ├── data_analyzer.py    # DATA DIVISION analysis
│   │   ├── redefines.py        # REDEFINES graph analysis
│   │   ├── procedure_analyzer.py # PROCEDURE DIVISION analysis
│   │   └── impact_analyzer.py  # Combined impact analysis
│   └── output/
│       ├── __init__.py
│       └── json_writer.py      # Output formatting
├── grammar/
│   ├── Cobol85.g4              # ANTLR grammar
│   └── build_parser.sh         # Script to regenerate parser
├── tests/
│   ├── fixtures/               # Test COBOL programs
│   ├── test_preprocessor.py
│   ├── test_parser.py
│   ├── test_data_analyzer.py
│   ├── test_redefines.py
│   ├── test_procedure_analyzer.py
│   └── test_impact_analyzer.py
├── config/
│   └── settings.yaml           # Configuration (copybook paths, etc.)
├── requirements.txt
├── setup.py
└── README.md
```

---

## 5. Testing Strategy

### Unit Tests

- **Preprocessor:** COPY resolution, format detection, continuation handling
- **Parser:** Valid COBOL parsing, error handling
- **Data Analysis:** Level hierarchy, variable mapping
- **REDEFINES:** Simple, chained, and subordinate REDEFINES
- **Procedure Analysis:** All modification statement types

### Integration Tests

- End-to-end with simple programs
- Programs with copybooks
- Complex REDEFINES scenarios

### Test Fixtures

1. Simple program - Basic structure
2. Copybook program - Multiple/nested COPYs
3. REDEFINES program - Various REDEFINES scenarios
4. All modifications - Every modifying statement type
5. Edge cases - FILLER, 66/88 levels, OCCURS DEPENDING ON

---

## 6. Key Technical Challenges

| Challenge | Solution |
|-----------|----------|
| COBOL dialect variations | Start with COBOL-85 standard; extend as needed |
| Complex REDEFINES chains | NetworkX graph connected components |
| Fixed vs free format | Format detection + normalization layer |
| Nested COPY resolution | Recursive resolver with cycle detection |
| All modification types | Comprehensive visitor methods per statement |

---

## 7. Future Enhancements

1. **Control flow analysis** - Track PERFORM statements for execution paths
2. **Variable read analysis** - Track reads, not just modifications
3. **Cross-program analysis** - Handle CALL statements
4. **IDE integration** - Language Server Protocol (LSP) support
5. **Visualization** - Data flow diagrams

---

## References

- [ProLeap COBOL Parser](https://github.com/uwol/proleap-cobol-parser) - ANTLR4-based Java parser
- [ANTLR4 COBOL 85 Grammar](https://github.com/antlr/grammars-v4/blob/master/cobol85/Cobol85.g4)
- [IBM REDEFINES Documentation](https://www.ibm.com/docs/en/cobol-linux-x86/1.1.0?topic=entry-redefines-clause)
