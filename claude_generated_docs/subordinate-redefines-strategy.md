# Subordinate-Level REDEFINES Impact Analysis Strategy

## Problem Statement

When a variable's value changes, we need to identify **all variables** that could potentially be affected due to REDEFINES clauses at **any level** (not just Level 01).

### Current Behavior

Currently, the system tracks REDEFINES at the **record level (Level 01)**:

```cobol
01 RECORD-A.
   05 FIELD-A PIC X(10).
01 RECORD-B REDEFINES RECORD-A.
   05 FIELD-B PIC X(10).
```

When FIELD-A changes → Output: `affected_records: ["RECORD-A", "RECORD-B"]`

### Desired Enhancement

Track REDEFINES at **subordinate levels** (05, 10, etc.) and report affected **variables**:

```cobol
01 MYLEVEL-01.
   05 MYLEVEL-05.
      10 MYVAR-10 PIC 9(5).
   05 MYLEVEL-05-REDEF REDEFINES MYLEVEL-05.
      10 OTHER-VAR-10 PIC X(5).
```

When MYVAR-10 changes → Output should include: `affected_variables: ["OTHER-VAR-10"]`

---

## Feasibility Assessment

**YES, this is fully possible** with the current architecture because:

1. **REDEFINES are already parsed at all levels** - The parser captures REDEFINES clauses whether they're at Level 01, 05, 10, or any other level
2. **Hierarchy is preserved** - Each DataItem knows its parent and children
3. **Root record traversal exists** - `root_record` property walks up to Level 01
4. **Graph infrastructure exists** - NetworkX is already integrated for relationship analysis

---

## Technical Strategy

### Phase 1: Extend Data Model

#### 1.1 Add Memory Overlap Concept

Variables share memory when:
1. One variable REDEFINES another (direct)
2. A variable's **ancestor** is redefined (indirect - your use case)
3. A variable is a **descendant** of a redefined item (indirect)

```python
@dataclass
class MemoryOverlapInfo:
    """Tracks which variables share the same memory region."""
    variable_name: str
    overlapping_variables: Set[str]
    overlap_reason: str  # "direct_redefines", "ancestor_redefines", "descendant_of_redefines"
    redefines_level: int  # Level at which REDEFINES occurs
```

#### 1.2 Extend VariableImpact Output

```python
@dataclass
class VariableImpact:
    variable_name: str
    affected_records: List[str]
    modification_type: str
    line_number: int
    # NEW FIELDS:
    affected_variables: List[str]  # Variables that share memory
    redefines_context: List[Dict]  # Details about each REDEFINES affecting this variable
```

### Phase 2: Enhance RedefinesAnalyzer

#### 2.1 New Method: Find Ancestor REDEFINES

For any variable, find all REDEFINES in its ancestor chain:

```python
def get_ancestor_redefines(self, variable_name: str) -> List[RedefinesRelation]:
    """
    Walk up the hierarchy from variable to Level 01.
    Collect all REDEFINES relationships encountered.

    Example:
        MYVAR-10 (level 10)
        └── parent: MYLEVEL-05 (level 05) ← REDEFINES ORIGINAL-05
            └── parent: MYLEVEL-01 (level 01)

    Returns: [RedefinesRelation(MYLEVEL-05 REDEFINES ORIGINAL-05)]
    """
    item = self._get_item(variable_name)
    ancestor_redefines = []

    current = item
    while current is not None:
        if current.redefines:
            redefined = self._get_item(current.redefines)
            if redefined:
                ancestor_redefines.append(RedefinesRelation(
                    redefining_item=current,
                    redefined_item=redefined,
                    level=current.level
                ))
        current = current.parent

    return ancestor_redefines
```

#### 2.2 New Method: Find Variables Sharing Memory

```python
def get_overlapping_variables(self, variable_name: str) -> Set[str]:
    """
    Find all variables that share memory with the given variable.

    Algorithm:
    1. Get all ancestor REDEFINES for the variable
    2. For each ancestor REDEFINES found:
       a. Find the redefined item and all items that redefine it
       b. Collect all subordinates of those items
       c. Filter to subordinates at same relative depth/position
    3. Return union of all overlapping variables
    """
```

#### 2.3 Memory Position Calculation

To determine which subordinate variables truly overlap, calculate byte offsets:

```python
def calculate_memory_position(self, item: DataItem) -> Tuple[int, int]:
    """
    Calculate (start_offset, size) relative to the item's root record.

    Uses PICTURE clause to determine size:
    - PIC X(10) → 10 bytes
    - PIC 9(5) → 5 bytes
    - PIC S9(5)V99 → 7 bytes

    Walks up parent chain to sum offsets.
    """
```

### Phase 3: Build Subordinate REDEFINES Graph

#### 3.1 Graph Structure

Create a second graph for subordinate-level relationships:

```python
class RedefinesAnalyzer:
    def __init__(self, program: CobolProgram):
        self.record_graph = nx.Graph()       # Existing: Level 01 relationships
        self.subordinate_graph = nx.Graph()  # NEW: All-level relationships
        self.memory_map: Dict[str, MemoryRegion] = {}  # NEW: Variable → memory region
```

#### 3.2 Subordinate Graph Nodes

Each node represents a variable with its memory position:

```python
class SubordinateNode:
    name: str
    level: int
    root_record: str
    relative_offset: int
    size: int
```

#### 3.3 Subordinate Graph Edges

Connect variables that share memory:

```python
def _build_subordinate_graph(self):
    """
    Build graph where edges connect variables sharing memory.

    For each REDEFINES at any level:
    1. Get all subordinates of redefining item
    2. Get all subordinates of redefined item
    3. Match by memory position overlap
    4. Add edges between overlapping pairs
    """
```

### Phase 4: Integration with Impact Analysis

#### 4.1 Enhanced Output Format

```json
{
  "sections_and_paragraphs": {
    "PROCESS-DATA": [
      {
        "variable": "MYVAR-10",
        "affected_records": ["MYLEVEL-01"],
        "affected_variables": [
          {
            "name": "OTHER-VAR-10",
            "reason": "ancestor_redefines",
            "redefines_at": "MYLEVEL-05-REDEF REDEFINES MYLEVEL-05",
            "redefines_level": 5
          }
        ],
        "modification_type": "MOVE",
        "line_number": 42
      }
    ]
  }
}
```

#### 4.2 Compact Output Option

For simpler use cases, provide a flat list:

```json
{
  "PROCESS-DATA": [
    {
      "variable": "MYVAR-10",
      "potentially_affected": ["OTHER-VAR-10", "ANOTHER-VAR-10"],
      "affected_records": ["MYLEVEL-01"]
    }
  ]
}
```

---

## Implementation Plan

### Step 1: Add Memory Position Calculation (Low Risk)

**File:** `src/analyzers/data_analyzer.py`

Add methods to calculate byte offsets from PICTURE clauses:
- `_parse_picture_size(picture: str) -> int`
- `_calculate_item_offset(item: DataItem) -> int`
- `get_memory_region(variable_name: str) -> Tuple[int, int]`

### Step 2: Extend RedefinesAnalyzer (Medium Risk)

**File:** `src/analyzers/redefines.py`

Add new methods:
- `get_ancestor_redefines(variable_name: str) -> List[RedefinesRelation]`
- `get_items_redefining_ancestor(variable_name: str) -> List[DataItem]`
- `_build_subordinate_graph()`
- `get_overlapping_variables(variable_name: str) -> Set[str]`

### Step 3: Update Impact Analyzer Output (Low Risk)

**File:** `src/analyzers/impact_analyzer.py`

- Add `affected_variables` field to output
- Call new RedefinesAnalyzer methods
- Support both detailed and compact output formats

### Step 4: Add Configuration Option (Low Risk)

**File:** `src/main.py` and `config/settings.yaml`

Add flag to enable/disable subordinate REDEFINES analysis:
```yaml
analysis:
  subordinate_redefines: true  # Enable detailed variable-level tracking
  output_format: "detailed"    # or "compact"
```

### Step 5: Comprehensive Testing

**File:** `tests/test_subordinate_redefines.py`

Test cases:
1. Simple subordinate REDEFINES (05 level)
2. Nested subordinate REDEFINES (05 and 10 levels)
3. Multiple REDEFINES of same item
4. REDEFINES chains (A REDEFINES B, C REDEFINES B)
5. Mixed record-level and subordinate REDEFINES

---

## Algorithm Deep Dive: Finding Affected Variables

### Example Structure

```cobol
01 EMPLOYEE-RECORD.
   05 EMP-ID              PIC 9(6).
   05 EMP-NAME.
      10 EMP-FIRST-NAME   PIC X(20).
      10 EMP-LAST-NAME    PIC X(20).
   05 EMP-NAME-FULL REDEFINES EMP-NAME PIC X(40).
   05 EMP-SALARY          PIC 9(7)V99.
```

### When EMP-FIRST-NAME Changes

**Step 1:** Get ancestor chain for EMP-FIRST-NAME:
```
EMP-FIRST-NAME (10)
  └── EMP-NAME (05) ← Has REDEFINES relationship!
      └── EMPLOYEE-RECORD (01)
```

**Step 2:** Find REDEFINES at Level 05:
- EMP-NAME-FULL REDEFINES EMP-NAME

**Step 3:** Determine memory overlap:
- EMP-NAME spans bytes 7-46 (relative to record start)
- EMP-FIRST-NAME spans bytes 7-26 (first 20 bytes of EMP-NAME)
- EMP-NAME-FULL spans bytes 7-46 (same as EMP-NAME)
- EMP-FIRST-NAME overlaps with bytes 7-26 of EMP-NAME-FULL

**Step 4:** Report:
```json
{
  "variable": "EMP-FIRST-NAME",
  "affected_variables": ["EMP-NAME-FULL"],
  "overlap_bytes": [7, 26]
}
```

### When EMP-SALARY Changes

**Step 1:** Get ancestor chain:
```
EMP-SALARY (05)
  └── EMPLOYEE-RECORD (01)
```

**Step 2:** No REDEFINES in ancestor chain at subordinate levels.

**Step 3:** Report (no affected variables at subordinate level):
```json
{
  "variable": "EMP-SALARY",
  "affected_variables": [],
  "affected_records": ["EMPLOYEE-RECORD"]
}
```

---

## Edge Cases to Handle

### 1. REDEFINES Chain

```cobol
05 FIELD-A PIC X(10).
05 FIELD-B REDEFINES FIELD-A PIC X(10).
05 FIELD-C REDEFINES FIELD-A PIC X(10).
```

All three (FIELD-A, FIELD-B, FIELD-C) share memory. Changing any affects all others.

### 2. Partial Overlap

```cobol
05 FIELD-GROUP.
   10 FIELD-PART1 PIC X(5).
   10 FIELD-PART2 PIC X(5).
05 FIELD-FULL REDEFINES FIELD-GROUP PIC X(10).
```

- FIELD-PART1 overlaps with first 5 bytes of FIELD-FULL
- FIELD-PART2 overlaps with last 5 bytes of FIELD-FULL

### 3. Nested Group REDEFINES

```cobol
01 RECORD.
   05 OUTER-GROUP.
      10 INNER-GROUP.
         15 DEEPEST PIC X(5).
      10 INNER-REDEF REDEFINES INNER-GROUP.
         15 OTHER-DEEPEST PIC X(5).
   05 OUTER-REDEF REDEFINES OUTER-GROUP.
      10 FLAT-VIEW PIC X(5).
```

When DEEPEST changes:
- OTHER-DEEPEST affected (via INNER-REDEF)
- FLAT-VIEW affected (via OUTER-REDEF)

### 4. OCCURS with REDEFINES

```cobol
05 TABLE-DATA.
   10 TABLE-ENTRY OCCURS 10 PIC X(10).
05 TABLE-REDEF REDEFINES TABLE-DATA PIC X(100).
```

Any TABLE-ENTRY(n) change affects TABLE-REDEF.

---

## Output Examples

### Detailed Output

```json
{
  "program_name": "EMPLOYEE-PROC",
  "subordinate_redefines_analysis": true,
  "sections_and_paragraphs": {
    "UPDATE-EMPLOYEE": [
      {
        "variable": "EMP-FIRST-NAME",
        "line_number": 156,
        "modification_type": "MOVE",
        "affected_records": ["EMPLOYEE-RECORD"],
        "affected_variables": [
          {
            "name": "EMP-NAME-FULL",
            "overlap_type": "full",
            "redefines_chain": "EMP-NAME-FULL REDEFINES EMP-NAME",
            "redefines_level": 5
          }
        ]
      }
    ]
  }
}
```

### Compact Output

```json
{
  "UPDATE-EMPLOYEE": [
    {
      "variable": "EMP-FIRST-NAME",
      "potentially_affected": ["EMP-NAME-FULL"],
      "records": ["EMPLOYEE-RECORD"]
    }
  ]
}
```

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Performance with large programs | Medium | Medium | Cache memory calculations; lazy evaluation |
| PICTURE parsing edge cases | Medium | Low | Comprehensive test suite; fallback to conservative estimates |
| Complex OCCURS scenarios | Low | Medium | Handle OCCURS as multiplier; document limitations |
| Breaking existing output | Low | High | New fields only; preserve backward compatibility |

---

## Success Criteria

1. **Correctness:** For any modified variable, all truly overlapping variables are reported
2. **No false negatives:** Never miss a variable that shares memory
3. **Minimal false positives:** Only report variables that actually overlap
4. **Performance:** Analysis completes in < 2x current time
5. **Backward compatibility:** Existing output format unchanged; new fields added

---

## Summary

This enhancement is **fully achievable** with the current architecture. The key insight is that we already have:

1. REDEFINES information at all levels (parsed and stored)
2. Complete parent-child hierarchy (traversable)
3. Graph analysis infrastructure (NetworkX)

We need to add:

1. Memory position calculation from PICTURE clauses
2. Ancestor chain REDEFINES detection
3. Variable-to-variable overlap mapping
4. Enhanced output format

The implementation can be done incrementally with minimal risk to existing functionality.
