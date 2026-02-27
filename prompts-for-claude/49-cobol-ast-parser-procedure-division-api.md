# Prompt: Add PROCEDURE DIVISION Analysis APIs to cobol-ast-parser

## Context

The `cobol-ast-parser` library is used by an LLM-powered analysis project to analyze COBOL programs. The project transforms COBOL execution traces into structured business documentation using LLM-powered agents.

### Current API surface

The library currently exposes three Python API functions (from `cobol_ast.api`):

1. **`resolve_copybooks(source_path, options)`** — inlines COPY statements, returns resolved source
2. **`analyze_for_paragraphs(source_path, paragraph_names, options)`** — filters DATA DIVISION to only variables referenced by specified paragraphs
3. **`analyze_with_tree(cobol_path, options)`** — returns a `CombinedResult` with:
   - `data_division_tree`: hierarchical DATA DIVISION structure (sections → records → fields with level, PIC, REDEFINES, OCCURS, position, line_number)
   - `analysis_result.variable_index`: maps variables to `modifying_paragraphs` and `accessing_paragraphs` lists

All three focus on the **DATA DIVISION**. The library has no API for analyzing the **PROCEDURE DIVISION** structure.

### The problem

The agentic analysis skill launches parallel LLM agents to analyze COBOL modules. Each agent reads raw COBOL source in 500-line chunks to discover:
- What paragraphs/sections exist in the module
- Which paragraphs PERFORM which other paragraphs
- What conditional branches exist (IF/EVALUATE)
- Which fields are referenced in each paragraph

This text-based discovery is error-prone:
- **Missed paragraphs**: agents scanning 500-line chunks can miss paragraph boundaries, especially near chunk boundaries
- **Incorrect control flow**: PERFORM THRU ranges, GO TO targets, and nested PERFORMs are hard to infer from reading source
- **Incomplete branch coverage**: long EVALUATE blocks with many WHEN clauses get partially missed
- **Wasted context**: agents spend tokens reading infrastructure paragraphs they'll ultimately skip

A deterministic PROCEDURE DIVISION analysis, provided upfront to each agent, would eliminate these errors and improve the quality of the business documentation significantly.

## What to implement

Add a new API function that analyzes the PROCEDURE DIVISION of a COBOL program and returns a structured description of its procedural skeleton: paragraphs, control flow, conditional branches, and per-paragraph field references.

### Proposed API

```python
from cobol_ast.api import analyze_procedure_division, ProcedureDivisionOptions

options = ProcedureDivisionOptions(
    copybook_paths=[Path("path/to/copybooks/")],
    resolve_copies=True,
)

result = analyze_procedure_division(
    source_path=Path("path/to/MODULE.cbl"),
    options=options,
)
```

### Output structure

The result should provide these views of the PROCEDURE DIVISION:

#### 1. Paragraph inventory

A complete list of every paragraph and section in the PROCEDURE DIVISION, with line numbers.

```python
result.paragraphs
# Returns:
[
    {
        "name": "INIT-PROGRAM",
        "type": "paragraph",        # "paragraph" or "section"
        "line_start": 1250,
        "line_end": 1280,
        "line_count": 31,
    },
    {
        "name": "VALORIZZA-DATO-CONTRATTO",
        "type": "paragraph",
        "line_start": 1450,
        "line_end": 1520,
        "line_count": 71,
    },
    ...
]
```

This list must be **exhaustive** — every paragraph and section in the PROCEDURE DIVISION must appear, with no exceptions.

#### 2. PERFORM graph

For each paragraph, the list of other paragraphs it PERFORMs (directly), including PERFORM THRU ranges.

```python
result.perform_graph
# Returns:
{
    "MAIN-PROCESS": {
        "performs": [
            {
                "target": "INIT-PROGRAM",
                "type": "simple",             # "simple", "thru", "until", "times", "varying"
                "line": 1205,
            },
            {
                "target": "PROCESS-POLICY",
                "type": "thru",
                "thru_target": "PROCESS-POLICY-EXIT",
                "thru_includes": ["PROCESS-POLICY", "VALIDATE-DATA", "COMPUTE-PREMIUM", "PROCESS-POLICY-EXIT"],
                "line": 1210,
            },
        ]
    },
    "INIT-PROGRAM": {
        "performs": [
            {
                "target": "SET-DEFAULTS",
                "type": "simple",
                "line": 1255,
            },
        ]
    },
    ...
}
```

Notes:
- `thru_includes` should list all paragraphs swept into the range (from the paragraph inventory, in source order between `target` and `thru_target` inclusive)
- Include the PERFORM variant (`UNTIL`, `TIMES`, `VARYING`) when present — this tells the consumer whether a paragraph executes once or in a loop
- For `PERFORM UNTIL` and `PERFORM VARYING`, include the condition text in a `condition` field (e.g., `"condition": "WS-EOF = 'Y'"`)

#### 3. GO TO targets

For each paragraph, any GO TO statements and their targets.

```python
result.goto_graph
# Returns:
{
    "VALIDATE-DATA": {
        "gotos": [
            {
                "target": "VALIDATE-ERROR",
                "line": 1475,
                "conditional": True,       # True if inside an IF/EVALUATE
                "condition_text": "IF WS-POLICY-STATUS = SPACES",
            },
        ]
    },
    ...
}
```

#### 4. CALL graph

For each paragraph, any CALL statements to external modules.

```python
result.call_graph
# Returns:
{
    "READ-POLICY-DB": {
        "calls": [
            {
                "target": "ELSRQ130",          # Called module name
                "line": 1500,
                "using_fields": ["WS-INPUT-AREA", "WS-OUTPUT-AREA"],  # Fields in the USING clause
                "is_dynamic": False,            # True if CALL identifier (variable), False if CALL literal
            },
        ]
    },
    ...
}
```

Notes:
- For dynamic CALLs (`CALL WS-PROGRAM-NAME`), set `is_dynamic: True` and put the identifier name in `target`
- `using_fields` should list the field names from the USING/BY REFERENCE/BY CONTENT clause

#### 5. Conditional branches per paragraph

For each paragraph, the conditional structures (IF/EVALUATE) with their branches.

```python
result.conditions
# Returns:
{
    "DETERMINE-PRODUCT-TYPE": {
        "conditions": [
            {
                "type": "evaluate",
                "subject": "WS-PRODUCT-CODE",
                "line": 1650,
                "branches": [
                    {"when": "'01'", "line": 1652, "description": "WHEN '01'"},
                    {"when": "'02'", "line": 1665, "description": "WHEN '02'"},
                    {"when": "'03' THRU '09'", "line": 1678, "description": "WHEN '03' THRU '09'"},
                    {"when": "OTHER", "line": 1690, "description": "WHEN OTHER"},
                ],
            },
            {
                "type": "if",
                "condition": "WS-POLICY-STATUS = '10'",
                "line": 1700,
                "has_else": True,
                "nested_conditions": [       # For nested IF inside the THEN/ELSE
                    {
                        "type": "if",
                        "condition": "WS-RENEWAL-FLAG = 'Y'",
                        "line": 1705,
                        "has_else": False,
                        "nested_conditions": [],
                    }
                ],
            },
        ],
    },
    ...
}
```

Notes:
- For EVALUATE, list every WHEN branch including WHEN OTHER
- For EVALUATE TRUE (no explicit subject), set `subject` to `"TRUE"`
- For IF, include whether ELSE exists and recursively include nested IFs
- Only top-level conditions within each paragraph — don't recurse into PERFORMed paragraphs

#### 6. Per-paragraph field references

For each paragraph, the DATA DIVISION fields it reads and writes.

```python
result.field_references
# Returns:
{
    "COMPUTE-PREMIUM": {
        "reads": ["WS-BASE-RATE", "WS-RISK-FACTOR", "WS-POLICY-TYPE", "INPUT-COVERAGE-AMOUNT"],
        "writes": ["WS-PREMIUM-AMOUNT", "WS-TAX-AMOUNT", "WS-TOTAL-PREMIUM"],
        "conditions_tested": ["VALID-POLICY", "HIGH-RISK"],  # 88-level names tested in IF/EVALUATE/WHEN
    },
    ...
}
```

Notes:
- `reads`: fields appearing in the "sending" position of MOVE, in IF/EVALUATE conditions, in COMPUTE right-hand side, in CALL USING (BY REFERENCE for read, or BY CONTENT)
- `writes`: fields appearing in the "receiving" position of MOVE, COMPUTE, INITIALIZE, STRING/UNSTRING INTO, READ INTO, CALL USING BY REFERENCE (when modified)
- `conditions_tested`: 88-level condition names used in IF/EVALUATE conditions (these are particularly valuable for business rule extraction)
- A field can appear in both `reads` and `writes` if it's both read and modified in the same paragraph
- Include fields from inlined copybooks (when `resolve_copies=True`)
- For group MOVEs (e.g., `MOVE WS-INPUT-AREA TO WS-OUTPUT-AREA`), list the group-level field name — don't expand to individual elementary items

### Convenience methods

The result object should provide these convenience methods:

```python
# Text summary suitable for including in an LLM prompt (compact, readable)
result.to_text()

# Dict/JSON representation (for programmatic use)
result.to_dict()

# Filter to specific paragraphs (useful when combining with a traced paragraph list)
result.for_paragraphs(["INIT-PROGRAM", "COMPUTE-PREMIUM"])
# Returns a new result containing only the specified paragraphs (but preserving
# their PERFORM targets in the graph even if the target is not in the list)
```

### The `to_text()` format

This is the most important method for the LLM agent use case. It should produce a compact, readable summary designed to be consumed by an LLM agent. Suggested format:

```
PROCEDURE DIVISION STRUCTURE — MODULE-NAME
============================================

PARAGRAPHS (15 total):
  1. INIT-PROGRAM                    [line 1250-1280, 31 lines]
  2. SET-DEFAULTS                    [line 1282-1310, 29 lines]
  3. MAIN-PROCESS                    [line 1312-1340, 29 lines]
  ...

PERFORM GRAPH:
  MAIN-PROCESS
    → PERFORM INIT-PROGRAM                                 [line 1315]
    → PERFORM PROCESS-POLICY THRU PROCESS-POLICY-EXIT      [line 1320]
        (includes: PROCESS-POLICY, VALIDATE-DATA, COMPUTE-PREMIUM, PROCESS-POLICY-EXIT)
    → PERFORM FINALIZE UNTIL WS-EOF = 'Y'                  [line 1335]

  INIT-PROGRAM
    → PERFORM SET-DEFAULTS                                  [line 1255]
  ...

GO TO TARGETS:
  VALIDATE-DATA
    → GO TO VALIDATE-ERROR  (IF WS-POLICY-STATUS = SPACES) [line 1475]
  ...

CALLS TO EXTERNAL MODULES:
  READ-POLICY-DB
    → CALL 'ELSRQ130' USING WS-INPUT-AREA, WS-OUTPUT-AREA  [line 1500]
  ...

CONDITIONAL BRANCHES:
  DETERMINE-PRODUCT-TYPE
    EVALUATE WS-PRODUCT-CODE                                [line 1650]
      WHEN '01'                                             [line 1652]
      WHEN '02'                                             [line 1665]
      WHEN '03' THRU '09'                                   [line 1678]
      WHEN OTHER                                            [line 1690]

  CHECK-POLICY-STATUS
    IF WS-POLICY-STATUS = '10'                              [line 1700]
      IF WS-RENEWAL-FLAG = 'Y'                              [line 1705]
    ELSE                                                    [line 1720]
  ...

FIELD REFERENCES:
  COMPUTE-PREMIUM
    Reads:  WS-BASE-RATE, WS-RISK-FACTOR, WS-POLICY-TYPE, INPUT-COVERAGE-AMOUNT
    Writes: WS-PREMIUM-AMOUNT, WS-TAX-AMOUNT, WS-TOTAL-PREMIUM
    88-conditions tested: VALID-POLICY, HIGH-RISK
  ...
```

## Design considerations

### Consistency with existing API patterns

Follow the existing library conventions:
- Options as a `@dataclass` (`ProcedureDivisionOptions`) with the same `copybook_paths` and `resolve_copies` fields as existing options classes
- Result object with `to_dict()` and `to_text()` methods (like `ParagraphAnalysisResult`)
- Raise `AnalysisError` for parse failures
- Accept `Path` for `source_path`

### COBOL dialect handling

The COBOL programs this will analyze are **IBM Enterprise COBOL** running on z/OS mainframes. Key dialect features to handle:
- **Fixed-format source** (columns 1-6 sequence, 7 indicator, 8-11 Area A, 12-72 Area B)
- **COPY ... REPLACING** statements (when `resolve_copies=True`)
- **PERFORM ... THRU** ranges (very common in mainframe COBOL)
- **EVALUATE TRUE / EVALUATE FALSE** patterns
- **GO TO ... DEPENDING ON** (indexed GO TO)
- **88-level conditions** used in IF/EVALUATE conditions
- **CALL ... USING BY REFERENCE / BY CONTENT**
- **Nested programs** (less common but possible) — analyze only the outermost program unless a specific program-id is requested
- **Paragraph labels with hyphens** (e.g., `3100-VALIDATE-INPUT`) — these are common and the parser must handle them
- **Section/paragraph distinction** — sections contain paragraphs; both are valid PERFORM targets

### Copybook resolution

PROCEDURE DIVISION code can contain COPY statements (though it's more common in DATA DIVISION). When `resolve_copies=True`, inline the copybook content before analysis so that PERFORMed paragraph targets inside copied code are included in the graph.

### Error tolerance

Production COBOL source files may have minor formatting irregularities. The parser should be **lenient** — if a single paragraph can't be fully parsed (e.g., a complex COMPUTE expression), include it in the inventory with whatever information could be extracted and note the partial parse, rather than failing the entire analysis. The paragraph inventory and PERFORM graph are the highest-value outputs and should succeed even if conditional branch analysis or field reference extraction is partial.

### Performance

The largest modules in the target dataset are ~6000 lines. The function will be called once per module (typically 5-15 modules per analysis run), and the result is cached. Response time under 5 seconds per module is acceptable.

## Priority

If the full implementation is too large for a single pass, prioritize in this order:

1. **Paragraph inventory** — highest impact, eliminates missed paragraphs entirely
2. **PERFORM graph** — second highest impact, enables accurate control flow understanding
3. **Conditional branches** — enables complete business rule extraction
4. **GO TO targets** and **CALL graph** — important for control flow completeness
5. **Field references** — useful but partially covered by existing `variable_index`

The `to_text()` method and the `for_paragraphs()` filter should be implemented alongside whichever views are built, as they are essential for the LLM agent integration.

# Plan
# Plan: Add PROCEDURE DIVISION Analysis API

## Context

The `cobol-ast-parser` library currently analyzes only the DATA DIVISION. The LLM agents need a deterministic PROCEDURE DIVISION skeleton (paragraphs, control flow, branches, field references) to replace error-prone text-based discovery. This plan adds `analyze_procedure_division()` to provide that.

## Architecture Decision

**Approach: Extend parser + new analyzer + new API function.**

- **Parser** (`cobol_parser.py`): Add new regex patterns for PERFORM targets, GO TO targets, and CALL program names as new statement types (`PERFORM_TARGET`, `GOTO_TARGET`, `CALL_PROGRAM`). These are additive — they don't modify existing statement extraction, so existing APIs are unaffected.
- **Analyzer** (new `procedure_division_analyzer.py`): Operates on the `SimplifiedParseTree` + `CobolProgram` to build the six output views. Handles PERFORM THRU resolution, conditional branch parsing (stack-based for nested IF/EVALUATE), and field reference aggregation from existing AST data.
- **API** (`api.py`): New `ProcedureDivisionOptions`, `ProcedureDivisionResult`, and `analyze_procedure_division()` following existing patterns.

## Files to Create/Modify

### 1. New test fixture: `tests/fixtures/procedure_division_program.cob`
- Multi-section COBOL program exercising: PERFORM simple/THRU/UNTIL, GO TO (conditional), CALL static/dynamic with USING, EVALUATE/WHEN/WHEN OTHER, nested IF, 88-level conditions, EXIT paragraph
- ~100 lines, generic English names only

### 2. Parser: `src/parser/cobol_parser.py`

**Fix paragraph/section line_number population:**
- In `_parse_procedure_division()`: compute `header_line_num` from `match.start()` and set on `SimplifiedSection` and `SimplifiedParagraph`
- In `_parse_section()`: compute and set `paragraph.line_number` from paragraph header match position
- Currently these fields exist (`line_number: int = 0`) but are never populated

**Add 3 new regex patterns (class attributes):**
- `PERFORM_TARGET_STMT`: captures target paragraph, optional THRU target, optional TIMES count, optional UNTIL condition, optional VARYING
- `GOTO_TARGET_STMT`: captures GO TO target paragraph
- `CALL_PROGRAM_STMT`: captures called program name (literal or identifier) and USING fields

**Add `_extract_control_flow_statements()` method:**
- Called at end of `_extract_statements()` (after existing `_extract_read_only_statements`)
- Appends `SimplifiedStatement` entries with types `PERFORM_TARGET`, `GOTO_TARGET`, `CALL_PROGRAM`
- Does NOT affect existing statement types — purely additive

### 3. New analyzer: `src/analyzers/procedure_division_analyzer.py`

**Internal dataclasses** (used by the analyzer, not exported):
- `ParagraphInfo(name, type, parent_section, line_start, line_end, line_count)`
- `PerformEntry(target, type, thru_target, thru_includes, condition, times, line)`
- `GotoEntry(target, line, conditional, condition_text)`
- `CallEntry(target, line, using_fields, is_dynamic)`
- `ConditionEntry(type, condition, subject, line, has_else, branches, nested_conditions)`
- `FieldReferenceEntry(reads, writes, conditions_tested)`

**`ProcedureDivisionAnalyzer` class:**
- `__init__(parse_tree, program, source_lines)` — takes SimplifiedParseTree, CobolProgram, and the raw source lines for conditional parsing
- `analyze()` — orchestrates the six extraction passes:
  1. `_build_paragraph_inventory()` — walks parse_tree sections/paragraphs; computes line_end from next paragraph's line_start; stores ordered list
  2. `_build_perform_graph()` — filters PERFORM_TARGET statements per paragraph; resolves THRU ranges from inventory
  3. `_build_goto_graph()` — filters GOTO_TARGET statements; detects conditional context by checking IF/EVALUATE nesting at the GO TO's line position
  4. `_build_call_graph()` — filters CALL_PROGRAM statements; identifies dynamic vs static
  5. `_build_condition_map()` — second pass over raw source per paragraph using stack-based IF/EVALUATE/END-IF/END-EVALUATE/WHEN token scanning to build nested `ConditionEntry` trees
  6. `_build_field_references()` — aggregates from existing `Paragraph.modifications` (writes) and `Paragraph.accesses` (reads); cross-references against `program.all_data_items` to identify 88-level items for `conditions_tested`

### 4. API: `src/cobol_ast/api.py`

**New `ProcedureDivisionOptions` dataclass:**
- `copybook_paths: Optional[List[Path]]`
- `resolve_copies: bool = True`
- `include_source_info: bool = True`

**New `ProcedureDivisionResult` dataclass:**
- `program_name`, `paragraphs`, `perform_graph`, `goto_graph`, `call_graph`, `conditions`, `field_references`
- `execution_time_seconds`, `source_info`, `warnings`
- `to_dict()` — JSON-serializable dict
- `to_text()` — compact LLM-friendly text format (spec's suggested format)
- `for_paragraphs(names)` — returns filtered result preserving PERFORM targets even if target isn't in the filter list

**New `analyze_procedure_division()` function:**
- Same pattern as `analyze_with_tree()`: validate input → read source → detect format → resolve copies → normalize → parse → build AST → run analyzer → apply line mapping → build result
- Uses `_resolve_copies()` helper
- Uses `_convert_to_original_line()` for line number mapping
- Error handling: re-raise ParseError/FileNotFoundError, wrap others in AnalysisError

### 5. Exports: `src/cobol_ast/__init__.py`
- Add `analyze_procedure_division`, `ProcedureDivisionOptions`, `ProcedureDivisionResult`

### 6. Tests: `tests/test_procedure_division_api.py`
- `TestProcedureDivisionOptions` — options defaults and custom values
- `TestAnalyzeProcedureDivision` — end-to-end using new fixture:
  - Paragraph inventory: completeness, line numbers, line counts, section types, ordering
  - PERFORM graph: simple, THRU (with thru_includes resolved), UNTIL with condition
  - GO TO graph: targets, conditional detection
  - CALL graph: static/dynamic, using_fields
  - Conditional branches: EVALUATE with WHEN branches, IF with ELSE, nested IF
  - Field references: reads, writes, conditions_tested (88-levels)
  - Convenience: to_dict serializable, to_text format, for_paragraphs filter
  - Error handling: FileNotFoundError, non-file paths
- `TestExistingFixtures` — verify the new API works on existing fixtures (simple_program.cob, all_modifications.cob)

### 7. Documentation: `docs/api-usage.md`
- Add section for `analyze_procedure_division()` with examples, options reference, result structure reference

## Implementation Order

All in one pass (the feature is self-contained), but within the implementation:

1. Create test fixture first
2. Fix parser paragraph/section line_number population
3. Add parser regex patterns + `_extract_control_flow_statements()`
4. Create analyzer
5. Create API dataclasses and function
6. Add exports
7. Write tests
8. Update docs
9. Run full test suite to verify no regressions

## Verification

```bash
# Run new tests
pytest tests/test_procedure_division_api.py -v

# Run all tests to verify no regressions
pytest

# Type checking
mypy src/cobol_ast/api.py src/analyzers/procedure_division_analyzer.py
```

# Refinement
The new api `analyze_procedure_division()` should work on Cobol source where the copybooks have been already resolved. Can you check if this is the case?

# Response
● Yes, the API already handles copybook resolution correctly. Here's the summary:                                                  
                                                                  
  - ProcedureDivisionOptions.resolve_copies defaults to True (line 1822)                                                           
  - When resolve_copies is True, the function calls _resolve_copies() (line 2221), which is the same shared helper used by the other APIs (analyze_paragraph_variables, get_data_division_tree, analyze_with_tree)                                              
  - The user can also provide copybook_paths to specify where to search for copybooks                                              
  - By the time the source reaches the parser (line 2232), COPY statements have already been expanded

  So analyze_procedure_division() works on COBOL source with copybooks already resolved — it handles the resolution internally before parsing, just like the other APIs do.