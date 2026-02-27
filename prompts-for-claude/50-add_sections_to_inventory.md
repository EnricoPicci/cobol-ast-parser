The api analyze_procedure_division must consider not only Cobol PARAGRAPHs but also Cobol SECTIONs.
The reason is that both PARAGRAPHs and SECTIONs can be target of the PERFORM operation.
Fix the api to include this requirement.
If the api signature changes, then highlight the changes.

# Plan
# Plan: Add SECTIONs to `analyze_procedure_division` Inventory

## Context

The `analyze_procedure_division()` API currently only adds **paragraphs** to the inventory and graphs. COBOL SECTIONs are tracked only as `parent_section` metadata on paragraph entries. However, in COBOL, both PARAGRAPHs and SECTIONs can be targets of `PERFORM`, `GO TO`, etc. For example, `PERFORM REPORT-SECTION` executes all paragraphs within that section. Sections must be first-class entries in the inventory so that callers can resolve PERFORM/GO TO targets that reference sections.

## Breaking Renames

These are intentional breaking changes for correctness:

| Old Name | New Name | Reason |
|----------|----------|--------|
| `ParagraphInfo` (dataclass) | `ProcedureEntry` | Represents both sections and paragraphs |
| `ProcedureDivisionResult.paragraphs` | `ProcedureDivisionResult.inventory` | Contains both sections and paragraphs |
| `ProcedureDivisionResult.for_paragraphs()` | `ProcedureDivisionResult.for_entries()` | Filters both sections and paragraphs |
| `analyzer.paragraphs` (on `ProcedureDivisionAnalyzer`) | `analyzer.inventory` | Contains both sections and paragraphs |
| `_build_paragraph_inventory()` | `_build_inventory()` | Builds sections + paragraphs |
| `_get_paragraph_names_ordered()` | `_get_entry_names_ordered()` | Returns section + paragraph names |

## Files to Modify

### 1. `src/analyzers/procedure_division_analyzer.py`

**Rename `ParagraphInfo` → `ProcedureEntry`** (line 18) and add `paragraphs` field:
```python
@dataclass
class ProcedureEntry:
    """An entry (section or paragraph) in the PROCEDURE DIVISION inventory."""
    name: str
    type: str  # "paragraph" or "section"
    parent_section: Optional[str]
    line_start: int
    line_end: int
    line_count: int
    paragraphs: Optional[List[str]] = None  # child paragraph names (sections only)
```

**Rename `self.paragraphs` → `self.inventory`** in `__init__` (line 107):
```python
self.inventory: List[ProcedureEntry] = []
```

**Rename `_build_paragraph_inventory()` → `_build_inventory()`** (line 123) — Add section entries:
- For each `section` in `proc_div.sections`, insert an entry with `type="section"`, `parent_section=None`, `line_start=section.line_number`, and `paragraphs=[list of child paragraph names]`
- Sorted alongside paragraphs by `line_start`, so each section entry sits before its child paragraphs
- Line-end computation works as-is: section entry ends just before its first child paragraph

**Rename `_get_paragraph_names_ordered()` → `_get_entry_names_ordered()`** (line 179)

**`_build_perform_graph()`** (line 196) — Also extract from `section.statements`:
```python
for section in proc_div.sections:
    entries = self._extract_performs_from_statements(section.statements)
    if entries:
        self.perform_graph[section.name] = entries
    for para in section.paragraphs:
        ...
```

**`_build_goto_graph()`** (line 266) — Same pattern for `section.statements`

**`_build_call_graph()`** (line 318) — Same pattern for `section.statements`

**`_build_condition_map()`** (line 358) — Uses `self.paragraphs` → update to `self.inventory`. Already works via line ranges; section entries get their own range (header area).

**`_build_field_references()`** (line 510) — Add section standalone refs:
- Add `_aggregate_section_field_refs(section)` method using `section.standalone_modifications` and `section.standalone_accesses`
- Call it for each section before iterating its paragraphs

### 2. `src/cobol_ast/api.py`

**`ProcedureDivisionResult`** (line 1826):
- Rename field `paragraphs` → `inventory`
- Rename method `for_paragraphs()` → `for_entries()`
- Update all docstrings to reference "inventory" and "sections and paragraphs"

**`to_text()`** — Change header from `"PARAGRAPHS:"` to `"INVENTORY:"`. For section entries, show child paragraph list.

**`_build_procedure_result()`** (line 2044):
- Read from `analyzer.inventory` instead of `analyzer.paragraphs`
- Include `paragraphs` key in dict for section entries
- Assign to `inventory=` instead of `paragraphs=`

### 3. `src/cobol_ast/__init__.py`

- No new exports needed (dataclasses already not exported directly; `ProcedureDivisionResult` is already exported)

### 4. `tests/test_procedure_division_api.py`

- Update all references from `result.paragraphs` → `result.inventory`
- Update `for_paragraphs()` → `for_entries()`
- `test_paragraph_count` → update from 12 to 18 (12 paragraphs + 6 sections)
- `test_paragraph_types` → expect both "paragraph" and "section" types
- Add new tests:
  - `test_section_entries_exist`: Verify section names in inventory
  - `test_section_entry_fields`: Verify type="section", parent_section=None, paragraphs list
  - `test_section_child_paragraphs`: Verify each section's child paragraph list
  - `test_section_ordering`: Sections appear before their child paragraphs
  - `test_for_entries_with_sections`: Filter by section name
  - `test_to_text_shows_sections`: Text output includes section entries

### 5. `docs/api-usage.md`

- Rename `paragraphs` → `inventory` in field table and examples
- Rename `for_paragraphs()` → `for_entries()` in method list and examples
- Document section entries with `paragraphs` child list
- Note the breaking changes

## Verification

```bash
pytest tests/test_procedure_division_api.py -v
pytest
mypy src/analyzers/procedure_division_analyzer.py src/cobol_ast/api.py
```

