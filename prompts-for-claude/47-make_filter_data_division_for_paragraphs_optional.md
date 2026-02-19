The api analyze_for_paragraphs returns a CombinedResult and the data division filtered data.
Make the calculation of the data division filtered data optional via an input parameter.
By default data division filtered data is calculated.
If the new parameter is set to false, data division filtered data is not calculated.
If it is not calculated, the api simply returns what the current analyze_with_tree api returns.

Check if for you this requirement is coherent with the rest of the apis offered by this package.
Ask for clarifications if needed.

# Response
# Make filtered data division optional in `analyze_for_paragraphs`

## Context

`analyze_for_paragraphs` currently always computes a filtered DATA DIVISION tree scoped to the requested paragraphs. Some callers only need the `CombinedResult`-equivalent data (analysis + full tree) without the filtering overhead. Adding an opt-out parameter lets callers skip the filtering step, at which point `analyze_for_paragraphs` effectively returns what `analyze_with_tree` returns, wrapped in a `ParagraphAnalysisResult` with empty filter fields.

## Changes

### 1. Add parameter to `ParagraphAnalysisOptions` (`src/cobol_ast/api.py:1420`)

Add a new field:

```python
include_filtered_data: bool = True
```

With docstring update explaining: when False, the filtering step is skipped and filter-related fields are returned empty.

### 2. Conditionally skip filtering in `analyze_for_paragraphs` (`src/cobol_ast/api.py:1609`)

After the `analyze_with_tree` call (line 1685), check `options.include_filtered_data`. If False, skip steps 2-6 (lines 1690-1760) and return early with:

- `filtered_sections=[]`
- `filtered_records=[]`
- `filter_summary={"total_records_before": len(tree.all_records), "total_records_after": 0, "records_removed": len(tree.all_records), "reduction_percentage": 100.0}` — or simpler: all zeros. Let me go with empty summary that reflects "not computed":
  - `filter_summary={}` — actually, keeping the structure consistent is better for callers. Use zeros: `{"total_records_before": 0, "total_records_after": 0, "records_removed": 0, "reduction_percentage": 0.0}`
- `paragraph_names_used=[]`

### 3. Update `ParagraphAnalysisResult` docstring (`src/cobol_ast/api.py:1443`)

Note that filter-related fields may be empty when `include_filtered_data=False`.

### 4. Update docs (`docs/api-usage.md`)

- Add `include_filtered_data` to the `ParagraphAnalysisOptions` field list
- Add a usage example showing the flag set to False
- Update the "When to Use Which API" comparison table if needed

### 5. Add tests (`tests/test_filter_api.py`)

Add tests in `TestAnalyzeForParagraphs`:

- `test_include_filtered_data_false_returns_empty_filter_fields`: Pass `include_filtered_data=False`, verify `filtered_sections == []`, `filtered_records == []`, `filter_summary` has zeros, `paragraph_names_used == []`
- `test_include_filtered_data_false_still_has_full_tree`: Verify `data_division_tree` and `analysis_result` are fully populated
- `test_include_filtered_data_true_is_default`: Call without the flag, verify filtering is performed (existing tests already cover this implicitly, but one explicit check is good)
- `test_include_filtered_data_false_to_text_returns_empty`: Verify `to_text()` returns empty string
- `test_include_filtered_data_false_to_dict_structure`: Verify `to_dict()` returns correct structure with empty filter fields

### 6. Save prompt file

Save the prompt to `prompts-for-claude/47-make_filter_data_division_for_paragraphs_optional.md`.

## Files to modify

| File | Change |
|------|--------|
| `src/cobol_ast/api.py` | Add field, skip logic, docstring |
| `tests/test_filter_api.py` | Add ~5 new test methods |
| `docs/api-usage.md` | Document new option |
| `prompts-for-claude/47-make_filter_data_division_for_paragraphs_optional.md` | Save prompt |

## Verification

```bash
pytest tests/test_filter_api.py -v        # All existing + new tests pass
pytest tests/test_api.py -v               # No regressions
mypy src/cobol_ast/api.py                 # No new type errors
```

