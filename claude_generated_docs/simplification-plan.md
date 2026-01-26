# Radical Simplification Plan: Keep Only `paragraph-variables-map`

## Overview

This plan radically simplifies the COBOL AST Parser CLI by keeping only the `paragraph-variables-map` command and removing all unused code, tests, and documentation.

## Current State

The CLI currently has 4 commands:
1. `analyze` - Core analysis, produces full analysis JSON
2. `filter-by-variable` - Takes existing analysis JSON, produces variable-centric view
3. `analyze-and-filter` - Combines analyze + filter-by-variable
4. `paragraph-variables-map` - Produces paragraph-centric view (THE ONLY COMMAND TO KEEP)

## Files to Modify/Delete

### 1. Remove Unused Code in `src/main.py`

**Lines to remove:**
- `handle_analyze()` function (lines 185-292)
- `handle_filter_by_variable()` function (lines 295-394)
- `handle_analyze_and_filter()` function (lines 397-529)
- `create_analyze_parser()` function (lines 633-733)
- `create_filter_parser()` function (lines 736-817)
- `create_analyze_and_filter_parser()` function (lines 820-943)

**Lines to modify:**
- Import statement (line 21): Remove `VariableFilter` import
- `main()` function: Remove backwards compatibility logic, remove 3 subcommand calls
- Update version to 2.0.0 (major breaking change)

### 2. Delete Unused Output Module

**Delete file:**
- `src/output/variable_filter.py`

**Modify file:**
- `src/output/__init__.py`: Remove `VariableFilter` import and export

### 3. Delete Unused Test Files

**Delete files:**
- `tests/test_variable_filter.py`

**Keep files:**
- `tests/test_paragraph_variables_map.py`
- `tests/test_impact_analyzer.py` (still used by paragraph-variables-map)
- `tests/test_parser.py` (still used)
- `tests/test_preprocessor.py` (still used)
- `tests/test_data_analyzer.py` (still used)
- `tests/test_procedure_analyzer.py` (still used)
- `tests/test_redefines.py` (still used)
- `tests/conftest.py` (shared fixtures)

### 4. Remove Old Claude Generated Docs (except this plan) and Create New Strategy Doc

**Delete files in `claude_generated_docs/`:**
- `cobol-analyzer-strategy.md`
- `subordinate-redefines-strategy.md`
- `variable-filter-strategy.md`
- `paragraph-variables-map-plan.md`

**Keep:**
- `simplification-plan.md` (this file)

**Create new file:**
- `claude_generated_docs/strategy.md` - Single document describing the simplified app's strategy

### 5. Update Documentation

**Update `README.md`:**
- Remove references to `analyze`, `filter-by-variable`, `analyze-and-filter` commands
- Update quick example to show only `paragraph-variables-map`
- Update features list

**Update `docs/cli-reference.md`:**
- Remove all sections for removed commands
- Keep only `paragraph-variables-map` command section
- Update synopsis, description, examples

**Update `docs/quickstart.md`:**
- Simplify to show only `paragraph-variables-map` usage
- Remove references to other commands

**Update `docs/json-output-reference.md`:**
- Review and remove any sections specific to removed commands

## Implementation Order

1. **Phase 1: Remove code from `main.py`**
   - Remove 3 handler functions
   - Remove 3 parser creation functions
   - Update `main()` function
   - Update imports

2. **Phase 2: Remove `variable_filter.py`**
   - Delete the file
   - Update `src/output/__init__.py`

3. **Phase 3: Delete test file**
   - Delete `tests/test_variable_filter.py`

4. **Phase 4: Update `claude_generated_docs/`**
   - Delete old files (except this plan)
   - Create new `strategy.md`

5. **Phase 5: Update documentation**
   - Update `README.md`
   - Update `docs/cli-reference.md`
   - Update `docs/quickstart.md`
   - Review `docs/json-output-reference.md`

6. **Phase 6: Verify**
   - Run tests to ensure nothing is broken
   - Test the CLI manually

## New Strategy Document Content

The new `claude_generated_docs/strategy.md` will describe:
- Purpose: Map paragraphs/sections to variables that may change
- Architecture: COBOL source → Parse → AST → Analysis → Paragraph-Variables Map
- Key components:
  - Preprocessor (COPY resolution, format detection, normalization)
  - Parser (regex-based COBOL parser)
  - AST Builder (parse tree to domain AST)
  - Impact Analyzer (orchestrates analysis)
  - Paragraph Variables Mapper (transforms analysis to paragraph-centric view)
- Output format and semantics
- REDEFINES handling
- Ancestor modification tracking

## Verification

After implementation:
```bash
# Run all tests
pytest

# Test CLI manually
python -m src paragraph-variables-map some-cobol-source/COBCALC.cbl -o ./output

# Verify help
python -m src --help
python -m src paragraph-variables-map --help
```

## Summary of Changes

| Action | Count |
|--------|-------|
| Functions removed from main.py | 6 |
| Files deleted | 5 (1 source, 1 test, 4 docs) |
| Files modified | 5 (main.py, output/__init__.py, README.md, cli-reference.md, quickstart.md) |
| Files created | 1 (strategy.md) |
