# Implementation Plan: Combined API for DataDivisionTree and AnalysisResult

## Objective

Create a new `analyze_with_tree()` API that produces both `DataDivisionTree` and `AnalysisResult` in a single pass, avoiding duplicate parsing and preprocessing.

## Problem Statement

Currently, calling both `get_data_division_tree()` and `analyze_paragraph_variables()` separately results in:
- 2x file reads
- 2x format detection
- 2x COPY resolution
- 2x ANTLR parsing
- 2x AST construction
- 2x DataStructureAnalyzer runs

A combined API will perform each of these operations only once (~40% efficiency gain).

---

## Implementation Steps

### Step 1: Add New Dataclasses to `api.py`

**File:** `/workspaces/cobol-ast-parser/src/cobol_ast/api.py`

Add after `DataDivisionTree` class (around line 545):

```python
@dataclass
class CombinedOptions:
    """Options for combined analysis and tree generation."""
    copybook_paths: Optional[List[Path]] = None
    resolve_copies: bool = True
    include_redefines: bool = True
    include_ancestor_mods: bool = True
    include_source_info: bool = True
    include_filler: bool = True
    include_88_levels: bool = True

    def to_analysis_options(self) -> AnalysisOptions:
        return AnalysisOptions(
            copybook_paths=self.copybook_paths,
            resolve_copies=self.resolve_copies,
            include_redefines=self.include_redefines,
            include_ancestor_mods=self.include_ancestor_mods,
            include_source_info=self.include_source_info,
        )

    def to_tree_options(self) -> TreeOptions:
        return TreeOptions(
            copybook_paths=self.copybook_paths,
            resolve_copies=self.resolve_copies,
            include_filler=self.include_filler,
            include_88_levels=self.include_88_levels,
            include_source_info=self.include_source_info,
        )


@dataclass
class CombinedResult:
    """Result of combined analysis and tree generation."""
    program_name: str
    data_division_tree: DataDivisionTree
    analysis_result: AnalysisResult
    execution_time_seconds: float
```

### Step 2: Add `analyze_with_tree()` Function

**File:** `/workspaces/cobol-ast-parser/src/cobol_ast/api.py`

Add at end of file (after `get_data_division_tree()`):

The function will:
1. Perform shared preprocessing (read, detect format, resolve COPY, parse, build AST) - ONCE
2. Run `ImpactAnalyzer` (which contains `DataStructureAnalyzer` internally)
3. Generate `AnalysisResult` (with `variable_index`)
4. Reuse `analyzer.data_analyzer._memory_regions` to build `DataDivisionTree`
5. Return `CombinedResult` with both outputs

Key code structure:
```python
def analyze_with_tree(
    source_path: Path,
    options: Optional[CombinedOptions] = None,
) -> CombinedResult:
    # 1. Shared preprocessing (same as both existing APIs)
    # 2. analyzer = ImpactAnalyzer(program); analyzer.analyze()
    # 3. Generate AnalysisResult outputs
    # 4. Reuse analyzer.data_analyzer._memory_regions for tree
    # 5. Transform records to DataItemNode tree
    # 6. Return CombinedResult
```

### Step 3: Update Exports

**File:** `/workspaces/cobol-ast-parser/src/__init__.py`

Add to imports and `__all__`:
```python
from .cobol_ast.api import (
    # ... existing ...
    analyze_with_tree,
    CombinedOptions,
    CombinedResult,
)

__all__ = [
    # ... existing ...
    "analyze_with_tree",
    "CombinedOptions",
    "CombinedResult",
]
```

### Step 4: Create Validation Script

**File:** `/workspaces/cobol-ast-parser/sample-input-data/run_combined_analysis.py`

Script that:
1. Runs `analyze_paragraph_variables()` separately
2. Runs `get_data_division_tree()` separately
3. Runs `analyze_with_tree()` (combined)
4. Compares outputs (ignoring `execution_time_seconds`)
5. Reports pass/fail and performance improvement

---

## Files to Modify

| File | Changes |
|------|---------|
| `src/cobol_ast/api.py` | Add `CombinedOptions`, `CombinedResult`, `analyze_with_tree()` |
| `src/__init__.py` | Export new types |
| `sample-input-data/run_combined_analysis.py` | New validation script |

---

## Validation Procedure

After implementation, run:

```bash
# 1. Generate baseline outputs
python3 sample-input-data/run_data_division_tree.py
python3 sample-input-data/run_analyze_paragraph_variables.py

# 2. Run combined API validation
python3 sample-input-data/run_combined_analysis.py
```

The validation script will:
- Compare `DataDivisionTree` from combined vs separate (must be equal)
- Compare `variable_index` from combined vs separate (must be equal)
- Report timing improvement

---

## Key Implementation Details

1. **Reuse of ImpactAnalyzer internals:**
   - `ImpactAnalyzer` already creates `DataStructureAnalyzer` at line 64 of `impact_analyzer.py`
   - Access via `analyzer.data_analyzer._memory_regions`

2. **Line mapping handling:**
   - Build `copybook_line_map` from original source before COPY resolution
   - Apply line mapping to both outputs consistently

3. **Section ordering in tree:**
   - Maintain standard order: FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE

4. **Comparison function:**
   - Must ignore `execution_time_seconds` field
   - Recursively compare nested structures
