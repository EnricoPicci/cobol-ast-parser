Add an api that clients can call if they want to resolve all copibooks in a Cobol module.
The input of the api is the following:
- the path to a cobol module
- the path to a folder containing all the copybooks (defaults to the same path of the cobol module)

The output is a structure that contains:
- the text representing the Cobol code once all copybooks have been resolved
- a mapping that allows to map each line of code with the file it is contained in

Resolution of the copybook must be recursive so that copybooks contained in copybooks are resolved.

Look into the current codebase because I think a very similar logic is implemented to support the current  APIs.
If you find such logic, restructure the code so that there is only one implementation of the copybook resolution logic.
Do not change in any way the current APIs, just add a new one.

Ask for any clarification needed and then plan the implementation.

# Response
# Plan: Add `resolve_copybooks` API

## Summary

Add a new public API `resolve_copybooks()` that resolves all COPY statements in a COBOL module and returns the resolved source text + line mapping. Refactor existing APIs to share the copybook resolution orchestration logic.

## Changes

### 1. Extract shared copybook resolution helper (`src/cobol_ast/api.py`)

Create an internal `_resolve_copies()` helper and a `_CopyResolutionOutput` dataclass that encapsulates the duplicated pattern found in all 3 existing APIs (lines 306-318, 894-908, 1093-1110):

```python
@dataclass
class _CopyResolutionOutput:
    resolved_source: str
    line_mapping: Dict[int, LineMapping]
    original_line_count: int
    warnings: List[str]

def _resolve_copies(
    source: str,
    source_path: Path,
    copybook_paths: Optional[List[Path]] = None,
) -> _CopyResolutionOutput:
    """Single implementation of copybook resolution orchestration."""
    from preprocessor import CopyResolver
    copy_paths = list(copybook_paths or [])
    copy_paths.insert(0, source_path.parent)
    resolver = CopyResolver(copy_paths)
    resolved = resolver.resolve(source, source_path.name)
    return _CopyResolutionOutput(
        resolved_source=resolved,
        line_mapping=resolver.line_mapping,
        original_line_count=resolver.original_line_count,
        warnings=resolver.warnings,
    )
```

### 2. Refactor existing APIs to use `_resolve_copies()`

Replace the duplicated 6-8 line blocks in:
- `analyze_paragraph_variables()` (lines ~306-318)
- `get_data_division_tree()` (lines ~894-908)
- `analyze_with_tree()` (lines ~1093-1110)

Each will call `_resolve_copies()` and then extract what it needs from the result.

### 3. Add new public API types (`src/cobol_ast/api.py`)

```python
@dataclass
class CopybookResolutionOptions:
    """Options for copybook resolution."""
    copybook_paths: Optional[List[Path]] = None

@dataclass
class CopybookResolutionResult:
    """Result of copybook resolution."""
    resolved_source: str                    # COBOL source with COPY statements expanded
    line_mapping: Dict[int, LineMapping]     # Maps resolved line -> original location
    execution_time_seconds: float
    warnings: List[str] = field(default_factory=list)
```

### 4. Add new `resolve_copybooks()` function (`src/cobol_ast/api.py`)

```python
def resolve_copybooks(
    source_path: Path,
    options: Optional[CopybookResolutionOptions] = None,
) -> CopybookResolutionResult:
```

Pipeline: read source file -> call `_resolve_copies()` -> return result. No format detection, normalization, or parsing needed.

### 5. Update exports (`src/cobol_ast/__init__.py`)

Add to imports and `__all__`:
- `resolve_copybooks`
- `CopybookResolutionOptions`
- `CopybookResolutionResult`
- `LineMapping` (from preprocessor, re-exported for API consumers)

### 6. Add tests (`tests/test_api.py`)

Test cases using existing fixtures (`tests/fixtures/copybook_main.cob` + `tests/fixtures/copybooks/`):
- Basic resolution: verify COPY statements are replaced
- Line mapping: verify lines map back to correct source files
- Default copybook path: source directory is used when no paths specified
- Missing copybook: warnings returned, no crash
- Recursive resolution: nested COPYs resolved

### 7. Update documentation

- `docs/api-usage.md`: Add section for `resolve_copybooks()` with examples
- `docs/json-output-reference.md`: Update if needed (likely not, since this API returns Python objects not JSON)

## Files to modify

| File | Change |
|------|--------|
| `src/cobol_ast/api.py` | Add helper, new API, refactor existing APIs |
| `src/cobol_ast/__init__.py` | Export new symbols |
| `tests/test_api.py` | Add tests for `resolve_copybooks` |
| `docs/api-usage.md` | Document new API |

## Verification

1. `pytest tests/test_api.py` - all existing tests still pass (no regression)
2. New tests for `resolve_copybooks` pass
3. `pytest` - full test suite passes
4. `mypy src/` - type checking passes
