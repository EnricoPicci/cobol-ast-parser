# Strategy and Plan: Expose CLI Logic as a Programmatic API

## Confirmation

**Yes, `handle_paragraph_variables_map` is the entry point** for the only CLI command (`paragraph-variables-map`). It is defined at `src/main.py:201`.

However, this function is tightly coupled to the CLI (it takes an `argparse` namespace object and returns an exit code), making it unsuitable for direct API use.

## Current Architecture Analysis

### Entry Point Flow

```
main() → argparse → handle_paragraph_variables_map(args) → exit code
                              │
                              ├─ analyze_cobol_file()      ← Core analysis (reusable)
                              ├─ ParagraphVariablesMapper  ← Mapping logic (reusable)
                              └─ JSONWriter                ← Output formatting (reusable)
```

### CLI Outputs (Two JSON Files)

The CLI command produces **two distinct JSON outputs**:

| Output | Filename Pattern | Source | Description |
|--------|------------------|--------|-------------|
| **Analysis** | `{program_name}-analysis.json` | `analyze_cobol_file()` | Full analysis with all data items, modifications, records, REDEFINES info |
| **Paragraph Variables** | `{program_name}-paragraph-variables.json` | `ParagraphVariablesMapper.map()` | Simplified paragraph-centric view of which variables each section/paragraph modifies |

The API must return **both** outputs to match CLI functionality.

### Problems with `handle_paragraph_variables_map` as API

| Issue | Description |
|-------|-------------|
| **Input coupling** | Takes `argparse.Namespace` object, not typed parameters |
| **Output coupling** | Returns exit code (int), not analysis results |
| **Side effects** | Writes to files, prints to stdout |
| **Error handling** | Catches exceptions and returns 1, losing error details |
| **Logging setup** | Expects logging already configured externally |

### Existing Reusable Function

`analyze_cobol_file()` at line 81 is **already a good API candidate** - it:
- Takes typed parameters (`Path`, `List[Path]`, `bool`, etc.)
- Returns a dictionary with analysis results
- Has minimal side effects (optional file writing)

However, it only performs the **analysis phase**, not the **paragraph-variables mapping phase**.

---

## Strategy: Create a Clean Public API Module

### Approach

Create a new `src/api.py` module that:
1. Exposes the full pipeline as a single function
2. Uses typed dataclasses for input/output
3. Has no CLI dependencies
4. Provides clear error handling with exceptions
5. Is the recommended entry point for programmatic use

### Design Principles

1. **No argparse dependency** - Use typed parameters
2. **Return data, not exit codes** - Return structured results or raise exceptions
3. **Optional side effects** - File writing is opt-in via parameters
4. **Composable** - Allow users to call lower-level functions if needed
5. **Documented** - Clear docstrings with usage examples

---

## Implementation Plan

### Phase 1: Create the API Module

**File:** `src/api.py`

```python
"""Public API for COBOL paragraph-variables analysis.

This module provides the programmatic interface for analyzing COBOL programs.
Use these functions instead of calling CLI internals directly.

Example:
    from api import analyze_paragraph_variables

    result = analyze_paragraph_variables(
        source_path=Path("program.cob"),
        copybook_paths=[Path("./copybooks")],
    )

    # Access both JSON outputs (same as CLI produces)
    print(result.analysis)             # Full analysis JSON
    print(result.paragraph_variables)  # Paragraph-variables map JSON
"""

from dataclasses import dataclass
from pathlib import Path
from typing import Optional, List

@dataclass
class AnalysisOptions:
    """Options for COBOL analysis."""
    copybook_paths: Optional[List[Path]] = None
    resolve_copies: bool = True
    include_redefines: bool = True
    include_ancestor_mods: bool = True
    include_source_info: bool = False

@dataclass
class AnalysisResult:
    """Result of paragraph-variables analysis.

    Contains both JSON outputs that the CLI command generates:
    - analysis: Full analysis output ({program_name}-analysis.json)
    - paragraph_variables: Mapped view ({program_name}-paragraph-variables.json)
    """
    program_name: str

    # The two main outputs (matching CLI JSON files)
    analysis: dict             # Full analysis ({program_name}-analysis.json)
    paragraph_variables: dict  # Mapped view ({program_name}-paragraph-variables.json)

    # Metadata
    execution_time_seconds: float
    source_info: Optional[dict] = None

def analyze_paragraph_variables(
    source_path: Path,
    options: Optional[AnalysisOptions] = None,
) -> AnalysisResult:
    """Analyze a COBOL source file and return paragraph-variables mapping.

    This is the main entry point for programmatic use. Returns both JSON
    outputs that the CLI command would generate.

    Args:
        source_path: Path to the COBOL source file
        options: Analysis options (uses defaults if not provided)

    Returns:
        AnalysisResult containing both:
        - analysis: Full analysis dict (same as {program_name}-analysis.json)
        - paragraph_variables: Mapped dict (same as {program_name}-paragraph-variables.json)

    Raises:
        FileNotFoundError: If source file doesn't exist
        ParseError: If COBOL source cannot be parsed
        AnalysisError: If analysis fails
    """
    ...
```

### Phase 2: Refactor `handle_paragraph_variables_map`

After creating the API, refactor the CLI handler to use it:

```python
def handle_paragraph_variables_map(args) -> int:
    """CLI wrapper that delegates to the public API."""
    try:
        options = AnalysisOptions(
            copybook_paths=args.copybook_paths,
            resolve_copies=not args.no_copy_resolution,
            include_redefines=not args.no_redefines,
            include_ancestor_mods=not args.no_ancestor_mods,
            include_source_info=args.include_source_info,
        )

        result = analyze_paragraph_variables(args.source, options)

        # Handle output (file writing, stdout, etc.)
        ...
        return 0

    except FileNotFoundError as e:
        logger.error(str(e))
        return 1
    except ParseError as e:
        logger.error(f"Parse error: {e}")
        return 1
```

### Phase 3: Update Package Exports

**File:** `src/__init__.py`

```python
"""COBOL AST Parser - Public API."""

from .api import (
    analyze_paragraph_variables,
    AnalysisOptions,
    AnalysisResult,
)

__all__ = [
    "analyze_paragraph_variables",
    "AnalysisOptions",
    "AnalysisResult",
]
```

### Phase 4: Documentation

Add usage examples to README or create `docs/api-usage.md`:

```python
# Quick start
from src import analyze_paragraph_variables
from pathlib import Path

result = analyze_paragraph_variables(Path("myprogram.cob"))

# Access both JSON outputs (same as CLI generates)
print(result.program_name)
print(result.analysis)             # Same as {program_name}-analysis.json
print(result.paragraph_variables)  # Same as {program_name}-paragraph-variables.json

# With options
from src import AnalysisOptions

options = AnalysisOptions(
    copybook_paths=[Path("./copybooks"), Path("./includes")],
    include_redefines=False,
    include_source_info=True,
)
result = analyze_paragraph_variables(Path("myprogram.cob"), options)

# Optionally write to files yourself
import json
Path("analysis.json").write_text(json.dumps(result.analysis, indent=2))
Path("paragraph-variables.json").write_text(json.dumps(result.paragraph_variables, indent=2))
```

---

## File Changes Summary

| File | Action | Description |
|------|--------|-------------|
| `src/api.py` | **Create** | New public API module |
| `src/__init__.py` | **Modify** | Export public API symbols |
| `src/main.py` | **Modify** | Refactor CLI to use API |
| `tests/test_api.py` | **Create** | Unit tests for API |

---

## Alternative Approaches Considered

### Alternative A: Expose `analyze_cobol_file` directly

**Pros:** Already exists, minimal changes
**Cons:** Only returns 1 of 2 JSON outputs (the analysis); users would need to manually call `ParagraphVariablesMapper` to get the second output (paragraph-variables map)

### Alternative B: Add parameters to `handle_paragraph_variables_map`

**Pros:** No new files
**Cons:** Function would become complex with both CLI and API concerns; argparse dependency remains

### Alternative C: Create a class-based API

```python
class CobolAnalyzer:
    def __init__(self, options: AnalysisOptions): ...
    def analyze(self, source_path: Path) -> AnalysisResult: ...
```

**Pros:** More extensible, easier to add state
**Cons:** Over-engineering for current needs; a simple function is sufficient

**Recommendation:** Proceed with the function-based API (main strategy) as it's the simplest solution that meets requirements.

---

## Success Criteria

1. Users can `from src import analyze_paragraph_variables` and call it directly
2. No argparse or CLI dependencies in the API
3. Clear type hints and documentation
4. Existing CLI behavior unchanged
5. Unit tests pass for API usage

---

## Next Steps

1. Review and approve this plan
2. Create `src/api.py` with the public API
3. Add tests in `tests/test_api.py`
4. Refactor `handle_paragraph_variables_map` to use the API
5. Update `src/__init__.py` exports
