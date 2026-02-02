# Versioning Policy

This document describes the versioning policy for `cobol-ast-parser` and instructions for clients importing the package.

## Semantic Versioning

This project follows [Semantic Versioning 2.0.0](https://semver.org/):

```
MAJOR.MINOR.PATCH
```

| Component | When to Bump | Example |
|-----------|--------------|---------|
| **MAJOR** | Breaking changes to the public API | Removed `AnalysisResult.analysis` field, changed function signatures |
| **MINOR** | New features, backward-compatible additions | Added `analyze_with_tree()` function, new options in `AnalysisOptions` |
| **PATCH** | Bug fixes, documentation, internal changes | Fixed parsing bug, improved error messages |

### What Constitutes a Breaking Change

- Removing or renaming public API functions/classes
- Changing function signatures (required parameters, return types)
- Changing JSON output structure in incompatible ways
- Removing fields from result objects
- Changing the behavior of existing functions in ways that break expectations

### What Does NOT Constitute a Breaking Change

- Adding new optional parameters with defaults
- Adding new fields to result objects
- Adding new functions/classes
- Bug fixes (even if someone depended on the buggy behavior)
- Performance improvements
- Internal refactoring

## Version Location

The canonical version is defined in `src/__init__.py`:

```python
__version__ = "0.1.0"
```

This is read dynamically by `setup.py` ensuring a single source of truth.

## Creating a Release

Use the release script:

```bash
# Preview what would happen
python scripts/release.py patch --dry-run

# Create a patch release (0.1.0 -> 0.1.1)
python scripts/release.py patch

# Create a minor release (0.1.0 -> 0.2.0)
python scripts/release.py minor

# Create a major release (0.1.0 -> 1.0.0)
python scripts/release.py major

# Create and push in one command
python scripts/release.py patch --push

# Check current version
python scripts/release.py --check
```

The script will:
1. Bump the version in `src/__init__.py`
2. Update `CHANGELOG.md`
3. Create a git commit
4. Create an annotated git tag (e.g., `v0.2.0`)
5. Optionally push to origin

---

# Client Import Instructions

## Installation Methods

### Method 1: Install from Git (Recommended for Development)

Install the latest version from main branch:

```bash
pip install git+https://github.com/EnricoPicci/cobol-ast-parser.git
```

Install a specific version using a git tag:

```bash
# Install version 0.2.0
pip install git+https://github.com/EnricoPicci/cobol-ast-parser.git@v0.2.0
```

Install from a specific branch:

```bash
pip install git+https://github.com/EnricoPicci/cobol-ast-parser.git@feature-branch
```

### Method 2: In requirements.txt

```txt
# Latest from main
cobol-ast-parser @ git+https://github.com/EnricoPicci/cobol-ast-parser.git

# Pinned to specific version (recommended for production)
cobol-ast-parser @ git+https://github.com/EnricoPicci/cobol-ast-parser.git@v0.2.0

# Pinned to specific commit (most reproducible)
cobol-ast-parser @ git+https://github.com/EnricoPicci/cobol-ast-parser.git@30a0597
```

### Method 3: In pyproject.toml

```toml
[project]
dependencies = [
    "cobol-ast-parser @ git+https://github.com/EnricoPicci/cobol-ast-parser.git@v0.2.0",
]
```

### Method 4: Local Development Install

Clone and install in editable mode:

```bash
git clone https://github.com/EnricoPicci/cobol-ast-parser.git
cd cobol-ast-parser
pip install -e .
```

## Usage in Code

```python
from cobol_ast import (
    analyze_paragraph_variables,
    AnalysisOptions,
    AnalysisResult,
    __version__,
)
from pathlib import Path

# Check version
print(f"Using cobol-ast-parser version {__version__}")

# Basic analysis
result = analyze_paragraph_variables(Path("program.cob"))
print(result.paragraph_variables)

# With options
options = AnalysisOptions(copybook_paths=[Path("./copybooks")])
result = analyze_paragraph_variables(Path("program.cob"), options)
```

## Version Compatibility

When depending on this package, consider these strategies:

### For Applications (Pin Exact Versions)

```txt
# requirements.txt
cobol-ast-parser @ git+https://github.com/EnricoPicci/cobol-ast-parser.git@v0.2.0
```

### For Libraries (Allow Compatible Versions)

If building a library that depends on cobol-ast-parser, document the minimum version:

```python
# In your library's setup.py or pyproject.toml
# Note: pip doesn't support version ranges with git URLs directly
# Document the minimum version in your README instead
```

## Checking Installed Version

```python
import cobol_ast
print(cobol_ast.__version__)  # "0.2.0"
```

Or via command line:

```bash
python -c "import cobol_ast; print(cobol_ast.__version__)"
```

## Upgrading

```bash
# Reinstall to get latest
pip install --force-reinstall git+https://github.com/EnricoPicci/cobol-ast-parser.git

# Or upgrade to specific version
pip install --force-reinstall git+https://github.com/EnricoPicci/cobol-ast-parser.git@v0.3.0
```
