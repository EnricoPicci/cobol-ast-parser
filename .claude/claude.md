# COBOL AST Parser - Development Guidelines

## General Python Best Practices

### Code Style

- Follow PEP 8 style guide for Python code
- Use 4 spaces for indentation (no tabs)
- Maximum line length: 100 characters
- Use snake_case for functions and variables, PascalCase for classes
- Use UPPER_CASE for constants

### Type Hints

- Always use type hints for function signatures
- Use `typing` module for complex types: `List`, `Dict`, `Optional`, `Set`, `Tuple`
- Use `from __future__ import annotations` for forward references
- Example:
  ```python
  def resolve_copy(self, source: str, copybook_paths: List[Path]) -> str:
  ```

### Dataclasses

- Prefer `@dataclass` for data containers over plain classes
- Use `field(default_factory=list)` for mutable defaults
- Example:
  ```python
  @dataclass
  class DataItem:
      name: str
      level: int
      children: List['DataItem'] = field(default_factory=list)
  ```

### Documentation

- Use docstrings for all public modules, classes, and functions
- Follow Google-style docstrings format
- Include type information in docstrings when not using type hints
- Example:
  ```python
  def get_affected_records(self, variable_name: str) -> Set[str]:
      """Get all Level 01 records affected when a variable is modified.

      Args:
          variable_name: The name of the variable being modified.

      Returns:
          Set of Level 01 record names that share memory with this variable.
      """
  ```

### Error Handling

- Create specific exception classes for different error types
- Use descriptive error messages
- Prefer explicit exception handling over bare `except:`
- Example:
  ```python
  class CircularCopyError(Exception):
      """Raised when circular COPY dependencies are detected."""
      pass
  ```

### Imports

- Group imports: standard library, third-party, local
- Use absolute imports
- Avoid `from module import *`
- Sort imports alphabetically within groups

### Testing

- Use pytest for all tests
- Name test files with `test_` prefix
- Name test functions with `test_` prefix
- Use fixtures for common setup
- Aim for high coverage on core parsing and analysis logic

---

## Project-Specific Guidelines

### COBOL Terminology

Use consistent naming that matches COBOL terminology:

| Term | Usage |
|------|-------|
| `record_description` | Level 01 data item and its subordinates |
| `data_item` | Any item in DATA DIVISION (any level) |
| `section` | PROCEDURE DIVISION section |
| `paragraph` | PROCEDURE DIVISION paragraph |
| `copybook` | External source file included via COPY |
| `redefines` | Memory overlay relationship |

### Module Organization

```
src/
├── preprocessor/     # COPY resolution, format handling
├── parser/           # ANTLR integration
│   └── generated/    # ANTLR-generated files (do not edit)
├── ast/              # Domain-specific AST nodes
├── analyzers/        # Analysis logic
└── output/           # Output formatting
```

### AST Node Design

- All AST nodes should be dataclasses
- Use `Optional` for nullable fields
- Parent references should use `Optional` to handle root nodes
- Include `line_number` for error reporting and debugging

### ANTLR Integration

- Keep generated ANTLR files in `src/parser/generated/`
- Never manually edit generated files
- Create wrapper classes for parser integration
- Use Visitor pattern (not Listener) for AST building

### REDEFINES Handling

- Model REDEFINES as an undirected graph using NetworkX
- Use connected components to find all related records
- Handle both Level 01 REDEFINES and subordinate REDEFINES
- Document the memory overlap semantics clearly

### Variable Modification Tracking

Track all COBOL statements that modify variables:

```python
MODIFYING_STATEMENTS = [
    'MOVE', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
    'STRING', 'UNSTRING', 'INSPECT', 'ACCEPT', 'READ', 'RETURN',
    'INITIALIZE', 'SET', 'SEARCH'
]
```

For each statement type, identify the TARGET variables (not source):
- `MOVE A TO B` - B is modified
- `ADD A TO B` - B is modified
- `COMPUTE C = A + B` - C is modified

### Configuration

- Use YAML for configuration files
- Support multiple copybook search paths
- Allow configurable output formats

### Logging

- Use Python's `logging` module
- Include context in log messages (file name, line number)
- Use appropriate log levels:
  - DEBUG: Parsing details, AST construction
  - INFO: File processing, analysis progress
  - WARNING: Unresolved references, deprecated syntax
  - ERROR: Parse failures, missing copybooks

### Performance Considerations

- Cache resolved copybooks to avoid re-parsing
- Use lazy loading for large programs
- Consider memory usage with deep AST structures

### Output Format

- Primary output: JSON
- Include metadata (program name, analysis date)
- Structure output by section/paragraph name
- List affected records alphabetically for consistency
