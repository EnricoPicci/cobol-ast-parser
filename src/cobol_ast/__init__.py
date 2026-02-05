"""AST module containing domain-specific AST node definitions and public API."""

__version__ = "0.2.5"

from .nodes import (
    DataItem,
    RecordDescription,
    VariableModification,
    ModificationType,
    Paragraph,
    Section,
    CobolProgram,
)
from .builder import ASTBuilder
from .api import (
    analyze_paragraph_variables,
    AnalysisOptions,
    AnalysisResult,
    AnalysisError,
    get_data_division_tree,
    TreeOptions,
    DataDivisionTree,
    DataItemNode,
    DataDivisionSection,
    _build_copybook_line_map,
)

__all__ = [
    "__version__",
    # AST nodes
    "DataItem",
    "RecordDescription",
    "VariableModification",
    "ModificationType",
    "Paragraph",
    "Section",
    "CobolProgram",
    "ASTBuilder",
    # Public API
    "analyze_paragraph_variables",
    "AnalysisOptions",
    "AnalysisResult",
    "AnalysisError",
    "get_data_division_tree",
    "TreeOptions",
    "DataDivisionTree",
    "DataItemNode",
    "DataDivisionSection",
    "_build_copybook_line_map",
]
