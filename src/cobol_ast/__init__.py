"""AST module containing domain-specific AST node definitions and public API."""

__version__ = "0.8.0"

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
    resolve_copybooks,
    CopybookResolutionOptions,
    CopybookResolutionResult,
    analyze_for_paragraphs,
    ParagraphAnalysisOptions,
    ParagraphAnalysisResult,
    analyze_procedure_division,
    ProcedureDivisionOptions,
    ProcedureDivisionResult,
)
from preprocessor import LineMapping

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
    # Copybook resolution API
    "resolve_copybooks",
    "CopybookResolutionOptions",
    "CopybookResolutionResult",
    "LineMapping",
    # Paragraph analysis API
    "analyze_for_paragraphs",
    "ParagraphAnalysisOptions",
    "ParagraphAnalysisResult",
    # Procedure division analysis API
    "analyze_procedure_division",
    "ProcedureDivisionOptions",
    "ProcedureDivisionResult",
]
