"""COBOL AST Parser - A source code analyzer for COBOL programs.

Public API:
    analyze_paragraph_variables: Main entry point for programmatic analysis
    AnalysisOptions: Configuration options for analysis
    AnalysisResult: Result container with both JSON outputs
    AnalysisError: Exception raised when analysis fails
    get_data_division_tree: Get hierarchical tree view of DATA DIVISION
    TreeOptions: Configuration options for tree generation
    DataDivisionTree: Result container for tree structure
    DataItemNode: Node representing a data item in the tree
    DataDivisionSection: DATA DIVISION section container

Example:
    >>> from cobol_ast import analyze_paragraph_variables, AnalysisOptions
    >>> from pathlib import Path
    >>>
    >>> result = analyze_paragraph_variables(Path("myprogram.cob"))
    >>> print(result.analysis)             # Full analysis JSON
    >>> print(result.paragraph_variables)  # Paragraph-variables map JSON
"""

from .cobol_ast.api import (
    analyze_paragraph_variables,
    AnalysisOptions,
    AnalysisResult,
    AnalysisError,
    get_data_division_tree,
    TreeOptions,
    DataDivisionTree,
    DataItemNode,
    DataDivisionSection,
    analyze_with_tree,
    CombinedOptions,
    CombinedResult,
)

__version__ = "0.2.0"

__all__ = [
    "analyze_paragraph_variables",
    "AnalysisOptions",
    "AnalysisResult",
    "AnalysisError",
    "get_data_division_tree",
    "TreeOptions",
    "DataDivisionTree",
    "DataItemNode",
    "DataDivisionSection",
    "analyze_with_tree",
    "CombinedOptions",
    "CombinedResult",
]
