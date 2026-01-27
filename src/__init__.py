"""COBOL AST Parser - A source code analyzer for COBOL programs.

Public API:
    analyze_paragraph_variables: Main entry point for programmatic analysis
    AnalysisOptions: Configuration options for analysis
    AnalysisResult: Result container with both JSON outputs
    AnalysisError: Exception raised when analysis fails

Example:
    >>> from src import analyze_paragraph_variables, AnalysisOptions
    >>> from pathlib import Path
    >>>
    >>> result = analyze_paragraph_variables(Path("myprogram.cob"))
    >>> print(result.analysis)             # Full analysis JSON
    >>> print(result.paragraph_variables)  # Paragraph-variables map JSON
"""

from .api import (
    analyze_paragraph_variables,
    AnalysisOptions,
    AnalysisResult,
    AnalysisError,
)

__version__ = "0.1.0"

__all__ = [
    "analyze_paragraph_variables",
    "AnalysisOptions",
    "AnalysisResult",
    "AnalysisError",
]
