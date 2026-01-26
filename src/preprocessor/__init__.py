"""Preprocessor module for COBOL source code normalization."""

from .copy_resolver import CopyResolver, LineMapping
from .format_detector import SourceFormat, detect_format
from .normalizer import normalize_source

__all__ = ["CopyResolver", "LineMapping", "SourceFormat", "detect_format", "normalize_source"]
