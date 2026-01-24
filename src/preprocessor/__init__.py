"""Preprocessor module for COBOL source code normalization."""

from .copy_resolver import CopyResolver
from .format_detector import SourceFormat, detect_format
from .normalizer import normalize_source

__all__ = ["CopyResolver", "SourceFormat", "detect_format", "normalize_source"]
