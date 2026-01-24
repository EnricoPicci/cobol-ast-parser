"""COPY statement resolver for COBOL programs.

This module handles the resolution of COPY statements, which include
external copybook files into the source code. Features:
- Recursive resolution (nested COPYs)
- Circular dependency detection
- REPLACING clause support
- Library/path search
"""

import re
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple
from dataclasses import dataclass, field


@dataclass
class CopyStatement:
    """Represents a parsed COPY statement."""

    copybook_name: str
    library_name: Optional[str] = None
    replacings: List[Tuple[str, str]] = field(default_factory=list)
    line_number: int = 0
    original_text: str = ""


class CopyResolutionError(Exception):
    """Error during COPY statement resolution."""

    pass


class CircularCopyError(CopyResolutionError):
    """Circular COPY dependency detected."""

    def __init__(self, cycle: List[str]):
        self.cycle = cycle
        super().__init__(f"Circular COPY dependency detected: {' -> '.join(cycle)}")


class CopyNotFoundError(CopyResolutionError):
    """Copybook file not found."""

    def __init__(self, copybook_name: str, searched_paths: List[Path]):
        self.copybook_name = copybook_name
        self.searched_paths = searched_paths
        paths_str = ", ".join(str(p) for p in searched_paths)
        super().__init__(
            f"Copybook '{copybook_name}' not found. Searched: {paths_str}"
        )


class CopyResolver:
    """Resolves COPY statements in COBOL source code."""

    # Regex pattern to match COPY statements
    # COPY copybook-name [OF|IN library] [REPLACING ...] .
    COPY_PATTERN = re.compile(
        r"\bCOPY\s+"
        r"([A-Za-z0-9_-]+)"  # Copybook name
        r"(?:\s+(?:OF|IN)\s+([A-Za-z0-9_-]+))?"  # Optional library
        r"(?:\s+REPLACING\s+(.*?))?"  # Optional REPLACING clause
        r"\s*\.",  # Terminating period
        re.IGNORECASE | re.DOTALL,
    )

    # Pattern for REPLACING clause items
    REPLACING_PATTERN = re.compile(
        r"(==.*?==|[A-Za-z0-9_-]+)\s+BY\s+(==.*?==|[A-Za-z0-9_-]+)",
        re.IGNORECASE,
    )

    # Maximum nesting depth (COBOL standard)
    MAX_COPY_DEPTH = 16

    # Common copybook extensions
    COPYBOOK_EXTENSIONS = [".cpy", ".copy", ".cbl", ".cob", ""]

    def __init__(
        self,
        copybook_paths: Optional[List[Path]] = None,
        extensions: Optional[List[str]] = None,
    ):
        """Initialize the COPY resolver.

        Args:
            copybook_paths: List of directories to search for copybooks
            extensions: List of file extensions to try
        """
        self.copybook_paths = copybook_paths or [Path(".")]
        self.extensions = extensions or self.COPYBOOK_EXTENSIONS
        self.resolved_cache: Dict[str, str] = {}
        self.resolution_stack: List[str] = []

    def resolve(self, source: str, source_name: str = "<main>") -> str:
        """Resolve all COPY statements in the source.

        Args:
            source: The COBOL source code
            source_name: Name of the source file (for error messages)

        Returns:
            Source code with all COPY statements resolved

        Raises:
            CircularCopyError: If circular dependencies detected
            CopyNotFoundError: If a copybook cannot be found
            CopyResolutionError: For other resolution errors
        """
        self.resolution_stack = [source_name]
        return self._resolve_recursive(source, 0)

    def _resolve_recursive(self, source: str, depth: int) -> str:
        """Recursively resolve COPY statements.

        Args:
            source: Source code to process
            depth: Current recursion depth

        Returns:
            Resolved source code
        """
        if depth > self.MAX_COPY_DEPTH:
            raise CopyResolutionError(
                f"Maximum COPY nesting depth ({self.MAX_COPY_DEPTH}) exceeded"
            )

        result = source
        offset = 0

        for match in self.COPY_PATTERN.finditer(source):
            copy_stmt = self._parse_copy_statement(match)

            # Check for circular dependency
            if copy_stmt.copybook_name in self.resolution_stack:
                cycle = self.resolution_stack + [copy_stmt.copybook_name]
                raise CircularCopyError(cycle)

            # Load copybook content
            copybook_content = self._load_copybook(
                copy_stmt.copybook_name, copy_stmt.library_name
            )

            # Apply REPLACING clause
            if copy_stmt.replacings:
                copybook_content = self._apply_replacings(
                    copybook_content, copy_stmt.replacings
                )

            # Recursively resolve nested COPYs
            self.resolution_stack.append(copy_stmt.copybook_name)
            copybook_content = self._resolve_recursive(copybook_content, depth + 1)
            self.resolution_stack.pop()

            # Replace the COPY statement with resolved content
            start = match.start() + offset
            end = match.end() + offset

            # Add comment showing the COPY resolution
            comment = f"      * COPY {copy_stmt.copybook_name} resolved\n"
            replacement = comment + copybook_content

            result = result[:start] + replacement + result[end:]
            offset += len(replacement) - (end - start)

        return result

    def _parse_copy_statement(self, match: re.Match) -> CopyStatement:
        """Parse a COPY statement match into a CopyStatement object."""
        copybook_name = match.group(1).upper()
        library_name = match.group(2).upper() if match.group(2) else None

        replacings = []
        if match.group(3):
            replacing_text = match.group(3)
            for rep_match in self.REPLACING_PATTERN.finditer(replacing_text):
                old_text = self._normalize_replacing_text(rep_match.group(1))
                new_text = self._normalize_replacing_text(rep_match.group(2))
                replacings.append((old_text, new_text))

        return CopyStatement(
            copybook_name=copybook_name,
            library_name=library_name,
            replacings=replacings,
            original_text=match.group(0),
        )

    def _normalize_replacing_text(self, text: str) -> str:
        """Normalize REPLACING text by removing pseudo-text delimiters."""
        text = text.strip()
        if text.startswith("==") and text.endswith("=="):
            return text[2:-2].strip()
        return text

    def _load_copybook(
        self, copybook_name: str, library_name: Optional[str]
    ) -> str:
        """Load copybook content from file.

        Args:
            copybook_name: Name of the copybook
            library_name: Optional library/directory name

        Returns:
            Content of the copybook file

        Raises:
            CopyNotFoundError: If copybook cannot be found
        """
        # Check cache first
        cache_key = f"{library_name or ''}:{copybook_name}"
        if cache_key in self.resolved_cache:
            return self.resolved_cache[cache_key]

        searched_paths = []

        for base_path in self.copybook_paths:
            search_dir = base_path
            if library_name:
                search_dir = base_path / library_name

            for ext in self.extensions:
                candidate = search_dir / f"{copybook_name}{ext}"
                searched_paths.append(candidate)

                if candidate.exists() and candidate.is_file():
                    content = candidate.read_text(encoding="utf-8", errors="replace")
                    self.resolved_cache[cache_key] = content
                    return content

                # Also try lowercase
                candidate_lower = search_dir / f"{copybook_name.lower()}{ext}"
                if candidate_lower not in searched_paths:
                    searched_paths.append(candidate_lower)
                    if candidate_lower.exists() and candidate_lower.is_file():
                        content = candidate_lower.read_text(
                            encoding="utf-8", errors="replace"
                        )
                        self.resolved_cache[cache_key] = content
                        return content

        raise CopyNotFoundError(copybook_name, searched_paths)

    def _apply_replacings(
        self, content: str, replacings: List[Tuple[str, str]]
    ) -> str:
        """Apply REPLACING clause substitutions.

        Args:
            content: Copybook content
            replacings: List of (old, new) replacement pairs

        Returns:
            Content with replacements applied
        """
        result = content
        for old_text, new_text in replacings:
            # Use word boundary matching for identifiers
            if old_text.isidentifier():
                pattern = r"\b" + re.escape(old_text) + r"\b"
            else:
                pattern = re.escape(old_text)

            result = re.sub(pattern, new_text, result, flags=re.IGNORECASE)

        return result

    def clear_cache(self):
        """Clear the resolved copybook cache."""
        self.resolved_cache.clear()

    def add_copybook_path(self, path: Path):
        """Add a path to search for copybooks.

        Args:
            path: Directory path to add
        """
        if path not in self.copybook_paths:
            self.copybook_paths.append(path)
