#!/usr/bin/env python3
"""Release script for cobol-ast-parser.

Usage:
    python scripts/release.py patch    # 0.1.0 -> 0.1.1
    python scripts/release.py minor    # 0.1.0 -> 0.2.0
    python scripts/release.py major    # 0.1.0 -> 1.0.0
    python scripts/release.py --check  # Show current version without changing

Options:
    --dry-run    Show what would happen without making changes
    --no-tag     Skip creating git tag
    --push       Push commits and tags to origin after release
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

ROOT_DIR = Path(__file__).parent.parent
VERSION_FILE = ROOT_DIR / "src" / "cobol_ast" / "__init__.py"
CHANGELOG_FILE = ROOT_DIR / "CHANGELOG.md"

VERSION_PATTERN = re.compile(r'^__version__\s*=\s*["\'](\d+\.\d+\.\d+)["\']', re.MULTILINE)


def get_current_version() -> str:
    """Read current version from src/__init__.py."""
    content = VERSION_FILE.read_text()
    match = VERSION_PATTERN.search(content)
    if not match:
        raise ValueError(f"Could not find __version__ in {VERSION_FILE}")
    return match.group(1)


def parse_version(version: str) -> tuple[int, int, int]:
    """Parse version string into tuple."""
    parts = version.split(".")
    return int(parts[0]), int(parts[1]), int(parts[2])


def bump_version(current: str, bump_type: str) -> str:
    """Calculate new version based on bump type."""
    major, minor, patch = parse_version(current)

    if bump_type == "major":
        return f"{major + 1}.0.0"
    elif bump_type == "minor":
        return f"{major}.{minor + 1}.0"
    elif bump_type == "patch":
        return f"{major}.{minor}.{patch + 1}"
    else:
        raise ValueError(f"Unknown bump type: {bump_type}")


def update_version_file(new_version: str, dry_run: bool = False) -> None:
    """Update __version__ in src/__init__.py."""
    content = VERSION_FILE.read_text()
    new_content = VERSION_PATTERN.sub(f'__version__ = "{new_version}"', content)

    if dry_run:
        print(f"[DRY RUN] Would update {VERSION_FILE}")
    else:
        VERSION_FILE.write_text(new_content)
        print(f"Updated {VERSION_FILE}")


def update_changelog(new_version: str, dry_run: bool = False) -> None:
    """Add new version section to CHANGELOG.md."""
    from datetime import date

    if not CHANGELOG_FILE.exists():
        if dry_run:
            print(f"[DRY RUN] Would create {CHANGELOG_FILE}")
            return
        # Create initial changelog
        content = f"""# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [{new_version}] - {date.today().isoformat()}

### Added
- Initial release

"""
        CHANGELOG_FILE.write_text(content)
        print(f"Created {CHANGELOG_FILE}")
        return

    content = CHANGELOG_FILE.read_text()

    # Check if version already exists
    if f"## [{new_version}]" in content:
        print(f"Version {new_version} already in changelog, skipping")
        return

    # Insert new version after [Unreleased]
    new_section = f"""## [{new_version}] - {date.today().isoformat()}

### Changed
- (Add changes here)

"""
    content = content.replace(
        "## [Unreleased]\n",
        f"## [Unreleased]\n\n{new_section}"
    )

    if dry_run:
        print(f"[DRY RUN] Would update {CHANGELOG_FILE}")
    else:
        CHANGELOG_FILE.write_text(content)
        print(f"Updated {CHANGELOG_FILE}")


def run_git_command(args: list[str], dry_run: bool = False) -> str:
    """Run a git command."""
    cmd = ["git"] + args
    if dry_run:
        print(f"[DRY RUN] Would run: {' '.join(cmd)}")
        return ""
    result = subprocess.run(cmd, capture_output=True, text=True, cwd=ROOT_DIR)
    if result.returncode != 0:
        raise RuntimeError(f"Git command failed: {result.stderr}")
    return result.stdout.strip()


def check_git_clean() -> bool:
    """Check if git working directory is clean."""
    result = subprocess.run(
        ["git", "status", "--porcelain"],
        capture_output=True,
        text=True,
        cwd=ROOT_DIR
    )
    return len(result.stdout.strip()) == 0


def create_release(
    bump_type: str,
    dry_run: bool = False,
    no_tag: bool = False,
    push: bool = False
) -> None:
    """Create a new release."""
    current = get_current_version()
    new_version = bump_version(current, bump_type)

    print(f"\n{'=' * 50}")
    print(f"Release: {current} -> {new_version}")
    print(f"{'=' * 50}\n")

    if not dry_run and not check_git_clean():
        print("WARNING: Working directory has uncommitted changes.")
        response = input("Continue anyway? [y/N] ")
        if response.lower() != "y":
            print("Aborted.")
            sys.exit(1)

    # Update version file
    update_version_file(new_version, dry_run)

    # Update changelog
    update_changelog(new_version, dry_run)

    # Git commit
    run_git_command(["add", str(VERSION_FILE), str(CHANGELOG_FILE)], dry_run)
    run_git_command(["commit", "-m", f"chore: release v{new_version}"], dry_run)

    # Git tag
    if not no_tag:
        tag_name = f"v{new_version}"
        run_git_command(["tag", "-a", tag_name, "-m", f"Release {new_version}"], dry_run)
        print(f"Created tag: {tag_name}")

    # Push
    if push:
        run_git_command(["push", "origin", "main"], dry_run)
        if not no_tag:
            run_git_command(["push", "origin", f"v{new_version}"], dry_run)
        print("Pushed to origin")

    print(f"\n{'=' * 50}")
    print(f"Released v{new_version}")
    if not push:
        print("\nTo push the release:")
        print(f"  git push origin main && git push origin v{new_version}")
    print(f"{'=' * 50}\n")


def main():
    parser = argparse.ArgumentParser(description="Release script for cobol-ast-parser")
    parser.add_argument(
        "bump_type",
        nargs="?",
        choices=["major", "minor", "patch"],
        help="Version bump type"
    )
    parser.add_argument("--check", action="store_true", help="Show current version")
    parser.add_argument("--dry-run", action="store_true", help="Show what would happen")
    parser.add_argument("--no-tag", action="store_true", help="Skip creating git tag")
    parser.add_argument("--push", action="store_true", help="Push to origin after release")

    args = parser.parse_args()

    if args.check:
        version = get_current_version()
        print(f"Current version: {version}")
        sys.exit(0)

    if not args.bump_type:
        parser.print_help()
        sys.exit(1)

    create_release(
        args.bump_type,
        dry_run=args.dry_run,
        no_tag=args.no_tag,
        push=args.push
    )


if __name__ == "__main__":
    main()
