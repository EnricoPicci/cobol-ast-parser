"""Setup configuration for COBOL AST Parser."""

import re
from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

with open("requirements.txt", "r", encoding="utf-8") as fh:
    requirements = [line.strip() for line in fh if line.strip() and not line.startswith("#")]

# Read version from src/__init__.py (single source of truth)
with open("src/__init__.py", "r", encoding="utf-8") as fh:
    version_match = re.search(r'^__version__\s*=\s*["\']([^"\']+)["\']', fh.read(), re.MULTILINE)
    if not version_match:
        raise RuntimeError("Unable to find __version__ in src/__init__.py")
    version = version_match.group(1)

setup(
    name="cobol-ast-parser",
    version=version,
    author="COBOL Analyzer Team",
    description="A COBOL source code analyzer for variable modification tracking",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/example/cobol-ast-parser",
    package_dir={"": "src"},
    packages=find_packages(where="src"),
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "Topic :: Software Development :: Compilers",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
    ],
    python_requires=">=3.9",
    install_requires=requirements,
    entry_points={
        "console_scripts": [
            "cobol-analyzer=main:main",
        ],
    },
)
