#!/bin/bash
# Build script to generate Python parser from COBOL85 ANTLR4 grammar

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GRAMMAR_DIR="$SCRIPT_DIR"
OUTPUT_DIR="$SCRIPT_DIR/../src/parser/generated"

echo "Building COBOL85 parser..."

# Check if ANTLR4 is available
if ! command -v antlr4 &> /dev/null; then
    echo "Error: antlr4 command not found."
    echo "Install ANTLR4 or use the Java jar directly:"
    echo "  java -jar antlr-4.13.1-complete.jar -Dlanguage=Python3 -visitor Cobol85.g4"
    exit 1
fi

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Generate Python lexer, parser, and visitor
cd "$GRAMMAR_DIR"
antlr4 -Dlanguage=Python3 -visitor -o "$OUTPUT_DIR" \
    Cobol85Lexer.g4 \
    Cobol85Parser.g4

echo "Parser generated successfully in $OUTPUT_DIR"
echo "Files generated:"
ls -la "$OUTPUT_DIR"/*.py 2>/dev/null || echo "  (No Python files found - check ANTLR4 output)"
