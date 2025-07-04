#!/bin/bash

# clean-whitespace.sh - Efficiently clean trailing whitespace from *.org and *.el files
# Uses sed for fast processing without launching Emacs

set -e  # Exit on any error

# Find the .emacs.d directory
EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "🧹 Cleaning trailing whitespace from all *.org and *.el files..."

# Count files for progress indication (excluding package directories)
TOTAL_FILES=$(find "$EMACS_DIR" -type f \( -name "*.org" -o -name "*.el" \) \
    -not -path "*/elpaca/*" \
    -not -path "*/elpa/*" \
    -not -path "*/.git/*" | wc -l | tr -d ' ')
echo "   Found $TOTAL_FILES files to process (excluding package directories)"

# Use sed to remove trailing whitespace efficiently
# -i '' means edit in place (macOS), -i means edit in place (Linux)
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    find "$EMACS_DIR" -type f \( -name "*.org" -o -name "*.el" \) \
        -not -path "*/elpaca/*" \
        -not -path "*/elpa/*" \
        -not -path "*/.git/*" \
        -exec sed -i '' 's/[[:space:]]*$//' {} +
else
    # Linux
    find "$EMACS_DIR" -type f \( -name "*.org" -o -name "*.el" \) \
        -not -path "*/elpaca/*" \
        -not -path "*/elpa/*" \
        -not -path "*/.git/*" \
        -exec sed -i 's/[[:space:]]*$//' {} +
fi

echo "✅ Trailing whitespace cleaned from $TOTAL_FILES files"