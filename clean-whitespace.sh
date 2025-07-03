#!/bin/bash

# clean-whitespace.sh - Efficiently clean trailing whitespace from *.org and *.el files
# Uses sed for fast processing without launching Emacs

set -e  # Exit on any error

# Find the .emacs.d directory
EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "🧹 Cleaning trailing whitespace from all *.org and *.el files..."

# Count files for progress indication
TOTAL_FILES=$(find "$EMACS_DIR" -type f \( -name "*.org" -o -name "*.el" \) | wc -l | tr -d ' ')
echo "   Found $TOTAL_FILES files to process"

# Use sed to remove trailing whitespace efficiently
# -i '' means edit in place (macOS), -i means edit in place (Linux)
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    find "$EMACS_DIR" -type f \( -name "*.org" -o -name "*.el" \) -exec sed -i '' 's/[[:space:]]*$//' {} +
else
    # Linux
    find "$EMACS_DIR" -type f \( -name "*.org" -o -name "*.el" \) -exec sed -i 's/[[:space:]]*$//' {} +
fi

echo "✅ Trailing whitespace cleaned from $TOTAL_FILES files"