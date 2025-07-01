#!/bin/bash

# tangle-config.sh - Clean and tangle Emacs configuration
# This script cleans trailing whitespace from config.org and tangles it to init.el/early-init.el

set -e  # Exit on any error

# Find the .emacs.d directory
EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$EMACS_DIR/config.org"

echo "🔧 Emacs Config Tangler"
echo "Working directory: $EMACS_DIR"

# Check if config.org exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo "❌ Error: config.org not found in $EMACS_DIR"
    exit 1
fi

echo "📝 Cleaning trailing whitespace from config.org..."
emacs --batch \
    --eval "(progn
              (find-file \"$CONFIG_FILE\")
              (delete-trailing-whitespace)
              (save-buffer)
              (message \"✅ Cleaned trailing whitespace\"))" 2>/dev/null

echo "🔄 Tangling configuration..."
emacs --batch \
    --eval "(require 'org)" \
    --eval "(progn
              (find-file \"$CONFIG_FILE\")
              (org-babel-tangle)
              (message \"✅ Configuration tangled successfully\"))" 2>/dev/null

echo "✅ Configuration updated successfully!"
echo "   - config.org cleaned"
echo "   - init.el and early-init.el updated"