#!/bin/bash

# tangle-config.sh - Tangle Emacs configuration from config.org
# This script tangles config.org to init.el/early-init.el

set -e  # Exit on any error

# Find the .emacs.d directory
EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$EMACS_DIR/config.org"

echo "🔄 Tangling Emacs configuration..."
echo "Working directory: $EMACS_DIR"

# Check if config.org exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo "❌ Error: config.org not found in $EMACS_DIR"
    exit 1
fi

# Tangle the configuration
emacs --batch \
    --eval "(require 'org)" \
    --eval "(progn
              (find-file \"$CONFIG_FILE\")
              (org-babel-tangle)
              (message \"✅ Configuration tangled successfully\"))" 2>/dev/null

echo "✅ Configuration tangled successfully!"
echo "   - init.el and early-init.el updated from config.org"