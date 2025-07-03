#!/bin/bash

# build.sh - Complete build workflow for Emacs configuration
# Runs: 1. Clean whitespace 2. Tangle config 3. Run tests

set -e  # Exit on any error

EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$EMACS_DIR"

echo "🚀 Starting complete build workflow..."
echo "Working directory: $EMACS_DIR"
echo

# Step 1: Clean whitespace
echo "📋 Step 1/3: Cleaning trailing whitespace..."
./clean-whitespace.sh
echo

# Step 2: Tangle configuration
echo "📋 Step 2/3: Tangling configuration..."
./tangle-config.sh
echo

# Step 3: Run tests
echo "📋 Step 3/3: Running tests..."
./run-tests.sh

echo
echo "🎉 Build workflow completed successfully!"
echo "   ✅ Whitespace cleaned"
echo "   ✅ Configuration tangled"
echo "   ✅ All tests passed"