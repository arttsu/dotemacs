#!/bin/bash

# GTD Integration Test Runner

echo "🧪 Running GTD Integration Tests..."
echo "================================="

# Run tests with warning suppression
emacs -batch \
      -L tests \
      --eval "(setq warning-suppress-types '((emacs) (org)))" \
      --eval "(setq warning-suppress-log-types '((emacs) (org)))" \
      -l ert \
      -l test-gtd-integration \
      -f run-gtd-tests

# Exit code is handled by run-gtd-tests function