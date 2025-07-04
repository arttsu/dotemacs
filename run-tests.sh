#!/bin/bash

# GTD Integration Test Runner

echo "🧪 Running GTD Integration Tests..."
echo "================================="

# Run tests with warning suppression
emacs -batch \
      -L tests \
      --eval "(setq warning-suppress-types '((emacs) (org) (org-element)))" \
      --eval "(setq warning-suppress-log-types '((emacs) (org) (org-element)))" \
      --eval "(setq warning-minimum-level :emergency)" \
      --eval "(setq warning-minimum-log-level :emergency)" \
      -l ert \
      -l test-gtd-integration \
      -f run-gtd-tests

# Exit code is handled by run-gtd-tests function