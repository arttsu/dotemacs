#!/bin/bash

# GTD Integration Test Runner

echo "🧪 Running GTD Integration Tests..."
echo "================================="

# Run the tests using batch mode
emacs -batch \
      -L tests \
      -l ert \
      -l test-gtd-integration \
      -f run-gtd-tests

# Capture exit code
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "✅ All tests passed!"
else
    echo "❌ Some tests failed!"
fi

exit $EXIT_CODE