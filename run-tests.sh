#!/bin/bash

# GTD Integration Test Runner

echo "🧪 Running GTD Integration Tests..."
echo "================================="

# Run the tests using batch mode and suppress warnings
emacs -batch \
      -L tests \
      --eval "(setq warning-minimum-level :error)" \
      --eval "(setq warning-minimum-log-level :error)" \
      -l ert \
      -l test-gtd-integration \
      -f run-gtd-tests 2>/dev/null

# Capture exit code
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo "✅ All tests passed!"
else
    echo "❌ Some tests failed!"
fi

exit $EXIT_CODE