#!/bin/bash

# GTD Integration Test Runner

echo "🧪 Running GTD Integration Tests..."
echo "================================="

# Run the tests and filter out only org version warnings
emacs -batch \
      -L tests \
      -l ert \
      -l test-gtd-integration \
      -f run-gtd-tests 2>&1 | \
      sed '/Warning.*elpaca-use-package loaded before Elpaca activation/d' | \
      sed '/Warning.*org loaded before Elpaca activation/d' | \
      sed '/Warning.*Org version mismatch/d' | \
      sed '/This warning usually appears when a built-in Org version is loaded/,/re-compile\./d' | \
      sed '/prior to the more recent Org version\./d' | \
      sed '/the newer Org version attempt to be loaded later\./d' | \
      sed '/This "other package" is triggering built-in Org version, again/d' | \
      sed '/causing the version mismatch\./d' | \
      sed '/sufficient if the corresponding.*deferring the loading\./d' | \
      sed '/stale \.elc files are still left from the previous build\./d' | \
      sed '/^[[:space:]]*$/d'

# Capture exit code from emacs, not from grep
EXIT_CODE=${PIPESTATUS[0]}

if [ $EXIT_CODE -eq 0 ]; then
    echo "✅ All tests passed!"
else
    echo "❌ Some tests failed!"
fi

exit $EXIT_CODE