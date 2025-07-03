# Makefile for Emacs configuration
# Provides convenient targets for common development tasks

.PHONY: all build test tangle clean clean-whitespace help test-and-tangle

# Default target - complete build workflow
all: build

# Complete build workflow: clean whitespace, tangle, test
build:
	@./build.sh

# Clean trailing whitespace from all .org and .el files
clean-whitespace:
	@./clean-whitespace.sh

# Tangle configuration from config.org
tangle:
	@./tangle-config.sh

# Run all tests
test:
	@./run-tests.sh

# Clean compiled elisp files
clean:
	@echo "🧹 Cleaning compiled elisp files..."
	@find . -name "*.elc" -type f -delete
	@echo "✅ Cleaned!"

# Legacy target for backward compatibility
test-and-tangle: test tangle
	@echo "✅ Tests passed and config tangled!"

# Show help
help:
	@echo "Available targets:"
	@echo "  all              - Run complete build workflow (default)"
	@echo "  build            - Run complete build workflow: clean whitespace, tangle, test"
	@echo "  clean-whitespace - Clean trailing whitespace from .org and .el files"
	@echo "  tangle           - Tangle config.org to init.el/early-init.el"
	@echo "  test             - Run all GTD integration tests"
	@echo "  clean            - Remove compiled .elc files"
	@echo "  test-and-tangle  - Legacy: run tests then tangle"
	@echo "  help             - Show this help message"