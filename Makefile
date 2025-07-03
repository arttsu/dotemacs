# Makefile for Emacs configuration
# Provides convenient targets for common development tasks

.PHONY: all build test tangle clean clean-whitespace help test-and-tangle test-agenda create-golden-files update-golden-files

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

# Run only agenda tests
test-agenda:
	@echo "🧪 Running GTD Agenda Tests..."
	@echo "================================"
	@emacs --batch -L tests \
		--eval "(setq warning-suppress-types '((emacs) (org)))" \
		--eval "(setq warning-suppress-log-types '((emacs) (org)))" \
		-l ert -l test-gtd-integration \
		--eval "(ert-run-tests-batch-and-exit \"test-gtd-day-agenda\")"

# Create golden files (for initial setup or review)
create-golden-files:
	@echo "📋 Creating Golden Files for Review..."
	@echo "====================================="
	@emacs --batch -L tests \
		--eval "(setq warning-suppress-types '((emacs) (org)))" \
		--eval "(setq warning-suppress-log-types '((emacs) (org)))" \
		-l ert -l test-gtd-integration \
		--eval "(ert-run-tests-batch-and-exit \"test-gtd-create-golden-files\")" || true
	@echo ""
	@echo "📁 Golden files created in tests/test-data/"
	@echo "📝 Please review the files before committing:"
	@ls -la tests/test-data/*.txt
	@echo ""
	@echo "💡 If the output looks correct, these files will be used as the expected output for future tests."

# Update golden files after intentional changes
update-golden-files:
	@echo "🔄 Updating Golden Files from .FAILED versions..."
	@echo "==============================================="
	@if [ -f tests/test-data/day-agenda-comprehensive.txt.FAILED ]; then \
		echo "📝 Updating comprehensive golden file..."; \
		cp tests/test-data/day-agenda-comprehensive.txt.FAILED tests/test-data/day-agenda-comprehensive.txt; \
		rm tests/test-data/day-agenda-comprehensive.txt.FAILED; \
		echo "✅ Updated day-agenda-comprehensive.txt"; \
	else \
		echo "⚠️  No .FAILED file found for comprehensive test"; \
	fi
	@if [ -f tests/test-data/day-agenda-minimal.txt.FAILED ]; then \
		echo "📝 Updating minimal golden file..."; \
		cp tests/test-data/day-agenda-minimal.txt.FAILED tests/test-data/day-agenda-minimal.txt; \
		rm tests/test-data/day-agenda-minimal.txt.FAILED; \
		echo "✅ Updated day-agenda-minimal.txt"; \
	else \
		echo "⚠️  No .FAILED file found for minimal test"; \
	fi
	@echo ""
	@echo "✅ Golden files updated! Run 'make test-agenda' to verify tests pass."

# Show help
help:
	@echo "Available targets:"
	@echo "  all                - Run complete build workflow (default)"
	@echo "  build              - Run complete build workflow: clean whitespace, tangle, test"
	@echo "  clean-whitespace   - Clean trailing whitespace from .org and .el files"
	@echo "  tangle             - Tangle config.org to init.el/early-init.el"
	@echo "  test               - Run all GTD integration tests"
	@echo "  test-agenda        - Run only GTD agenda tests"
	@echo "  create-golden-files - Create/recreate golden files for agenda tests"
	@echo "  update-golden-files - Update golden files from .FAILED versions after review"
	@echo "  clean              - Remove compiled .elc files"
	@echo "  test-and-tangle    - Legacy: run tests then tangle"
	@echo "  help               - Show this help message"
	@echo ""
	@echo "Golden File Testing Workflow:"
	@echo "  1. When agenda tests fail, .FAILED files are created for manual review"
	@echo "  2. Compare .FAILED files with expected golden files"
	@echo "  3. If changes are intentional, run 'make update-golden-files'"
	@echo "  4. Run 'make test-agenda' to verify tests now pass"