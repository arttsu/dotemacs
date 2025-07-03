.PHONY: test tangle clean

test:
	@./run-tests.sh

tangle:
	@./tangle-config.sh

clean:
	@echo "🧹 Cleaning compiled elisp files..."
	@find . -name "*.elc" -type f -delete
	@echo "✅ Cleaned!"

test-and-tangle: test tangle
	@echo "✅ Tests passed and config tangled!"