# CLAUDE.md

This file provides guidance to Claude Code when working with this Emacs configuration.

## Goals

1. **Clean and well-structured config** - Follow best practices, only include necessary functionality
2. **Document custom GTD setup** - Help understand the workflow for future tasks
3. **Prefer built-in functionality** - Use Emacs built-ins when possible instead of custom implementations

## Guidelines

### General Principles

- **Ask when unsure** - About structure, naming conventions, or architectural decisions
- **Suggest improvements** - Point out better practices or built-in alternatives to custom code
- **Verify before changing** - Always check the backed up config.org for existing patterns (TODO: remove this once the migration is complete)
- **Explain changes** - When adding or modifying functionality, explain why and wait for approval

### Working with the Config

- Configuration is in `config.org` which tangles to `init.el` and `early-init.el`
- **Use the tangle script** - Run `./tangle-config.sh` after making changes to clean whitespace and tangle
- **Never tangle files automatically** - Always use the script or ask the user to run `M-x org-babel-tangle`
- Use regular `use-package` declarations (not `elpaca-use-package`) after enabling support
- `:ensure` defaults to `t` with elpaca-use-package-mode, use `:ensure nil` for built-ins
- **Check for package integrations** - When migrating packages, review the old config for special integration configurations (e.g., vertico-multiform settings for specific packages, custom keybindings that bridge packages, or specialized configurations that optimize package interactions)

### Code Style

- Use `elisp` (not `emacs-lisp`) in org src blocks
- Custom functions use `my-` prefix (not `my/`)
- OS predicates: `my-linux-p`, `my-macos-p`, `my-windows-p`
- Local machine config goes in `local.el` (git-ignored)
- Use `:custom` instead of `setq` and `setq-default` in use-package forms
- Keep comments minimal - explain WHY not WHAT
- Write for yourself as the target audience
- Group related settings with empty lines, avoid labeling sections

### Structure

- Keep sections focused and minimal
- **Config organization:**
  - **Early Init** - Minimal startup configuration
  - **Config Helpers** - Utility functions and predicates
  - **Setup** - Package manager, local config, custom file
  - **Core Configuration** - Essential Emacs settings, built-in packages
  - **Appearance** - Themes, fonts, visual customization
  - **Interface & Editing** - Packages that enhance how you interact with Emacs and manipulate text:
    - Completion systems (Vertico, Orderless, Corfu)
    - Enhanced commands (Consult)
    - Navigation (Avy)
    - Context actions (Embark)
    - Text manipulation (Smartparens, Multiple Cursors, etc.)
  - **Tools** - External integrations and utilities
  - **Org & GTD** - Org mode and Getting Things Done workflow packages:
    - Core Org configuration
    - Org Modern (visual enhancements)
    - Org Node (knowledge management)
  - **Programming** - Language-specific and development tools (coming soon)

## GTD System Documentation

For detailed documentation of the Getting Things Done system implementation, see [[file:GTD.org][GTD.org]].

### Test-Driven Development for GTD

**IMPORTANT**: When implementing changes to GTD workflow functions, follow a test-driven approach:

1. **Run tests before changes** - Always run `./run-tests.sh` or `make test` to ensure current functionality works
2. **Write/update tests first** - When adding new GTD functionality, write tests that define the expected behavior
3. **Implement the changes** - Make the minimal changes needed to pass the tests
4. **Run tests after changes** - Verify all tests pass before considering the work complete
5. **Commit with confidence** - The comprehensive test suite ensures no regressions

#### Running Tests

```bash
# Run all GTD tests
./run-tests.sh
# or
make test

# Test and tangle in one command
make test-and-tangle
```

#### Testing Philosophy

**Focus on user-facing behavior, not internal implementation:**

- **Test workflows, not functions** - Test complete user interactions (e.g., "sort a checklist", "mark as won't do") rather than individual helper functions
- **Test outcomes, not internals** - Verify final state (TODO items sorted correctly, properties set) rather than how the code achieves it  
- **Integration over unit tests** - Use real org-mode buffers and actual interactive functions rather than mocking
- **Document expected behavior** - Tests serve as executable documentation of how GTD features should work

**Test organization:**
- Group tests by user functionality (sorting, checklist management, etc.)
- Use descriptive test names that explain the workflow being tested
- Create reusable test utilities for common setup (creating test buffers, checklists, etc.)
- Only test "user-facing" functions that are bound to keys or called interactively

This approach makes tests more maintainable and meaningful - they catch real workflow regressions rather than breaking when internal implementation changes.
