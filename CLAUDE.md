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
- Never tangle files automatically - ask the user to run `M-x org-babel-tangle`
- Use regular `use-package` declarations (not `elpaca-use-package`) after enabling support
- `:ensure` defaults to `t` with elpaca-use-package-mode, use `:ensure nil` for built-ins

### Code Style

- Use `elisp` (not `emacs-lisp`) in org src blocks
- Custom functions use `my-` prefix (not `my/`)
- OS predicates: `my-linux-p`, `my-macos-p`, `my-windows-p`
- Local machine config goes in `local.el` (git-ignored)

### Structure

- Keep sections focused and minimal

## GTD System Documentation

[To be documented - custom Getting Things Done implementation details will go here]
