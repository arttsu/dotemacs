# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal Emacs configuration built with modern Elisp practices using Elpaca package manager. The configuration is modular and feature-rich, supporting various programming languages and productivity workflows.

## Configuration Architecture

### Core Files
- `init.el` - Main configuration file with all package declarations and settings
- `early-init.el` - Minimal early initialization (package startup disabled)
- `local.el` - Machine-specific overrides (fonts, shells, feature flags)
- `custom.el` - Auto-generated customizations

### Package Management
Uses **Elpaca** as the package manager instead of traditional package.el or straight.el. Elpaca automatically handles package installation, building, and loading.

### Key Configuration Patterns
- All packages use `use-package` declarations through `elpaca-use-package`
- Feature flags in `local.el` control optional functionality (e.g., `my-use-copilot`, `my-use-aider`)
- Platform-specific configuration via helper functions (`my-macos-p`, `my-linux-p`, `my-windows-p`)
- Custom functions prefixed with `my-` to avoid conflicts

## Core Development Tools

### Text Editing & Navigation
- **Vertico** + **Orderless** + **Consult** for completion framework
- **Avy** for jump navigation with embark integration
- **Expand Region** and **Multiple Cursors** for text selection
- **Smartparens** for structured editing
- **Super Save** for automatic file saving

### Development Features
- **LSP Mode** with language-specific configurations
- **Magit** for Git integration
- **Project.el** for project management
- **Flymake** for syntax checking
- **Corfu** for completion UI

### Programming Language Support
- Python (LSP + Pet for virtual environments)
- Scala (LSP Metals)
- Clojure (CIDER + Babashka)
- Various modes: GraphQL, Dockerfile, YAML, Markdown, Fish shell

### Optional AI Tools (controlled by flags)
- **Copilot** integration when `my-use-copilot` is enabled
- **Aider** integration when `my-use-aider` is enabled  
- **GPTel** for ChatGPT interaction

### Productivity Tools
- **Org Mode** with extensive GTD workflow configuration
- **Org Node** for note linking and management
- **Easysession** for session management
- **EMMS** for media playback
- **VTerm** for terminal emulation (when shell configured)

## Org Mode GTD System

Sophisticated Getting Things Done implementation with:
- Personal and open project directories (`~/org-personal`, `~/org-open`)
- Custom capture templates in `capture-templates/` directory
- Automated project/area management with priorities and tags
- Custom agenda views and sorting functions

## Development Commands

### Package Management
```elisp
;; Reinstall a package
(elpaca-rebuild 'package-name)

;; Update all packages  
(elpaca-update-all)

;; View package status
(elpaca-status)
```

### Configuration Reloading
```elisp
;; Reload configuration
(load-file user-init-file)

;; Evaluate current buffer (for testing changes)
(eval-buffer)
```

### Common Keybindings Reference
- `<f5>` - LSP command prefix
- `C-c g` - Magit file dispatch
- `C-c c` - Org capture  
- `C-c a` - Org agenda
- `C-;` - Avy jump to character
- `C-.` - Embark act
- `M-o` - Ace window selection

## Local Configuration

To customize for your environment, modify `local.el`:
- Set `my-font` and `my-font-height` for preferred fonts
- Configure `my-vterm-shell` for terminal integration
- Enable/disable features with flags like `my-use-copilot`, `my-use-ripgrep`

## Notes for Claude Code

- Literal config style! Authoritative file is 'config.org' which is tangled to 'init.el'.
- This configuration has no traditional build/test commands - it's loaded directly by Emacs
- Changes to `init.el` require restarting Emacs or evaluating the modified sections
- Package installations happen automatically via Elpaca when first referenced
- The `elpaca/` directory contains all installed packages and should not be modified directly
- Custom functions and variables use `my-` prefix to avoid namespace conflicts (NOT `my/`)

## Completion System Notes

- Uses **Vertico + Orderless** for completion
- For `completing-read` with defaults, avoid `initial-input` parameter - use empty string and let user type to filter
- Vertico shows all options immediately, no need to pre-fill input field
- Orderless allows fuzzy matching, so users can type partial matches
