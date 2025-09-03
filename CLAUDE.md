# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Ceamx is a comprehensive Emacs configuration built for productivity, focusing on a modular architecture with extensive customization. The configuration is primarily written in Emacs Lisp and is organized as a literate configuration using Org mode, with some parts managed through traditional Elisp files.

## Architecture

### Core Structure

- **`init.el`** - Main initialization file that bootstraps the entire configuration
- **`config.org`** - Large literate configuration file (319KB+) containing most configuration blocks
- **`lisp/`** - Custom Elisp modules organized by functionality:
  - `ceamx-paths.el` - Path and directory definitions following XDG standards
  - `ceamx-lib.el` - Core utility functions and macros (including `setq!` macro)
  - `ceamx-simple.el` - Simple utility functions
  - `ceamx-init-*.el` - Feature-specific initialization modules
  - `ceamx-lisp.el`, `ceamx-completion.el` - Language and completion support
- **`site-lisp/`** - Third-party and custom packages not available through package managers
- **`nix/`** - Nix-based package management and development environment

### Configuration Patterns

**Package Management**: Uses both `package.el` (MELPA) and Nix for external dependencies. The `setup.el` macro system is used extensively for package configuration.

**Path Management**: Follows XDG Base Directory specification with custom path constants defined in `ceamx-paths.el`:
- Storage: `~/.local/share/ceamx/`
- Cache: `~/.cache/ceamx/`
- User directories: `~/Documents/notes/`, `~/Documents/reading/`, `~/Projects/`

**Key Bindings**: Extensive custom keymaps using prefix commands:
- `C-c` prefix for user commands organized by topic (bookmarks, capture, files, etc.)
- `C-h` enhanced help system with helpful.el
- `M-g` goto commands with consult integration
- `M-s` search commands

**Modular Design**: Configuration is split into logical modules in the `lisp/` directory, each handling specific aspects (UI, completion, tools, languages, etc.).

## Development Environment

### Nix Integration

This configuration includes Nix flakes for reproducible development:
- Development shell with necessary tools (aspell, pandoc, etc.)
- Custom packages like `editor` script for emacsclient integration
- Org-protocol desktop entry generation

### Custom Libraries

- **`setq!` macro** - Enhanced `setq` that triggers custom setters (from `ceamx-lib.el`)
- **Path utilities** - Functions like `ceamx-subdirs` for directory traversal
- **XDG compliance** - Proper handling of configuration, data, and cache directories

## Key Features

### Completion System
- Vertico for minibuffer completion
- Consult for enhanced commands
- Embark for contextual actions
- Corfu for in-buffer completion
- Orderless completion style

### Note-Taking & Writing
- Org mode with extensive configuration
- Denote for note management
- Typo mode for typography
- Variable pitch fonts in text modes

### Development Tools
- Eglot for language server protocol
- Flymake for syntax checking
- Magit for version control
- Multiple language support modules

### UI & Appearance
- Doric themes with light/dark toggle
- Fontaine for font management
- Spacious padding for better spacing
- Nerd icons integration

## Working with This Codebase

### Common Tasks

**Adding new packages**: Packages are configured using the `setup` macro system in `init.el`. Follow existing patterns for consistency.

**Modifying keybindings**: Key bindings are organized by prefix in the "Keybindings" section of `init.el`. Use the established prefix system.

**Adding custom functions**: Place utility functions in appropriate modules in `lisp/` directory. Simple functions go in `ceamx-simple.el`.

### Development Workflow

1. **Testing changes**: The configuration includes development tools - use the Nix development shell for a consistent environment
2. **Path handling**: Always use the path constants from `ceamx-paths.el` rather than hardcoded paths
3. **Custom variables**: Use the `setq!` macro instead of `setq` for customizable variables

### Important Notes

- The configuration targets Linux primarily (with macOS compatibility)
- Uses XDG directory standards throughout
- Heavily customized with personal preferences - adapt keybindings and settings as needed
- The `config.org` file is very large (319KB+) - use specific sections when making changes
- Package configuration follows a consistent pattern using `setup.el` macros