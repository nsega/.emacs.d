# .emacs.d

Modern Emacs configuration optimized for Emacs 30+ using best practices and contemporary tools.

## Overview

This configuration has been fully modernized (January 2026) to use:
- **use-package** for declarative package management
- **Vertico ecosystem** for lightweight, fast completion
- **Eglot** for LSP-powered code intelligence
- **Tree-sitter** for accurate syntax highlighting (with fallbacks)
- **early-init.el** for zero-flash startup
- **Pixel-precise scrolling** for native macOS trackpad feel
- **no-littering** for clean repository structure
- **Modern advice-add** (no deprecated code)
- **Single init.el** (consolidated from 6 files)

## Quick Start

```bash
# Clone the repository
git clone https://github.com/nsega/.emacs.d.git ~/.emacs.d

# Install dependencies (see below)
brew install cmigemo ripgrep

# Launch Emacs (packages install automatically)
emacs
```

For detailed usage, see the **[Learning Resources](#learning-resources)** below.

## Requirements

- **Emacs 30.x** or later (29.x also works)
- **macOS** (tested on Apple Silicon)
- **Homebrew** for package management

### Optional but Recommended
```bash
brew install ripgrep      # Fast project search
brew install pandoc       # Markdown preview
```

## Key Features

### Completion Framework (Vertico Ecosystem)
- **vertico** - Fast vertical completion UI
- **consult** - Enhanced commands (buffer switching, search, etc.)
- **marginalia** - Rich annotations in minibuffer
- **embark** - Contextual actions on completion candidates
- **orderless** - Flexible matching (type words in any order!)

### Code Intelligence (Eglot LSP)
- Jump to definition / Find references
- Code completion with Company
- Real-time diagnostics
- Symbol renaming across files
- Documentation on hover

### Project Management
- **Projectile** - Project-aware navigation
- Auto-detects project roots (git, go.mod, etc.)
- Fast file finding and project-wide search

### Editing Enhancements
- **yasnippet** - Template system
- **smartparens** - Smart parenthesis handling
- **iedit** - Multi-cursor editing
- **undo-tree** - Visual undo history
- Smart line copy/cut (no selection needed)
- Auto-indent on yank

### macOS Optimizations
- **Pixel-scroll-precision-mode** - Smooth trackpad scrolling (feels native)
- **early-init.el** - Zero-flash startup (UI disabled before frame init)
- **exec-path-from-shell** - Proper PATH inheritance from shell
- **no-littering** - Clean .emacs.d (files organized in var/ and etc/)

### Language Support
| Language   | Mode               | LSP Server                     | Install Command |
|------------|--------------------|--------------------------------|-----------------|
| Python     | python-ts-mode     | pylsp / basedpyright           | `pip install python-lsp-server` |
| Go         | go-ts-mode         | gopls                          | `go install golang.org/x/tools/gopls@latest` |
| JavaScript | js-ts-mode         | typescript-language-server     | `npm install -g typescript-language-server typescript` |
| TypeScript | typescript-ts-mode | typescript-language-server     | `npm install -g typescript-language-server typescript` |
| Java       | java-ts-mode       | jdtls                          | `brew install jdtls` |
| Kotlin     | kotlin-mode        | kotlin-language-server         | `brew install kotlin-language-server` |
| Groovy     | groovy-mode        | -                              | (syntax highlighting only) |
| C/C++      | c-ts-mode / c++-ts-mode | clangd                    | `xcode-select --install` or `brew install llvm` |
| YAML       | yaml-ts-mode       | yaml-language-server (opt)     | `npm install -g yaml-language-server` |
| Markdown   | markdown-mode      | -                              | `brew install pandoc` (for preview) |

**Tree-sitter modes** (`*-ts-mode`) provide accurate syntax highlighting. Grammars install via `M-x treesit-install-language-grammar`.

### Terminal Integration
- **vterm** - Full-featured terminal emulator
- **eat** - Pure elisp terminal (fallback)
- **Claude Code** - AI pair programming with Claude CLI

#### Claude Code Key Bindings
| Key | Command | Description |
|-----|---------|-------------|
| `C-c c c` | claude-code | Start Claude Code session |
| `C-c c r` | Send region | Send selected code to Claude |
| `C-c c s` | Send command | Send a command to Claude |
| `C-c c t` | Toggle window | Show/hide Claude window |
| `C-c c k` | Kill instance | Kill Claude session |

### Markdown Preview
Live preview with side-by-side editing using EWW (built-in browser).

| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-c l` | Live preview | Toggle live preview (updates as you type) |
| `C-c C-c p` | Preview | Open preview in external browser |
| `C-c C-c v` | Export & view | Export HTML and open in browser |
| `C-c C-c g` | grip-mode | GitHub-flavored preview (exact GitHub rendering) |

**Mermaid diagram support:** Install `npm install -g @mermaid-js/mermaid-cli mermaid-filter` to render Mermaid diagrams. Use `C-c C-c p` (external browser) for Mermaid since EWW has limited image support.

**GitHub preview:** Run `go install github.com/chrishrb/go-grip@latest` for GitHub-flavored rendering with `C-c C-c g` (works offline, no API limits).

## Installation

### 1. Clone Repository
```bash
git clone https://github.com/nsega/.emacs.d.git ~/.emacs.d
```

### 2. Install System Dependencies
```bash
# Required for Japanese search
brew install cmigemo

# Highly recommended for fast search
brew install ripgrep

# Optional: Markdown support
brew install pandoc

# Terminal support (required for Claude Code)
brew install libvterm

# Claude Code CLI (for AI pair programming)
brew install --cask claude
```

### 3. Install LSP Servers (for code intelligence)

#### Python
```bash
pip install python-lsp-server
# Or for better performance:
pip install basedpyright
```

#### Go
```bash
go install golang.org/x/tools/gopls@latest
```

#### TypeScript
```bash
npm install -g typescript-language-server typescript
```

#### Java
```bash
brew install jdtls
```

#### Kotlin
```bash
brew install kotlin-language-server
```

#### YAML (optional)
```bash
npm install -g yaml-language-server
```

### 4. Install Tree-sitter Grammars (optional but recommended)

Tree-sitter provides accurate syntax highlighting. Install grammars in Emacs:
```
M-x treesit-install-language-grammar RET c
M-x treesit-install-language-grammar RET cpp
M-x treesit-install-language-grammar RET python
M-x treesit-install-language-grammar RET go
M-x treesit-install-language-grammar RET javascript
M-x treesit-install-language-grammar RET typescript
M-x treesit-install-language-grammar RET java
M-x treesit-install-language-grammar RET kotlin
```

### 5. Launch Emacs
```bash
emacs
```

On first launch, all packages install automatically from MELPA. This may take 1-2 minutes.

## Essential Key Bindings

### Most Important (Learn These First!)
| Key | Command | Description |
|-----|---------|-------------|
| `M-.` | Jump to definition | LSP-powered navigation |
| `M-,` | Go back | Like browser back button |
| `M-?` | Find references | Show all uses of symbol |
| `M-g i` | Jump in file | Jump to function/section (imenu) |
| `M-s l` | Search buffer | Search with live preview |
| `C-c p f` | Find file | Find file in project |
| `C-x b` | Switch buffer | Buffer list with preview |

### Completion (Vertico)
| Key | Command | Description |
|-----|---------|-------------|
| `M-x` | Execute command | Command palette with vertico |
| `C-x b` | consult-buffer | Switch buffers (shows recent files) |
| `C-x C-f` | find-file | Find/open file |
| `C-j / C-n` | Next item | In vertico completion |
| `C-k / C-p` | Previous item | In vertico completion |

### Search & Navigation
| Key | Command | Description |
|-----|---------|-------------|
| `M-s l` | consult-line | Search lines in buffer (live!) |
| `M-s r` | consult-ripgrep | Fast project-wide search |
| `M-s g` | consult-grep | Grep search in directory |
| `M-g i` | consult-imenu | Jump to function/section |

### Project Management (Projectile)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c p p` | Switch project | List recent projects |
| `C-c p f` | Find file | Find file in project |
| `C-c p a` | Toggle test/code | Switch between code and test |
| `C-c p s g` | Grep project | Search across project |

### Code Intelligence (LSP via Eglot)
| Key | Command | Description |
|-----|---------|-------------|
| `M-.` | xref-find-definitions | Jump to definition |
| `M-?` | xref-find-references | Find all references |
| `M-,` | xref-pop-marker-stack | Go back |
| `C-h .` | eldoc-doc-buffer | Show documentation |
| `C-c r` | eglot-rename | Rename symbol everywhere |

### Editing
| Key | Command | Description |
|-----|---------|-------------|
| `C-;` | iedit-mode | Edit all occurrences |
| `M-%` | anzu-query-replace | Replace with count |
| `C-M-%` | anzu-query-replace-regexp | Regex replace |
| `M-w` | Copy line | Copy line (no selection needed) |
| `C-w` | Cut line | Cut line (no selection needed) |
| `C-c i` | Indent buffer/region | Auto-indent |
| `C-a` | Smart beginning of line | Toggle indent/start |
| `M-o` | Smart open line | Insert line below/above |

### Advanced
| Key | Command | Description |
|-----|---------|-------------|
| `C-.` | embark-act | Show actions for thing at point |
| `C-h B` | embark-bindings | Show all key bindings |

For complete reference, see **[USER_GUIDE.md](USER_GUIDE.md)**

## Learning Resources

This repository includes comprehensive documentation:

### 1. **[USER_GUIDE.md](USER_GUIDE.md)** - Complete Reference
The comprehensive guide covering everything you need:
- Quick reference: Essential key bindings organized by category
- Workflows: 6 common patterns for reading code
- Terminal tips: Terminal-specific compatibility and configuration
- Advanced features: Power user techniques
- Troubleshooting: Solutions to common problems

### 2. **[TUTORIAL.md](TUTORIAL.md)** - Hands-On Exercises
Step-by-step interactive tutorial:
- 6 exercises using real demo code (Go projects)
- Obsidian vault navigation examples
- Power user workflows
- Practice challenges
- Real-world tasks

### 3. **[Modernization Plan](.claude/plans/modernization-for-emacs30.md)** - What Changed
Documentation of the modernization process:
- All 8 phases with detailed steps
- Before/after comparisons
- Performance improvements
- Rollback instructions

**Recommended learning path:**
1. Read USER_GUIDE.md Quick Reference section
2. Try exercises 1-3 in TUTORIAL.md
3. Reference USER_GUIDE.md workflows as needed
4. **Terminal users:** See USER_GUIDE.md Terminal Emacs section

## Directory Structure

```
~/.emacs.d/
├── init.el                      # Main configuration (single file!)
├── README.md                    # This file
├── USER_GUIDE.md               # Complete user guide (reference + workflows + terminal tips)
├── TUTORIAL.md                 # Hands-on exercises and tutorials
├── CLAUDE.md                   # Guidelines for Claude Code
├── LICENSE                     # MIT License
├── .claude/
│   ├── plans/
│   │   └── modernization-for-emacs30.md  # Modernization plan
│   └── settings.local.json     # Local Claude settings
├── elpa/                       # Installed packages (auto-generated)
├── snippets/                   # Yasnippet templates
└── custom.backup.*/            # Backups from modernization
```

**Note:** The old `custom/` directory structure has been consolidated into a single `init.el` using use-package.

## Usage Examples

### Opening a Project
```bash
cd /path/to/your/project
emacs .
```

Emacs opens in Dired (directory editor):
- Press `RET` on files to open them
- Press `^` to go up to parent directory
- Or use `C-c p f` to find files quickly

### Exploring Code
```
1. C-c p f     → Find file in project
2. M-g i       → See all functions
3. M-.         → Jump to definition
4. M-?         → Find references
5. M-,         → Go back
```

### Searching Across Project
```
M-s r          → Type search term
→ Navigate results with C-n/C-p
→ Press RET to jump to match
```

### Renaming a Symbol
```
M-.            → Jump to definition
C-c r          → eglot-rename
→ Type new name
→ RET          → Renames everywhere!
```

## Customization

### Adding New Packages

All packages use `use-package`. Add to `init.el`:

```elisp
(use-package your-package
  :config
  ;; Your configuration
  :bind (("C-c y" . your-command)))
```

### Configuring LSP for New Languages

```elisp
(use-package your-mode
  :hook (your-mode . eglot-ensure)
  :config
  ;; Optional: custom LSP server
  (add-to-list 'eglot-server-programs
               '(your-mode . ("your-lsp-server" "--args"))))
```

### Performance Tuning

Check package load times:
```
M-x use-package-report
```

## Troubleshooting

### LSP Not Working?

1. **Check if Eglot is active:** Look for "Eglot" in mode line
2. **Manually start:** `M-x eglot-ensure`
3. **Restart server:** `M-x eglot-reconnect`
4. **Check server is installed:**
   ```bash
   which gopls
   which pylsp
   which typescript-language-server
   ```

### Search is Slow?

Install ripgrep for blazing fast search:
```bash
brew install ripgrep
```
Then use `M-s r` instead of `M-s g`

### Packages Won't Install?

```bash
# Clean package cache
rm -rf ~/.emacs.d/elpa
emacs  # Packages reinstall automatically
```

### Can't Find Commands?

Check available commands:
```
M-x consult-  [TAB]    # See all consult commands
M-x projectile-  [TAB] # See all projectile commands
C-h B                  # Show all key bindings
```

### Japanese Input Not Working?

```bash
brew install cmigemo
```

Verify dictionary path in init.el matches:
```bash
ls /opt/homebrew/share/migemo/utf-8/migemo-dict
```

### Emacs Opens in Home Directory Instead of Current Directory?

This was fixed in the modernization. Make sure you have the latest version:
```bash
cd ~/.emacs.d
git pull
```

## What's Different from Traditional Configs?

### Before Modernization
- 6 separate files (init.el + custom/*.el)
- Helm for completion (heavy)
- Multiple navigation systems (helm-gtags, ggtags, CEDET)
- Deprecated `defadvice` code
- Manual package management

### After Modernization
- **Single init.el** with use-package
- **Vertico ecosystem** (20-30% faster)
- **Single LSP system** (Eglot only)
- **Tree-sitter parsing** (accurate syntax highlighting)
- **early-init.el** (zero-flash startup)
- **Pixel-precise scrolling** (native macOS feel)
- **no-littering** (clean directory structure)
- **Modern advice-add** (no deprecated code)
- **Declarative package management**

**Result:** Simpler, faster, more maintainable, and feels like a native macOS app!

## Advanced Features

### Orderless Matching

Type search terms in any order:
- `"user test"` matches `test_user_function` or `user_management_test`
- Works in all vertico completions

### Live Previews

- `C-x b` - Preview buffers before switching
- `M-s l` - See matches as you type
- `C-c C-c l` - Markdown live preview (in markdown files)
- Navigate with `C-n`/`C-p` to peek

### Multi-Edit (iedit)

```
M-s l          → Find pattern
C-;            → Edit all occurrences at once
→ Type changes
C-;            → Exit iedit
```

### Embark Actions

```
C-x b          → Buffer list
→ Select buffer
C-.            → Show actions (view, save, kill, diff, etc.)
```

## Performance

- **Startup time:** ~2-3 seconds (packages lazy-load)
- **Search:** < 0.1s for most projects (with ripgrep)
- **LSP response:** Real-time (< 100ms)
- **Memory:** ~50-100MB base, ~200MB with multiple LSP servers

Use `M-x use-package-report` to analyze package load times.

## Contributing

This is a personal configuration, but improvements are welcome:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## License

MIT License - See LICENSE file for details

## Acknowledgments

Built with these amazing packages:
- [vertico](https://github.com/minad/vertico) - Vertical completion
- [consult](https://github.com/minad/consult) - Consulting completing-read
- [marginalia](https://github.com/minad/marginalia) - Rich annotations
- [embark](https://github.com/oantolin/embark) - Contextual actions
- [projectile](https://github.com/bbatsov/projectile) - Project interaction
- [company](https://github.com/company-mode/company-mode) - Completion
- [eglot](https://github.com/joaotavora/eglot) - LSP client
- [claude-code.el](https://github.com/stevemolitor/claude-code.el) - Claude Code integration
- [vterm](https://github.com/akermu/emacs-libvterm) - Terminal emulator

## Additional Resources

- [Emacs Wiki](https://www.emacswiki.org/)
- [Vertico Documentation](https://github.com/minad/vertico)
- [Eglot Manual](https://joaotavora.github.io/eglot/)
- [Projectile Docs](https://docs.projectile.mx/)

---

**Last Updated:** January 2026
**Emacs Version:** 30.x
**Status:** Production Ready

For questions or issues, please open an issue on GitHub.
