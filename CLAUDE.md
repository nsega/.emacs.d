# CLAUDE.md

This file provides guidance to Claude Code when working with this Emacs configuration repository.

## Overview

This is a modernized Emacs configuration optimized for Emacs 30+ using contemporary tools:
- **use-package** for declarative package management
- **Vertico ecosystem** (vertico, consult, marginalia, embark, orderless) for completion
- **Eglot** for LSP-powered code intelligence
- **project.el** (built-in) for project management (C-x p prefix)
- **Single init.el** (consolidated from 6 files)

## Important Rules

### Implementation Plans
- **Always look for and update implementation plans in `.claude/plans/` before starting work**
- Check if there's an existing plan related to the current task
- Update the plan status as work progresses
- Document completed phases and any deviations

### Code Style
- **Never use emojis** in any documentation files (README.md, guides, etc.)
- Use clear, professional language
- Maintain consistent markdown formatting

### Configuration Changes
- All package configuration must use `use-package` syntax
- Never add deprecated Emacs Lisp (e.g., `defadvice` - use `advice-add` instead)
- Test changes by restarting Emacs to ensure no errors
- Keep init.el organized by sections (Package Management, Completion, LSP, etc.)

### Git Workflow
- **Always use feature branches** - never commit directly to main
- Create a PR for review before merging
- Commit changes frequently with descriptive messages
- Always include "Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>" in commits
- Push changes after each logical unit of work

**PR Workflow:**
1. Create feature branch: `git checkout -b feature/description`
2. Make changes and commit
3. Push branch: `git push -u origin feature/description`
4. Create PR using `gh pr create`
5. Merge after review/approval

## File Structure

```
~/.emacs.d/
├── init.el                      # Main configuration (single file!)
├── README.md                    # Main documentation
├── USER_GUIDE.md               # Complete user guide (reference + workflows + terminal tips)
├── TUTORIAL.md                 # Hands-on exercises and tutorials
├── LICENSE                     # MIT License
├── CLAUDE.md                   # This file
├── .claude/
│   ├── plans/
│   │   └── modernization-for-emacs30.md  # Modernization plan
│   └── settings.local.json     # Local Claude settings
├── elpa/                       # Installed packages (auto-generated)
├── snippets/                   # Yasnippet templates
└── custom.backup.*/            # Backups from modernization
```

## Key Bindings Philosophy

- Preserve existing key bindings (muscle memory is important)
- Provide terminal-friendly alternatives for Meta key conflicts
- Document all custom bindings in guides
- Use standard Emacs conventions where possible

## Testing Protocol

When making changes to init.el:

1. **Syntax check**: Ensure valid Emacs Lisp
2. **Test startup**: Restart Emacs and check for errors
3. **Test functionality**: Verify the changed feature works
4. **Check performance**: Use `M-x use-package-report` if adding packages
5. **Update documentation**: Reflect changes in README.md or guides if needed

## Common Tasks

### Adding a New Package
```elisp
(use-package package-name
  :ensure t
  :config
  ;; Configuration here
  :bind (("C-c x" . package-command)))
```

### Adding LSP Support for New Language
```elisp
(use-package language-mode
  :hook (language-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(language-mode . ("lsp-server" "--args"))))
```

### Updating Documentation
- README.md - Main entry point, keep high-level
- USER_GUIDE.md - Complete reference (key bindings + workflows + terminal tips + troubleshooting)
- TUTORIAL.md - Hands-on exercises with real code examples

## Modernization History

This configuration was modernized in January 2024 through 8 phases:
1. Bootstrap use-package
2. Convert simple packages
3. Replace Helm → Vertico ecosystem
4. Remove gtags/CEDET (use Eglot only)
5. Modernize defadvice → advice-add
6. Remove auto-complete (keep Company only)
7. Modernize Claude Code integration
8. Final cleanup and organization

**Implementation plan location:**
- `.claude/plans/modernization-for-emacs30.md` - Complete modernization plan

## Target Audience

- Primary user: Software engineer with macOS
- Uses: Python, Go, TypeScript, C/C++
- Workflow: Project-based development with LSP support
- Preference: Fast, minimal configuration with modern tools

## Troubleshooting Common Issues

### LSP not working
- Check Eglot is active: Look for "Eglot" in mode line
- Manually start: `M-x eglot-ensure`
- Restart server: `M-x eglot-reconnect`

### Terminal key bindings
- Meta key conflicts: Use ESC as alternative
- M-s prefix issues: Use C-c s alternatives
- See TERMINAL_TIPS.md for full guide

### Performance issues
- Check package load times: `M-x use-package-report`
- Consider lazy loading more packages
- Ensure ripgrep is installed for fast search

## References

- [Vertico GitHub](https://github.com/minad/vertico)
- [Eglot Manual](https://joaotavora.github.io/eglot/)
- [use-package Documentation](https://github.com/jwiegley/use-package)
- [project.el Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)
