# Further Emacs Optimization Plan

## Overview
Advanced optimizations for an already modern Emacs 30+ configuration. These refinements focus on startup performance, directory cleanliness, and leveraging Tree-sitter for superior syntax handling.

## Current State Assessment

| Feature | Current Status |
|---------|---------------|
| use-package | Fully implemented |
| Vertico ecosystem | Fully implemented |
| Eglot LSP | Fully implemented |
| exec-path-from-shell | **Already implemented** (init.el:655-668) |
| early-init.el | Missing |
| no-littering | Missing |
| Tree-sitter modes | Partial (TypeScript only) |

---

## Implementation Phases

### Phase 1: Create `early-init.el` - COMPLETED (2026-01-25)
**Goal**: Faster startup, no UI flickering

**Why**: Code in `early-init.el` runs before the package system and frame initialization. Moving UI-disabling code here prevents the brief flash of menu/tool bars.

**Create `~/.emacs.d/early-init.el`**:
```elisp
;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before init.el, before the package system and GUI
;; are initialized. Use it for frame settings and startup optimizations.

;;; Code:

;; Prevent package.el from loading packages before use-package
(setq package-enable-at-startup nil)

;; Disable UI elements early to prevent flickering
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent resize flickering
(setq frame-inhibit-implied-resize t)

;; Faster startup: temporarily increase GC threshold
;; Will be reset to reasonable value in init.el after startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Prevent early file-name-handler processing (speeds up startup)
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore defaults after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))  ; 16MB
            (setq gc-cons-percentage 0.1)
            (setq file-name-handler-alist my/file-name-handler-alist)))

;;; early-init.el ends here
```

**Update init.el**: Remove these lines (now handled in early-init.el):
- Line 66: `(setq gc-cons-threshold 100000000)` - move to early-init.el
- Line 566: `(menu-bar-mode -1)` - handled by default-frame-alist

**Verification**:
- Restart Emacs, observe no menu/tool bar flicker
- Check `M-x emacs-init-time` for improvement

---

### Phase 2: Add `no-littering`
**Goal**: Keep `.emacs.d` root clean

**Why**: Packages create various files (bookmarks, recentf, projectile cache, undo-tree history) that clutter the repository. `no-littering` redirects them to `var/` and `etc/` subdirectories.

**Add to init.el** (after use-package bootstrap, before other packages):
```elisp
;; ============================================================
;; Directory Organization - no-littering
;; ============================================================
(use-package no-littering
  :demand t  ; Load early to affect all subsequent packages
  :config
  ;; Put auto-save files in var directory
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Put native-comp cache in var directory
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))
```

**Add to .gitignore**:
```
var/
etc/
```

**Verification**:
- Restart Emacs
- Check that `~/.emacs.d/var/` and `~/.emacs.d/etc/` are created
- Verify auto-save files go to `var/auto-save/`

---

### Phase 3: Full Tree-sitter Transition
**Goal**: Superior syntax highlighting and indentation via concrete syntax trees

**Why**: Tree-sitter parses code into an actual syntax tree rather than using regex. This provides:
- More accurate syntax highlighting
- Faster indentation
- Better structural editing potential

**Current status**:
- TypeScript: `typescript-ts-mode` (done)
- Python: `python-mode` + `python-ts-mode` hooks (partial)
- Go: `go-mode` package (not tree-sitter)
- YAML: `yaml-mode` package (not tree-sitter)

**Prerequisites**: Install tree-sitter grammars
```bash
# The grammars should auto-install on first use in Emacs 30+
# Or manually via M-x treesit-install-language-grammar
```

**Update init.el**:

**3a. Python - Full tree-sitter**:
```elisp
;; ============================================================
;; Python Configuration (Tree-sitter)
;; ============================================================
;; Uses python-ts-mode (Emacs 29+) with Eglot
;; LSP servers: pip install python-lsp-server OR basedpyright

;; Prefer tree-sitter mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-hook 'python-ts-mode-hook 'eglot-ensure)

;; Python indentation
(setq python-indent-offset 4)
```

**3b. Go - Full tree-sitter**:
```elisp
;; ============================================================
;; Go Configuration (Tree-sitter)
;; ============================================================
;; Requires: go install golang.org/x/tools/gopls@latest

;; Help project.el find Go project roots
(defun project-find-go-module (dir)
  "Find Go module root for DIR by locating go.mod."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "Return root directory of Go PROJECT."
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;; Prefer tree-sitter mode (built-in, no go-mode package needed)
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

;; File associations
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))

(add-hook 'go-ts-mode-hook
          (lambda ()
            (eglot-ensure)
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (derived-mode-p 'go-ts-mode)
                          (eglot-format-buffer)))
                      nil t)))

;; Gopls configuration
(setq-default eglot-workspace-configuration
              '(:gopls (:staticcheck t :usePlaceholders t)))
```

**3c. YAML - Tree-sitter**:
```elisp
;; ============================================================
;; YAML Configuration (Tree-sitter)
;; ============================================================
;; Optional LSP: npm install -g yaml-language-server

;; Prefer tree-sitter mode (built-in)
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.eyaml\\'" . yaml-ts-mode))

(add-hook 'yaml-ts-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (when (executable-find "yaml-language-server")
              (eglot-ensure))))
```

**Package cleanup**: After tree-sitter transition, these packages become optional fallbacks:
- `go-mode` - Can keep for `go.mod` syntax or remove entirely
- `yaml-mode` - Can remove if yaml-ts-mode works well

**Verification**:
- Open `.py` file: mode line should show `Python[ts]` or `python-ts-mode`
- Open `.go` file: mode line should show `Go[ts]` or `go-ts-mode`
- Open `.yaml` file: mode line should show `YAML[ts]` or `yaml-ts-mode`
- Verify Eglot connects (mode line shows language server)
- Test `M-.` (go to definition), `M-?` (find references)

---

## Phase 4: Cleanup (Optional)
**Goal**: Remove redundant code after migration

**Remove from init.el**:
1. Line 66 `gc-cons-threshold` (moved to early-init.el)
2. Line 566 `menu-bar-mode -1` (handled by early-init.el)
3. Old python-mode hook (line 689) - replaced by python-ts-mode
4. `go-mode` use-package block (lines 718-734) - replaced by go-ts-mode
5. `yaml-mode` use-package block (lines 757-765) - replaced by yaml-ts-mode

**Remove from demo-packages list** (optional, can keep as fallbacks):
- `go-mode`
- `yaml-mode`

---

## Summary

| Phase | Change | Benefit |
|-------|--------|---------|
| 1 | early-init.el | Faster startup, no UI flicker |
| 2 | no-littering | Clean .emacs.d directory |
| 3 | Tree-sitter modes | Better syntax handling |
| 4 | Cleanup | Leaner configuration |

## Expected Results
- **Startup**: ~30-50% faster (early-init.el optimizations)
- **Directory**: Clean root, organized `var/` and `etc/`
- **Syntax**: More accurate highlighting, structural awareness
- **Packages**: Fewer external dependencies (tree-sitter is built-in)

## Notes

### exec-path-from-shell - Already Done
Your configuration already has this properly implemented (init.el:655-668) with:
- GOPATH, GOROOT, PYENV_ROOT, VOLTA_HOME
- Volta bin directory addition
- Pyenv shims addition

No changes needed here.

### Rollback
Keep current init.el as backup before starting:
```bash
cp ~/.emacs.d/init.el ~/.emacs.d/init.el.backup.$(date +%Y%m%d_%H%M%S)
```

---

## Implementation Order

1. **Phase 1** (early-init.el) - Standalone, safe to do first
2. **Phase 2** (no-littering) - Standalone, safe to do anytime
3. **Phase 3** (Tree-sitter) - Test each language individually
4. **Phase 4** (Cleanup) - Only after confirming Phase 3 works

Each phase is independent and can be done incrementally.
