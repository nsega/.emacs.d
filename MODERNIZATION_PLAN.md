# Emacs Init.el Modernization Plan for Emacs 30+

## Overview
Modernize your Emacs configuration to use `use-package`, replace deprecated tools (Helm → Vertico ecosystem, gtags/CEDET → Eglot), and convert deprecated `defadvice` to modern `advice-add`.

## User Requirements
- **Moderate modernization**: Migrate to use-package + replace deprecated tools
- **Preserve workflows**: Keep all muscle memory key bindings
- **Modernize code**: Convert deprecated `defadvice` to `advice-add`

## Implementation Progress

- [x] **Phase 1**: Backup and Bootstrap use-package ✅ (Completed: 2026-01-23)
- [x] **Phase 2**: Convert Simple Packages to use-package ✅ (Completed: 2026-01-23)
- [x] **Phase 3**: Replace Helm with Vertico Ecosystem ✅ (Completed: 2026-01-23)
- [x] **Phase 4**: Remove gtags/CEDET, Use Eglot ✅ (Completed: 2026-01-24)
- [x] **Phase 5**: Modernize setup-editing.el - Convert defadvice ✅ (Completed: 2026-01-24)
- [ ] **Phase 6**: Remove auto-complete, Keep Company
- [ ] **Phase 7**: Modernize Claude Code
- [ ] **Phase 8**: Final Cleanup and Organization

## Current State
- **Main config**: `~/.emacs.d/init.el` (572 lines)
- **Custom modules**: 5 files in `~/.emacs.d/custom/`
  - `setup-helm.el` (83 lines) - Helm completion framework
  - `setup-helm-gtags.el` (33 lines) - Tags integration
  - `setup-ggtags.el` (16 lines) - GNU Global tags
  - `setup-cedet.el` (22 lines) - Semantic analysis
  - `setup-editing.el` (299 lines) - **Contains 4 deprecated defadvice instances**

## Tools to Replace

### Completion: Helm → Vertico Ecosystem
- `helm` → `vertico` (vertical completion UI)
- `helm-M-x` → Enhanced `M-x` with vertico
- `helm-imenu` → `consult-imenu`
- `helm-occur` → `consult-line`
- `helm-projectile` → `projectile` with vertico integration
- Add: `marginalia` (rich annotations), `orderless` (flexible matching), `embark` (contextual actions)

### Code Navigation: Tags/CEDET → Eglot (Already Configured!)
- Remove: `helm-gtags`, `ggtags`, `CEDET` (redundant)
- Keep: Eglot (already configured for Python, Go, TypeScript)
- Keep: `dumb-jump` as fallback for non-LSP files

### Auto-complete → Company Only
- Remove: `auto-complete` (deprecated)
- Keep: `company` (modern, works with LSP)

## Implementation Phases

### Phase 1: Backup and Bootstrap use-package ✅ COMPLETED
**Goal**: Add use-package without breaking existing setup

**Steps**:
1. Create timestamped backups:
   ```bash
   cp ~/.emacs.d/init.el ~/.emacs.d/init.el.backup.$(date +%Y%m%d_%H%M%S)
   cp -r ~/.emacs.d/custom ~/.emacs.d/custom.backup.$(date +%Y%m%d_%H%M%S)
   ```

2. Add after `(package-initialize)` in init.el:
   ```elisp
   ;; Bootstrap use-package
   (unless (package-installed-p 'use-package)
     (package-refresh-contents)
     (package-install 'use-package))

   (eval-when-compile (require 'use-package))
   (setq use-package-always-ensure t)
   (setq use-package-compute-statistics t)  ; For performance monitoring
   ```

**Verification**: Start Emacs, check for errors

---

### Phase 2: Convert Simple Packages to use-package ✅ COMPLETED
**Goal**: Migrate packages with minimal configuration

**Convert these packages**:
- `anzu`, `volatile-highlights`, `clean-aindent-mode`, `dtrt-indent`
- `ws-butler`, `undo-tree`, `yasnippet`, `smartparens`
- `comment-dwim-2`, `iedit`, `zygospore`
- Language modes: `go-mode`, `yaml-mode`, `markdown-mode`, `migemo`
- Theme: `vscode-dark-plus-theme`
- Terminals: `vterm`, `eat`

**Example conversion**:
```elisp
;; OLD (manual)
(require 'anzu)
(global-anzu-mode)
(global-set-key (kbd "M-%") 'anzu-query-replace)

;; NEW (use-package)
(use-package anzu
  :config
  (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))
```

**Verification**: Test key bindings for each package

---

### Phase 3: Replace Helm with Vertico Ecosystem ✅ COMPLETED
**Goal**: Modern, lightweight completion framework

**Install new packages**:
```elisp
(use-package vertico
  :init (vertico-mode)
  :config (setq vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-g i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)
         :map minibuffer-local-map
         ("M-p" . consult-history)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))
```

**Update projectile**:
```elisp
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)  ; Use vertico
  :bind-keymap ("C-c p" . projectile-command-map))
```

**Remove**:
- Delete `~/.emacs.d/custom/setup-helm.el`
- Delete `~/.emacs.d/custom/setup-helm-gtags.el`
- Remove helm-related code from init.el (lines 71-72, 75-76, 89, 194-196, 202-226)
- Remove from package list: `helm`, `helm-gtags`, `helm-projectile`

**Key binding migration**:
| Old | New | Key |
|-----|-----|-----|
| `helm-M-x` | `execute-extended-command` | M-x |
| `helm-mini` | `consult-buffer` | C-x b |
| `helm-imenu` | `consult-imenu` | M-g i |
| `helm-occur` | `consult-line` | M-s l |

**Verification**:
- Test M-x (should show vertico UI)
- Test C-x b (consult-buffer)
- Test M-g i in code file (consult-imenu)

---

### Phase 4: Remove gtags/CEDET, Use Eglot ✅ COMPLETED
**Goal**: Simplify by using LSP instead of external tag systems

**Why this works**: User already has Eglot configured for Python, Go, TypeScript (init.el lines 398-466). Eglot provides better code intelligence via LSP.

**Remove**:
- Delete `~/.emacs.d/custom/setup-ggtags.el`
- Delete `~/.emacs.d/custom/setup-cedet.el`
- Remove from init.el: Lines 77-78 (require statements), 212-226 (gtags config)
- Remove from package list: `ggtags`, `helm-gtags`

**Keep**: dumb-jump as fallback (already configured lines 92-96)

**Navigation now uses**:
- `M-.` → `xref-find-definitions` (via Eglot)
- `M-?` → `xref-find-references` (via Eglot)
- `M-,` → `xref-pop-marker-stack`
- Fallback: dumb-jump for non-LSP files

**Verification**:
- Open Python/Go/TypeScript file
- Test M-. on function (jump to definition)
- Test M-? (find references)
- Test M-, (go back)

---

### Phase 5: Modernize setup-editing.el - Convert defadvice ✅ COMPLETED
**Goal**: Replace deprecated `defadvice` with modern `advice-add`

**4 deprecated instances to convert** (in `setup-editing.el`):

1. **kill-ring-save** (line 164-171): Copy line when no region
   ```elisp
   ;; NEW
   (defun my/slick-copy-advice (orig-fun &rest args)
     "Copy line when no region is active."
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (message "Copied line")
        (list (line-beginning-position)
              (line-beginning-position 2))))
     (apply orig-fun args))

   (advice-add 'kill-ring-save :around #'my/slick-copy-advice)
   ```

2. **kill-region** (line 173-179): Cut line when no region
   ```elisp
   (defun my/slick-cut-advice (orig-fun &rest args)
     "Cut line when no region is active."
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (line-beginning-position)
              (line-beginning-position 2))))
     (apply orig-fun args))

   (advice-add 'kill-region :around #'my/slick-cut-advice)
   ```

3. **kill-line** (line 183-191): Clean whitespace
   ```elisp
   (defun my/kill-line-whitespace-advice (&rest _args)
     "Clean whitespace before killing line."
     (when (member major-mode
                   '(emacs-lisp-mode scheme-mode lisp-mode
                     c-mode c++-mode objc-mode
                     latex-mode plain-tex-mode))
       (when (and (eolp) (not (bolp)))
         (forward-char 1)
         (just-one-space 0)
         (backward-char 1))))

   (advice-add 'kill-line :before #'my/kill-line-whitespace-advice)
   ```

4. **yank/yank-pop** (line 212-230): Auto-indent yanked text
   ```elisp
   (defun my/yank-indent-advice (orig-fun &rest args)
     "Auto-indent yanked text in programming modes."
     (let ((result (apply orig-fun args)))
       (when (and (not (car args))
                  (not (member major-mode yank-indent-blacklisted-modes))
                  (or (derived-mode-p 'prog-mode)
                      (member major-mode yank-indent-modes)))
         (let ((transient-mark-mode nil))
           (yank-advised-indent-function (region-beginning) (region-end))))
       result))

   (advice-add 'yank :around #'my/yank-indent-advice)
   (advice-add 'yank-pop :around #'my/yank-indent-advice)
   ```

**Consolidate into init.el**: Move all `setup-editing.el` content into main init.el using `use-package emacs` block

**Verification**:
- Test C-w with no selection (should cut line)
- Test M-w with no selection (should copy line)
- Test C-y in Python file (should auto-indent)

---

### Phase 6: Remove auto-complete, Keep Company
**Goal**: Consolidate to single completion framework

**Remove**:
- Lines 363-378 in init.el (auto-complete config)
- Remove from package list: `auto-complete`

**Enhanced company config**:
```elisp
(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map c-mode-map ("<tab>" . company-complete)
         :map c++-mode-map ("<tab>" . company-complete))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t)
  (delete 'company-semantic company-backends))

(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))
```

**Verification**: Test completion in Python, Go, C files

---

### Phase 7: Modernize Claude Code
**Goal**: Convert to use-package syntax

```elisp
(use-package claude-code
  :ensure nil
  :commands (claude-code claude-code-send claude-code-vterm)
  :init
  (unless (package-installed-p 'claude-code)
    (package-vc-install "https://github.com/stevemolitor/claude-code.el"))
  :config
  (setq claude-code-terminal-type 'vterm
        claude-code-command "claude"
        claude-code-save-before-send t)
  :bind (("C-c c c" . claude-code)
         ("C-c c s" . claude-code-send)
         ("C-c c v" . claude-code-vterm)))
```

---

### Phase 8: Final Cleanup and Organization
**Goal**: Single, well-organized init.el

**Remove obsolete files**:
```bash
rm ~/.emacs.d/custom/setup-helm.el
rm ~/.emacs.d/custom/setup-helm-gtags.el
rm ~/.emacs.d/custom/setup-ggtags.el
rm ~/.emacs.d/custom/setup-cedet.el
rm ~/.emacs.d/custom/setup-editing.el
```

**Remove old require statements** from init.el:
- Lines 75-78: `(require 'setup-helm)` etc.

**Organize init.el into sections**:
1. Package Management
2. Basic Settings
3. Completion Framework (Vertico + Consult)
4. Project Management (Projectile)
5. Editing Enhancements
6. Completion (Company)
7. Language Support (Eglot + LSP)
8. C/C++ Development
9. Navigation (xref)
10. Appearance (Theme, Font)
11. Japanese Configuration
12. Terminal Integration (Claude Code)
13. Key Bindings

---

## Critical Files to Modify

1. **`~/.emacs.d/init.el`** (572 lines)
   - Complete restructure to use-package format
   - Remove helm-related code
   - Remove gtags/CEDET code
   - Remove auto-complete code
   - Consolidate setup-editing.el content
   - Remove require statements for custom/*.el

2. **`~/.emacs.d/custom/setup-editing.el`** (299 lines)
   - Extract all custom functions
   - Convert 4 defadvice instances to advice-add
   - Migrate to use-package blocks in main init.el
   - **Then delete file**

3. **Delete these files**:
   - `~/.emacs.d/custom/setup-helm.el`
   - `~/.emacs.d/custom/setup-helm-gtags.el`
   - `~/.emacs.d/custom/setup-ggtags.el`
   - `~/.emacs.d/custom/setup-cedet.el`

---

## Verification Plan

### After Each Phase
1. Restart Emacs: `emacs`
2. Check `*Messages*` buffer for errors
3. Test phase-specific functionality (see each phase above)

### Final Comprehensive Test
- [ ] Emacs starts without errors
- [ ] All key bindings work (muscle memory test)
- [ ] M-x shows vertico UI
- [ ] C-x b shows consult-buffer with annotations
- [ ] M-g i shows imenu (consult-imenu)
- [ ] M-s l searches buffer (consult-line)
- [ ] C-c p commands work (projectile)
- [ ] M-. jumps to definition in Python/Go/TypeScript
- [ ] M-? shows references
- [ ] Company completion works
- [ ] C-w/M-w on no selection cuts/copies line
- [ ] C-y auto-indents in prog-mode
- [ ] C-c c c starts Claude Code
- [ ] All language modes load correctly (Go, Python, TypeScript, YAML, Markdown)

### Performance Check
```elisp
M-x use-package-report  ; Check package load times
```

---

## Rollback Strategy

### Full Rollback
```bash
cp ~/.emacs.d/init.el.backup.TIMESTAMP ~/.emacs.d/init.el
cp -r ~/.emacs.d/custom.backup.TIMESTAMP ~/.emacs.d/custom
```

### Incremental Rollback
- Comment out problematic `use-package` block
- Restart Emacs
- Debug with `M-x toggle-debug-on-error`

---

## Expected Benefits

1. **Simplicity**: Single init.el (vs 6 files)
2. **Performance**: ~20-30% faster startup (vertico lighter than helm)
3. **Maintainability**: Self-documenting use-package declarations
4. **Modern**: No deprecated code, Emacs 30+ best practices
5. **LSP-first**: Better code intelligence than tags
6. **Memory**: Lower usage (no duplicate navigation systems)
7. **Compatibility**: Preserves all existing workflows

---

## Implementation Timeline

Each phase is designed to be self-contained and testable. The plan can be executed all at once or incrementally over multiple sessions. All existing key bindings and workflows will be preserved throughout the migration.
