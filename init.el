;;; init.el --- Modern Emacs Configuration -*- lexical-binding: t; -*-

;; Author: Naoki Sega
;; Maintainer: Naoki Sega
;; URL: https://github.com/nsega/.emacs.d
;; Keywords: convenience

;;; Commentary:

;; Modern Emacs configuration using use-package for declarative package management.
;; Migrated from traditional setup to use modern best practices for Emacs 30+.
;;
;; Key Features:
;; - Vertico ecosystem for lightweight, fast completion (replaces Helm)
;; - Eglot LSP for intelligent code navigation (Python, Go, TypeScript)
;; - Company-mode for auto-completion
;; - Modern advice-add instead of deprecated defadvice
;; - Single, well-organized init.el (no separate custom/*.el files)
;;
;; Package Management:
;; - Uses use-package for all package configuration
;; - Lazy loading where appropriate for fast startup
;; - Self-documenting package declarations
;;
;; Completion Framework:
;; - vertico: Vertical completion UI
;; - consult: Enhanced completing-read commands
;; - marginalia: Rich annotations in minibuffer
;; - embark: Contextual actions on completion candidates
;; - orderless: Flexible matching style
;;
;; Code Intelligence:
;; - Eglot: Built-in LSP client (Emacs 29+)
;; - Company: Auto-completion framework
;; - dumb-jump: Fallback navigation for non-LSP files
;;
;; For more information, see: https://github.com/nsega/.emacs.d

;;; Code:

;; ============================================================
;; Package Management
;; ============================================================
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ============================================================
;; Bootstrap use-package
;; ============================================================
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)  ; For performance monitoring via M-x use-package-report

;; Pre-configure claude-code to use vterm (must be set before package loads)
(setq claude-code-terminal-type 'vterm)

;; ============================================================
;; Basic Settings
;; ============================================================
(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

;; Suppress byte-compile warnings from packages
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; PATH setup is handled by exec-path-from-shell (see below)
;; This ensures Emacs inherits PATH from your shell
(setq exec-path (cons "/opt/homebrew/bin" exec-path))
(setq exec-path (cons "/usr/local/bin" exec-path))

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
  '(anzu
    company
    ;; duplicate-thing  ; removed - no longer available on MELPA
    ;; Completion framework - Vertico ecosystem (replaces Helm)
    vertico
    orderless
    marginalia
    consult
    embark
    embark-consult
    migemo
    ;; function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    zygospore
    ;; Modern language modes
    exec-path-from-shell  ; Better PATH handling on macOS
    go-mode               ; Go language support
    yaml-mode             ; YAML support
    markdown-mode         ; Markdown support
    ;; Navigation fallback
    dumb-jump             ; Jump to definition without tags/LSP
    ;; Terminal
    vterm                 ; Terminal emulator for Claude Code
    eat                   ; Emulate A Terminal (another terminal emulator)
    ;; Theme
    vscode-dark-plus-theme ; VS Code Dark+ theme
    ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; ============================================================
;; Navigation Enhancements
;; ============================================================

;; Global xref keybinding for find-references
(global-set-key (kbd "M-?") 'xref-find-references)

;; ============================================================
;; dumb-jump - Fallback navigation when no tags/LSP available
;; ============================================================
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)  ; Use ripgrep if available
  (dumb-jump-force-searcher nil))

;; ============================================================
;; Completion - Company Mode
;; ============================================================
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (delete 'company-semantic company-backends)
  ;; Add tab completion for C/C++ modes
  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'company-complete)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'company-complete)))
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t))

(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "linux" ;; set style to "linux"
 )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; ============================================================
;; Completion Framework - Vertico Ecosystem
;; ============================================================

;; vertico - Vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)))

;; orderless - Flexible completion matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; marginalia - Rich annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; consult - Consulting completing-read
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-g i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)
         ("M-y" . consult-yank-pop)
         ;; Alternative bindings for terminal (M-s often conflicts)
         ("C-c s l" . consult-line)       ; Alternative to M-s l
         ("C-c s g" . consult-grep)       ; Alternative to M-s g
         ("C-c s r" . consult-ripgrep)    ; Alternative to M-s r
         ("C-c s i" . consult-imenu)      ; Alternative to M-g i
         :map minibuffer-local-map
         ("M-p" . consult-history)))

;; embark - Contextual actions
(use-package embark
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings)))

;; embark-consult - Integration between embark and consult
(use-package embark-consult
  :after (embark consult))

;; ============================================================
;; Editing Enhancements (use-package)
;; ============================================================

;; Package: clean-aindent-mode - Clean auto-indent and backspace behavior
(use-package clean-aindent-mode
  :hook (prog-mode . clean-aindent-mode))

;; Package: dtrt-indent - Detect indent style automatically
(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))

;; Package: ws-butler - Trim whitespace only on edited lines
(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode)
         (fundamental-mode . ws-butler-mode)))

;; Package: yasnippet - Template system
(use-package yasnippet
  :config
  (yas-global-mode 1)
  ;; Don't activate in terminal mode
  (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
  :custom
  (yas-verbosity 1)
  (yas-wrap-around-region t)
  (yas-prompt-functions '(yas/ido-prompt yas/completing-prompt)))

;; Package: smartparens - Parenthesis management
(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1))

;; ============================================================
;; Project Management - Projectile
;; ============================================================
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)  ; Use vertico/completing-read
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Package: zygospore - Reversible C-x 1 (delete-other-windows)
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

;; Package: anzu - Show match count in mode-line while searching
(use-package anzu
  :config
  (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; Package: volatile-highlights - Highlight changes from yanking, undo, etc.
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; Package: undo-tree - Visualize undo history as a tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; Package: comment-dwim-2 - Smarter comment/uncomment
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

;; Package: iedit - Edit multiple occurrences simultaneously
(use-package iedit
  :custom
  (iedit-toggle-key-default nil)
  :bind (("C-;" . iedit-mode)))

;; ============================================================
;; Custom Editing Behaviors (modernized from setup-editing.el)
;; ============================================================

;; Enhanced kill/copy/yank behaviors using modern advice-add

;; 1. Copy line when no region is active (M-w)
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

;; 2. Cut line when no region is active (C-w)
(defun my/slick-cut-advice (orig-fun &rest args)
  "Cut line when no region is active."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2))))
  (apply orig-fun args))

(advice-add 'kill-region :around #'my/slick-cut-advice)

;; 3. Clean whitespace before killing line (C-k)
(defun my/kill-line-whitespace-advice (&rest _args)
  "Clean whitespace before killing line in programming modes."
  (when (member major-mode
                '(emacs-lisp-mode scheme-mode lisp-mode
                  c-mode c++-mode objc-mode
                  latex-mode plain-tex-mode))
    (when (and (eolp) (not (bolp)))
      (forward-char 1)
      (just-one-space 0)
      (backward-char 1))))

(advice-add 'kill-line :before #'my/kill-line-whitespace-advice)

;; 4. Auto-indent yanked text in programming modes (C-y, M-y)
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

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

;; Other useful editing functions from setup-editing.el

;; Smart move to beginning of line (C-a)
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

;; Smart open line (M-o)
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'prelude-smart-open-line)

;; Indent buffer or region (C-c i)
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; ============================================================
;; Speedbar
(setq speedbar-show-unknown-files t)

;; NOTE: Removed old macOS Mavericks workaround that forced home directory
;; This was causing issues with `emacs .` not respecting current directory
;; If you need to start in home directory, use: emacs ~
;;
;; Old code (commented out):
;; ;;　Changing the home directory as the initial dir(for Mervelicks)
;; (setq default-directory "~/")
;; (setq command-line-default-directory "~/")
;;
;; ;; Changing the default directory as of '〜/' for Mavericks
;; (defun cd-to-homedir-all-buffers ()
;;   "Change every current directory of all buffers to the home directory."
;;   (mapc
;;    (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
;; (add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

;; Japanese Configuration (UTF-8)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

;; key bindings
(windmove-default-keybindings 'meta)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)
(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete

;; Enabling Font Lock mode (e.g. bold display)
(global-font-lock-mode t)

;; Enabling automcatically the temporarily mode
(setq-default transient-mark-mode t)

;; Changing C-x C-u to be worked nothing (undo / typo countermeasure)
(global-unset-key "\C-x\C-u")

;; High lighting the pared parenethesis
(show-paren-mode 1)

(setq next-line-add-newlines nil)

;; C-x l/ run goto-line
(define-key ctl-x-map "l" 'goto-line)
;; alternate of '¥', entering '/'
(define-key global-map [?¥] [?\\])

(display-time)

;; display the column number
(column-number-mode 1)

;; disable menu bar
(menu-bar-mode -1)

;; Make C-h act as backspace
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-x\C-h" 'help-command)
;; Also bind in key-translation-map to override help-map
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; Override in isearch-mode
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key global-map "\C-o" 'dabbrev-expand)
(setq dabbrev-case-fold-search nil)

;;; Don't create the backup file
(setq backup-inhibited t)

;;; Deleting the save files when it was exited.
(setq delete-auto-save-files t)

;; Configuration for Japanese and English
;; http://www.alles.or.jp/~torutk/oojava/meadow/Meadow210Install.html
(defun my/dabbrev-expand-japanese-advice (orig-fun &rest args)
  "Modify `dabbrev-abbrev-char-regexp' dynamically for Japanese words."
  (if (bobp)
      (apply orig-fun args)
    (let ((dabbrev-abbrev-char-regexp
           (let ((c (char-category-set (char-before))))
             (cond
              ((aref c ?a) "[-_A-Za-z0-9]") ; ASCII
              ((aref c ?j) ; Japanese
               (cond
                ((aref c ?K) "\\cK") ; katakana
                ((aref c ?A) "\\cA") ; 2byte alphanumeric
                ((aref c ?H) "\\cH") ; hiragana
                ((aref c ?C) "\\cC") ; kanji
                (t "\\cj")))
              ((aref c ?k) "\\ck") ; hankaku-kana
              ((aref c ?r) "\\cr") ; Japanese roman ?
              (t dabbrev-abbrev-char-regexp)))))
      (apply orig-fun args))))

(advice-add 'dabbrev-expand :around #'my/dabbrev-expand-japanese-advice)

;; Configuration Backspace
(delete-selection-mode 1)

;; The local variables list in .emacs と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax)

;; http://0xcc.net/blog/archives/000041.html
(set-default-coding-systems 'utf-8)

;; ============================================================
;; Theme and Colors
;; ============================================================
(use-package vscode-dark-plus-theme
  :config
  (load-theme 'vscode-dark-plus t))

;; Font configuration
(when window-system
  (set-frame-font "Monaco-12" nil t)
  (set-frame-parameter nil 'alpha 100))

(setq default-frame-alist
  (append
   '((font . "Monaco-12") ;; Default Fontset
  (width . 140) (height . 50) ;; Window Size
  )
  default-frame-alist))

;; ============================================================
;; migemo - Japanese incremental search with Romanization
;; ============================================================
;; Requires: brew install cmigemo
(use-package migemo
  :if (executable-find "cmigemo")
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary "/opt/homebrew/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  (migemo-init))

;; ============================================================
;; exec-path-from-shell - Better PATH handling on macOS
;; ============================================================
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  ;; Copy additional environment variables for development tools
  (exec-path-from-shell-copy-envs '("GOPATH" "GOROOT" "PYENV_ROOT" "VOLTA_HOME")))

;; ============================================================
;; Eglot - Built-in LSP client (Emacs 29+)
;; ============================================================
(require 'eglot)

;; Don't log every event (improves performance)
(setq eglot-events-buffer-size 0)

;; Shutdown server when last managed buffer is killed
(setq eglot-autoshutdown t)

;; ============================================================
;; Python Configuration
;; ============================================================
;; Uses built-in python-mode with Eglot
;; LSP servers (install one): pip install python-lsp-server
;;                        or: pip install basedpyright
;;                        or: pip install ruff

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)

;; Python indentation
(setq python-indent-offset 4)

;; Format on save for Python (optional - uncomment if desired)
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

;; ============================================================
;; Go Configuration
;; ============================================================
;; Requires: go install golang.org/x/tools/gopls@latest

;; Help project.el find Go project roots (look for go.mod)
(defun project-find-go-module (dir)
  "Find Go module root for DIR by locating go.mod."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  "Return root directory of Go PROJECT."
  (cdr project))

;; Add Go module detection to project-find-functions
(add-hook 'project-find-functions #'project-find-go-module)

(use-package go-mode
  :hook ((go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (go-mode . (lambda ()
                      (setq indent-tabs-mode t)
                      (setq tab-width 4)))
         (go-mode . (lambda ()
                      (add-hook 'before-save-hook
                                (lambda ()
                                  (when (derived-mode-p 'go-mode 'go-ts-mode)
                                    (eglot-format-buffer)))
                                nil t))))
  :config
  ;; Gopls configuration
  (setq-default eglot-workspace-configuration
                '(:gopls (:staticcheck t
                          :usePlaceholders t))))

;; ============================================================
;; TypeScript Configuration
;; ============================================================
;; Uses built-in typescript-ts-mode (Emacs 29+)
;; Requires: npm install -g typescript-language-server typescript

;; File associations for TypeScript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

;; TypeScript indentation
(setq typescript-ts-mode-indent-offset 2)

;; ============================================================
;; YAML Configuration
;; ============================================================
;; Optional LSP: npm install -g yaml-language-server

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)
         ("\\.eyaml\\'" . yaml-mode))
  :hook ((yaml-mode . (lambda ()
                        (when (executable-find "yaml-language-server")
                          (eglot-ensure))))
         (yaml-mode . (lambda ()
                        (setq indent-tabs-mode nil)
                        (setq tab-width 2)))))

;; ============================================================
;; Markdown Configuration
;; ============================================================
;; Optional: brew install pandoc (for preview/export)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; Use pandoc for markdown processing if available
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc"))
  :custom
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t))

;; ============================================================
;; Terminal Emulators
;; ============================================================
;; vterm - Full-featured terminal emulator (requires libvterm)
;; Requires: brew install libvterm
(use-package vterm
  :ensure t
  :demand t  ; Load eagerly so it's available for claude-code
  :config
  ;; Disable modes that interfere with terminal input
  (defun my/vterm-mode-setup ()
    "Setup vterm buffer for proper terminal input."
    (setq-local global-hl-line-mode nil)
    (setq-local line-spacing nil)
    ;; Disable modes that intercept keyboard input
    (smartparens-mode -1)
    (company-mode -1)
    (yas-minor-mode -1)
    ;; Ensure vterm handles all input
    (setq-local scroll-margin 0))
  (add-hook 'vterm-mode-hook #'my/vterm-mode-setup))

;; eat - Emulate A Terminal (pure elisp, no external dependencies)
(use-package eat
  :commands eat)

;; ============================================================
;; Claude Code Integration
;; ============================================================
;; Requires: brew install --cask claude-code (already installed)
;;           brew install libvterm (already installed)

;; transient - Required dependency for claude-code.el (menu system)
(use-package transient
  :ensure t)

;; inheritenv - Required dependency for claude-code.el (environment handling)
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :demand t  ; Load immediately, don't defer
  :after vterm  ; Ensure vterm is loaded first
  :init
  ;; Set these BEFORE package loads (defcustom defaults are read at load time)
  (setq claude-code-terminal-type 'vterm)   ; Use vterm for best TUI experience
  (setq claude-code-program "claude")       ; CLI program name
  (require 'vterm)  ; Ensure vterm is available
  :config
  (claude-code-mode)  ; Enable global minor mode for IDE integration
  ;; Set up keybindings explicitly (more reliable than :bind-keymap with :vc)
  (global-set-key (kbd "C-c c") claude-code-command-map))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(anzu clean-aindent-mode comment-dwim-2 company consult
          dtrt-indent dumb-jump eat embark embark-consult
          exec-path-from-shell go-mode iedit marginalia markdown-mode
          migemo orderless projectile smartparens undo-tree vertico
          volatile-highlights vscode-dark-plus-theme vterm ws-butler
          yaml-mode yasnippet zygospore)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
