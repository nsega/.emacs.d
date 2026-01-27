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
;; - Tree-sitter modes for Python, Go, YAML (accurate syntax highlighting)
;; - Company-mode for auto-completion
;; - Modern advice-add instead of deprecated defadvice
;; - Single, well-organized init.el (no separate custom/*.el files)
;; - early-init.el for faster startup and no UI flickering
;; - no-littering for clean .emacs.d directory
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

;; ============================================================
;; Directory Organization - no-littering
;; ============================================================
;; Keep .emacs.d clean by redirecting package files to var/ and etc/
(use-package no-littering
  :demand t  ; Load early to affect all subsequent packages
  :config
  ;; Put auto-save files in var directory
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Put native-comp cache in var directory (only if native-comp is available)
  (when (and (fboundp 'startup-redirect-eln-cache)
             (boundp 'native-comp-eln-load-path))
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

;; Pre-configure claude-code to use vterm (must be set before package loads)
(setq claude-code-terminal-backend 'vterm)

;; ============================================================
;; Basic Settings
;; ============================================================
;; NOTE: gc-cons-threshold is set in early-init.el for faster startup
;; and automatically reset after Emacs finishes loading
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

;; ============================================================
;; macOS Clipboard Integration
;; ============================================================
;; Sync kill ring with macOS system clipboard via pbcopy/pbpaste
(when (eq system-type 'darwin)
  (setq interprogram-cut-function
        (lambda (text &optional _)
          (let ((process-connection-type nil))
            (let ((proc (start-process "pbcopy" nil "pbcopy")))
              (process-send-string proc text)
              (process-send-eof proc)))))
  (setq interprogram-paste-function
        (lambda ()
          (let ((clipboard (shell-command-to-string "pbpaste")))
            (unless (string= clipboard (car kill-ring))
              clipboard)))))

;; ============================================================
;; macOS Native Scrolling
;; ============================================================
;; Enable pixel-precise scrolling for smooth trackpad experience
;; Makes Emacs feel like a native macOS application (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

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
    volatile-highlights
    undo-tree
    zygospore
    ;; Modern language modes
    exec-path-from-shell  ; Better PATH handling on macOS
    go-mode               ; Go support (fallback if tree-sitter grammar unavailable)
    yaml-mode             ; YAML support (fallback if tree-sitter grammar unavailable)
    typescript-mode       ; TypeScript support (fallback if tree-sitter grammar unavailable)
    web-mode              ; TSX/JSX support (fallback if tree-sitter grammar unavailable)
    markdown-mode         ; Markdown support
    ;; JVM languages
    kotlin-mode           ; Kotlin support (fallback if tree-sitter grammar unavailable)
    groovy-mode           ; Groovy/Gradle support
    ;; Navigation fallback
    dumb-jump             ; Jump to definition without tags/LSP
    ;; Terminal
    vterm                 ; Terminal emulator for Claude Code
    eat                   ; Emulate A Terminal (another terminal emulator)
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
;; M-. (xref-find-definitions) and M-? (xref-find-references) are
;; built-in bindings. Eglot automatically registers as xref backend.

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
         ("C-c e b" . embark-bindings)))  ; C-h B conflicts with C-h as backspace

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
;; Project Management - Built-in project.el (Emacs 28+)
;; ============================================================
;; project.el is built-in and integrates natively with Eglot, xref, and Consult.
;; Default prefix: C-x p

(setq project-switch-commands
      '((project-find-file "Find file" ?f)
        (project-find-regexp "Find regexp" ?g)
        (project-find-dir "Find directory" ?d)
        (project-eshell "Eshell" ?e)))

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

;; NOTE: menu-bar, tool-bar, and scroll-bars are disabled in early-init.el
;; to prevent UI flickering during startup

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
;; modus-vivendi - High contrast dark theme (built-in since Emacs 28)
;; Part of the modus-themes package by Protesilaos Stavrou
(load-theme 'modus-vivendi t)

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
  (exec-path-from-shell-copy-envs '("GOPATH" "GOROOT" "PYENV_ROOT" "VOLTA_HOME"))
  ;; Add Volta bin directory to exec-path for npm global packages
  (when-let ((volta-home (getenv "VOLTA_HOME")))
    (add-to-list 'exec-path (expand-file-name "bin" volta-home)))
  ;; Add pyenv shims to exec-path for pip global packages
  (let ((pyenv-root (or (getenv "PYENV_ROOT")
                        (expand-file-name "~/.pyenv"))))
    (when (file-directory-p pyenv-root)
      (add-to-list 'exec-path (expand-file-name "shims" pyenv-root)))))

;; ============================================================
;; Eglot - Built-in LSP client (Emacs 29+)
;; ============================================================
(require 'eglot)

;; Don't log every event (improves performance)
(setq eglot-events-buffer-size 0)

;; Shutdown server when last managed buffer is killed
(setq eglot-autoshutdown t)

;; ============================================================
;; Tree-sitter Configuration
;; ============================================================
;; Tree-sitter provides accurate syntax highlighting via grammar libraries.
;; Install grammars with: M-x treesit-install-language-grammar
;;
;; Grammar sources for common languages:
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        ;; C/C++
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        ;; JavaScript/TypeScript
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        ;; JVM languages
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
        (groovy "https://github.com/murtaza64/tree-sitter-groovy")))

;; Helper function to check if tree-sitter grammar is actually usable
(defun my/treesit-available-p (lang)
  "Check if tree-sitter grammar for LANG is actually usable.
Uses treesit-ready-p which verifies the grammar can be loaded."
  (and (fboundp 'treesit-ready-p)
       (treesit-ready-p lang t)))  ; t = quiet, don't signal error

;; ============================================================
;; Python Configuration
;; ============================================================
;; Uses python-ts-mode if grammar available, otherwise python-mode
;; LSP servers (install one): pip install python-lsp-server
;;                        or: pip install basedpyright
;;                        or: pip install ruff

;; Use tree-sitter mode if grammar is available
(when (my/treesit-available-p 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)

;; Python indentation
(setq python-indent-offset 4)

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

;; Use tree-sitter mode if grammar is available, otherwise use go-mode
(if (my/treesit-available-p 'go)
    (progn
      (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
      (when (my/treesit-available-p 'gomod)
        (add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode)))
      (add-hook 'go-ts-mode-hook
                (lambda ()
                  (eglot-ensure)
                  (setq-local indent-tabs-mode t)
                  (setq-local tab-width 4)
                  (add-hook 'before-save-hook
                            (lambda ()
                              (when (derived-mode-p 'go-ts-mode)
                                (eglot-format-buffer)))
                            nil t))))
  ;; Fallback to go-mode package
  (use-package go-mode
    :hook ((go-mode . eglot-ensure)
           (go-mode . (lambda ()
                        (setq-local indent-tabs-mode t)
                        (setq-local tab-width 4)))
           (go-mode . (lambda ()
                        (add-hook 'before-save-hook
                                  (lambda ()
                                    (when (derived-mode-p 'go-mode)
                                      (eglot-format-buffer)))
                                  nil t))))))

;; Gopls configuration
(setq-default eglot-workspace-configuration
              '(:gopls (:staticcheck t :usePlaceholders t)))

;; ============================================================
;; C/C++ Configuration
;; ============================================================
;; Uses c-ts-mode and c++-ts-mode if grammars available (built-in Emacs 29+)
;; LSP: clangd (usually comes with Xcode or LLVM)
;;   Install: xcode-select --install (macOS)
;;        or: brew install llvm

;; Use tree-sitter modes if grammars are available
(when (my/treesit-available-p 'c)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

(when (my/treesit-available-p 'cpp)
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-ts-mode)))

;; Enable Eglot for C/C++ (clangd)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)

;; ============================================================
;; JavaScript / Node.js Configuration
;; ============================================================
;; Uses js-ts-mode if grammar available (built-in Emacs 29+)
;; Requires: npm install -g typescript-language-server typescript
;; (typescript-language-server also handles JavaScript)

;; Use tree-sitter mode if grammar is available
(when (my/treesit-available-p 'javascript)
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode)))

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)

;; JavaScript indentation
(setq js-indent-level 2)

;; ============================================================
;; TypeScript Configuration
;; ============================================================
;; Uses typescript-ts-mode if grammar available, otherwise typescript-mode
;; Requires: npm install -g typescript-language-server typescript

;; TypeScript indentation
(setq typescript-ts-mode-indent-offset 2)

;; Use tree-sitter mode if grammar available, otherwise typescript-mode package
(if (my/treesit-available-p 'typescript)
    (progn
      (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
      (add-hook 'typescript-ts-mode-hook 'eglot-ensure))
  ;; Fallback to typescript-mode package
  (use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . eglot-ensure)
    :custom
    (typescript-indent-level 2)))

;; TSX - tree-sitter mode if available, otherwise web-mode as fallback
(if (my/treesit-available-p 'tsx)
    (progn
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
      (add-hook 'tsx-ts-mode-hook 'eglot-ensure))
  ;; Fallback to web-mode for TSX
  (use-package web-mode
    :mode "\\.tsx\\'"
    :hook (web-mode . eglot-ensure)
    :custom
    (web-mode-markup-indent-offset 2)
    (web-mode-code-indent-offset 2)))

;; ============================================================
;; YAML Configuration
;; ============================================================
;; Optional LSP: npm install -g yaml-language-server

;; Use tree-sitter mode if grammar available, otherwise yaml-mode
(if (my/treesit-available-p 'yaml)
    (progn
      (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.eyaml\\'" . yaml-ts-mode))
      (add-hook 'yaml-ts-mode-hook
                (lambda ()
                  (setq-local indent-tabs-mode nil)
                  (setq-local tab-width 2)
                  (when (executable-find "yaml-language-server")
                    (eglot-ensure)))))
  ;; Fallback to yaml-mode package
  (use-package yaml-mode
    :mode (("\\.ya?ml\\'" . yaml-mode)
           ("\\.eyaml\\'" . yaml-mode))
    :hook ((yaml-mode . (lambda ()
                          (setq-local indent-tabs-mode nil)
                          (setq-local tab-width 2)
                          (when (executable-find "yaml-language-server")
                            (eglot-ensure)))))))

;; ============================================================
;; Java Configuration
;; ============================================================
;; Uses java-ts-mode if grammar available (built-in Emacs 29+)
;; Requires: Eclipse JDTLS for LSP
;;   macOS: brew install jdtls
;;   or download from: https://download.eclipse.org/jdtls/
;;
;; Note: JDTLS needs a workspace directory for each project

;; Use tree-sitter mode if grammar is available
(when (my/treesit-available-p 'java)
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode)))

;; Configure Eglot for Java (JDTLS)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdtls"))))

(add-hook 'java-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'eglot-ensure)

;; Java indentation
(add-hook 'java-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4)
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)))
(add-hook 'java-ts-mode-hook
          (lambda ()
            (setq-local c-basic-offset 4)
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)))

;; ============================================================
;; Kotlin Configuration
;; ============================================================
;; Uses kotlin-mode package (no built-in kotlin-ts-mode in Emacs yet)
;; Tree-sitter grammar available at: https://github.com/fwcd/tree-sitter-kotlin
;; Requires: kotlin-language-server for LSP
;;   Install: brew install kotlin-language-server
;;        or: https://github.com/fwcd/kotlin-language-server

;; Configure Eglot for Kotlin
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(kotlin-mode . ("kotlin-language-server"))))

;; Use kotlin-mode package
(use-package kotlin-mode
  :ensure t
  :mode (("\\.kt\\'" . kotlin-mode)
         ("\\.kts\\'" . kotlin-mode))
  :hook ((kotlin-mode . eglot-ensure)
         (kotlin-mode . (lambda ()
                          (setq-local tab-width 4)
                          (setq-local indent-tabs-mode nil)))))

;; ============================================================
;; Gradle / Groovy Configuration
;; ============================================================
;; Uses groovy-mode package (no built-in groovy-ts-mode in Emacs yet)
;; Tree-sitter grammar available at: https://github.com/murtaza64/tree-sitter-groovy
;;
;; Gradle build files come in two flavors:
;; - .gradle files: Groovy DSL (use groovy-mode)
;; - .gradle.kts files: Kotlin DSL (use kotlin-mode)
;;
;; Optional LSP: gradle-language-server (experimental)
;;   Install: npm install -g gradle-language-server

;; Groovy mode for .gradle and .groovy files
(use-package groovy-mode
  :ensure t
  :mode (("\\.gradle\\'" . groovy-mode)
         ("\\.groovy\\'" . groovy-mode))
  :config
  (setq groovy-indent-offset 4))

;; For .gradle.kts files, use Kotlin mode (already configured above)
;; The "\\.kts\\'" pattern in kotlin configuration handles build.gradle.kts

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
    (setq markdown-command
          (if (executable-find "mermaid-filter")
              "pandoc --filter mermaid-filter"
            "pandoc")))
  ;; Open live preview window on the right side
  (defun my/markdown-live-preview-window-right (file)
    "Open markdown live preview in a window on the right side."
    (eww-open-file file)
    (let ((buf (if (bound-and-true-p eww-auto-rename-buffer)
                   (cl-loop for b in (buffer-list)
                            when (string-match-p "eww\\*\\'" (buffer-name b))
                            return b)
                 (get-buffer "*eww*"))))
      (pop-to-buffer buf '((display-buffer-in-direction)
                           (direction . right)
                           (window-width . 0.5)))
      buf))
  (setq markdown-live-preview-window-function #'my/markdown-live-preview-window-right)
  :custom
  (markdown-enable-math t)
  (markdown-fontify-code-blocks-natively t)
  ;; Delete exported HTML files after live preview
  (markdown-live-preview-delete-export 'delete-on-export))

;; grip-mode - GitHub-flavored Markdown preview
;; Uses go-grip for local rendering (no GitHub API needed)
;; Requires: go install github.com/chrishrb/go-grip@latest
(use-package grip-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode))
  :custom
  (grip-command 'go-grip)           ; Use go-grip (no GitHub API)
  (grip-preview-use-webkit nil))    ; Use external browser

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
  (add-hook 'vterm-mode-hook #'my/vterm-mode-setup)
  ;; Keybindings to send Escape to terminal (for Claude Code cancel)
  (define-key vterm-mode-map (kbd "C-c C-e") #'vterm-send-escape)
  (define-key vterm-mode-map (kbd "C-c <escape>") #'vterm-send-escape)
  ;; Copy mode keybindings (C-c C-t to enter copy mode, then select and copy)
  ;; Global interprogram-cut-function handles clipboard sync automatically
  (define-key vterm-copy-mode-map (kbd "M-w") #'vterm-copy-mode-done)
  (define-key vterm-copy-mode-map (kbd "C-c C-c") #'vterm-copy-mode-done))

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
  (setq claude-code-terminal-backend 'vterm)   ; Use vterm for best TUI experience
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
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
