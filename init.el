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
    auto-complete
    company
    ;; duplicate-thing  ; removed - no longer available on MELPA
    ggtags
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

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)

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

;; function-args
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

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
;; Speedbar
(setq speedbar-show-unknown-files t)

;;　Changing the home directory as the initial dir(for Mervelicks)
(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; Changing the default directory as of '〜/' for Mavericks
(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

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

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start t)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

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
  :commands vterm)

;; eat - Emulate A Terminal (pure elisp, no external dependencies)
(use-package eat
  :commands eat)

;; ============================================================
;; Claude Code Integration
;; ============================================================
;; Requires: brew install --cask claude-code (already installed)
;;           brew install libvterm (already installed)

;; Install claude-code.el from GitHub using package-vc-install (Emacs 29+)
;; This installs to ~/.emacs.d/elpa/ (should be in .gitignore)
(unless (package-installed-p 'claude-code)
  (package-vc-install "https://github.com/stevemolitor/claude-code.el"))

;; Load and configure claude-code
(with-eval-after-load 'claude-code
  ;; Use vterm for the best TUI experience
  (setq claude-code-terminal-type 'vterm)

  ;; Set the correct command name (homebrew installs as 'claude')
  (setq claude-code-command "claude")

  ;; Auto-save buffers before sending to Claude
  (setq claude-code-save-before-send t)

  ;; Keybindings
  (global-set-key (kbd "C-c c c") 'claude-code)        ; Start/switch to Claude
  (global-set-key (kbd "C-c c s") 'claude-code-send)   ; Send region/buffer
  (global-set-key (kbd "C-c c v") 'claude-code-vterm)) ; Open raw vterm buffer

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(anzu auto-complete claude-code clean-aindent-mode comment-dwim-2
          company consult dtrt-indent dumb-jump eat embark embark-consult
          exec-path-from-shell ggtags go-mode iedit marginalia
          markdown-mode migemo orderless projectile smartparens undo-tree
          vertico volatile-highlights vscode-dark-plus-theme vterm ws-butler
          yaml-mode yasnippet zygospore))
 '(package-vc-selected-packages
   '((claude-code :vc-backend Git :url
                  "https://github.com/stevemolitor/claude-code.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
