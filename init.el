(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
    helm
    helm-gtags
    helm-projectile
    ;; helm-swoop  ; removed - no longer available on MELPA
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

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)

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

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: projejctile
(require 'projectile)
(projectile-mode +1)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; Package helm-gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)


;; Package: speedbar
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

;; Theme and Colors
;; VS Code Dark+ theme - similar to Visual Studio Code's default dark theme
(when (require 'vscode-dark-plus-theme nil t)
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

;; migemo
;; migemo.el provides Japanese increment search with 'Romanization of Japanese'(Roma-character).
;; Requires: brew install cmigemo
(when (executable-find "cmigemo")
  (require 'migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  ;; Set your installed path (Homebrew on Apple Silicon)
  (setq migemo-dictionary "/opt/homebrew/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
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

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; ============================================================
;; exec-path-from-shell - Better PATH handling on macOS
;; ============================================================
;; This ensures Emacs inherits PATH from your shell (pyenv, volta, ~/bin, etc.)
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
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

(when (require 'go-mode nil t)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook 'eglot-ensure)

  ;; Go uses tabs for indentation
  (add-hook 'go-mode-hook
            (lambda ()
              (setq indent-tabs-mode t)
              (setq tab-width 4)))

  ;; Format and organize imports on save
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (when (derived-mode-p 'go-mode 'go-ts-mode)
                            (eglot-format-buffer)))
                        nil t)))

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

(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.eyaml\\'" . yaml-mode))

  ;; Enable Eglot for YAML if yaml-language-server is installed
  (add-hook 'yaml-mode-hook
            (lambda ()
              (when (executable-find "yaml-language-server")
                (eglot-ensure))))

  ;; YAML indentation
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 2))))

;; ============================================================
;; Markdown Configuration
;; ============================================================
;; Optional: brew install pandoc (for preview/export)

(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

  ;; Use pandoc for markdown processing if available
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc"))

  ;; Enable math support (e.g., for LaTeX equations)
  (setq markdown-enable-math t)

  ;; Fontify code blocks
  (setq markdown-fontify-code-blocks-natively t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
