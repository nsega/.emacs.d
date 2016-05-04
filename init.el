(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(setq exec-path (cons "/usr/local/bin" exec-path))
(setenv "PATH"
    (concat '"/usr/local/bin:" (getenv "PATH")))

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
(setq default-buffer-file-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

;; key bindings
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

(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-x\C-h" 'help-command)
(define-key global-map "\C-o" 'dabbrev-expand)
(setq dabbrev-case-fold-search nil)

;;; Don't create the backup file
(setq backup-inhibited t)

;;; Deleting the save files when it was exited.                     
(setq delete-auto-save-files t)

;; Configuration for Japanese and English
;; http://www.alles.or.jp/~torutk/oojava/meadow/Meadow210Install.html
(defadvice dabbrev-expand
  (around modify-regexp-for-japanese activate compile)
  "Modify `dabbrev-abbrev-char-regexp' dynamically for Japanese words."
  (if (bobp)
      ad-do-it
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
      ad-do-it)))

;; Configuration Backspace
(delete-selection-mode 1)

;; The local variables list in .emacs と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax) 

;; http://0xcc.net/blog/archives/000041.html
(set-default-coding-systems 'utf-8)

;;Color
(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "LightGray")
   (set-cursor-color "Gray")
   (set-frame-parameter nil 'alpha 100)
   ))

(setq default-frame-alist
  (append
  '((font . "fontset-13") ;; Default Fontset
  (width . 140) (height . 50) ;; Window Size
  )
  default-frame-alist))

;; Pacakge Installer
;;(require 'package)
;;(add-to-list 'package-archives
;;	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(when
;;    (load
;;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;;(package-initialize))

;; anything
;;(require 'anything-startup)
;;(global-set-key (kbd "C-x b") 'anything)

;; migemo
;; migemo.el provides Japanese increment search with 'Romanization of Japanese'(Roma-character).
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
;; Set your installed path
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)

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


;; For php configuration
;; php-mode
(require 'php-mode)
(setq php-mode-force-pear t) 
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode)) 
;; php-mode-hook
(add-hook 'php-mode-hook
          (lambda ()
            (require 'php-completion)
            (php-completion-mode t)
            (define-key php-mode-map (kbd "C-o") 'phpcmp-complete) 
            (make-local-variable 'ac-sources)
            (setq ac-sources '(
                               ac-source-words-in-same-mode-buffers
                               ac-source-php-completion
                               ac-source-filename
                               ))))


;;;; For Javascript configuration
;; js2-mode
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;; lintnode
;;(add-to-list 'load-path "~/.emacs.d/lintnode")
;;(require 'flymake-jslint)
;; Make sure we can find the lintnode executable
;;(setq lintnode-location "~/.emacs.d/lintnode")
;; JSLint can be... opinionated
;;(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
;;(add-hook 'js-mode-hook
;;	  (lambda () 
;;	    (lintnode-hook)))
;; flymake-jslint
;;(require 'flymake-jslint)
;;(add-hook 'js-mode-hook 'flymake-jslint-load)
;;(add-hook 'js-mode-hook
;;	  (lambda () (flymake-mode t)))
;; flymake-cursor
;;(require 'flymake-cursor)

;; js-comint (Javascript console)
;;(require 'js-comint)
;; Use node as our repl
;;(setq inferior-js-program-command "node") 
;;(setq inferior-js-mode-hook
;;      (lambda ()
;;        ;; We like nice colors
;;        (ansi-color-for-comint-mode-on)
;;        ;; Deal with some prompt nonsense
;;        (add-to-list 'comint-preoutput-filter-functions
;;                     (lambda (output)
;;                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
;;                       (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))


;;;; For ruby configuration
;; ruby-mode (require ruby-mode.el)
;; (autoload 'ruby-mode "ruby-mode"
;;   "Mode for editing ruby source files" t)
;; (setq auto-mode-alist
;;       (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
;; (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
;;                                      interpreter-mode-alist))
;; (autoload 'run-ruby "inf-ruby"
;;   "Run an inferior Ruby process")
;; (autoload 'inf-ruby-keys "inf-ruby"
;;   "Set local key defs for inf-ruby in ruby-mode")
;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;             (inf-ruby-keys)))
