;;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-
;; ~/elisp をライブラリパスに追加
(setq load-path
      (append
       (list
	(expand-file-name "~/.emacs.d/elisp/")
	)
       load-path))

(setq exec-path (cons "/usr/local/bin" exec-path))
(setenv "PATH"
    (concat '"/usr/local/bin:" (getenv "PATH")))

;;　起動ディレクトリをホームディレクトリに変更する(for Mervelicks)
(setq default-directory "~/") 
(setq command-line-default-directory "~/")

;; Emacs package system
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Mavericks用デフォルトディレクトリを"~/"にする
;;(setq inhibit-splash-screen t)
(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

;; 日本語設定 (UTF-8)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq slime-net-coding-system 'utf-8-unix)

;; フォントロックモード (強調表示等) を有効にする
(global-font-lock-mode t)

;; 一時マークモードの自動有効化
(setq-default transient-mark-mode t)

;; C-x C-u が何もしないように変更する (undo の typo 時誤動作防止)
(global-unset-key "\C-x\C-u")

;; 括弧の対応をハイライト.
(show-paren-mode 1) 

;; バッファ末尾に余計な改行コードを防ぐための設定
(setq next-line-add-newlines nil) 

;; C-x l で goto-line を実行
(define-key ctl-x-map "l" 'goto-line) 

;; 時間を表示
(display-time) 

;; 列数表示
(column-number-mode 1) 

;; メニューバーを消す
(menu-bar-mode -1)

;; C-h でカーソルの左にある文字を消す
(define-key global-map "\C-h" 'delete-backward-char)

;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(define-key global-map "\C-x\C-h" 'help-command)

;; C-o に動的略語展開機能を割り当てる
(define-key global-map "\C-o" 'dabbrev-expand)
(setq dabbrev-case-fold-search nil) ; 大文字小文字を区別

;; clojure
(setq mac-option-modifier 'meta)

;;; バックアップファイルを作らない                                        
(setq backup-inhibited t)

;;; 終了時にオートセーブファイルを消す                                    
(setq delete-auto-save-files t)

;; 日本語・英語混じり文での区切判定
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

;; BS で選択範囲を消す
(delete-selection-mode 1)

;; The local variables list in .emacs と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax) 

;; リセットされた場合に UTF-8 に戻す
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
  '((font . "fontset-12") ;; デフォルトフォントセット
  (width . 140) (height . 50) ;; ウィンドウサイズ
  )
  default-frame-alist))

;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/") ;Emacs Lispをインストールするディレクトリの指定

;; Pacakge Installer
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; install-elisp のコマンドを使える様にします。
(require 'install-elisp)
;; 次にElisp ファイルをインストールする場所を指定します。
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;; anything
(require 'anything-startup)
(global-set-key (kbd "C-x b") 'anything)

;; migemo
;; migemo.el provides Japanese increment search with 'Romanization of Japanese'(ローマ字).
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

;; For go configuration
;; go-mode
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)


;; For php configuration
;; php-mode
(require 'php-mode)
(setq php-mode-force-pear t) ;PEAR規約のインデント設定にする
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode)) ;*.phpのファイルのときにphp-modeを自動起動する
;; php-mode-hook
(add-hook 'php-mode-hook
          (lambda ()
            (require 'php-completion)
            (php-completion-mode t)
            (define-key php-mode-map (kbd "C-o") 'phpcmp-complete) ;php-completionの補完実行キーバインドの設定
            (make-local-variable 'ac-sources)
            (setq ac-sources '(
                               ac-source-words-in-same-mode-buffers
                               ac-source-php-completion
                               ac-source-filename
                               ))))


;;;; For Javascript configuration
;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
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
