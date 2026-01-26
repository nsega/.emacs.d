;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Author: Naoki Sega
;; Maintainer: Naoki Sega

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
