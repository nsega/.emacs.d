;; For Go configuration
;; Uses Eglot (configured in init.el) for LSP support via gopls
;; Requires: go install golang.org/x/tools/gopls@latest

(require 'go-mode)

;; Go-specific keybindings (these work alongside Eglot)
(add-hook 'go-mode-hook
          (lambda ()
            ;; Useful Go commands
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c C-g") 'go-goto-imports)
            (local-set-key (kbd "C-c C-k") 'godoc)))

;; NOTE: Formatting and completion are handled by Eglot in init.el
;; NOTE: go-oracle, goflymake, company-go are deprecated in favor of gopls


