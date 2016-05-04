;; For go configuration
;; go-mode
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Go Code auto completion
(require 'go-autocomplete) 

;; go-remove-unused-imports
(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

;; goto-imports
(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-g") 'go-goto-imports)))

;; gofmt
(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-f") 'gofmt)))
(add-hook 'before-save-hook 'gofmt-before-save)

;; go-doc
(add-hook 'go-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-k") 'godoc)))

;; go-oracle-mode
(load "~/src/golang.org/x/tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)

;; go-flymake
(add-to-list 'load-path "~/src/github.com/dougm/goflymake")
(require 'go-flymake)

;; company-go
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))


