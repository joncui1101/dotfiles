;;; init-go.el --- Setup go environment -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :preface
  (defun jc/lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :mode "\\.go\\'"
  :hook
  (go-mode . jc/lsp-go-install-save-hooks)
  :custom
  (gofmt-command "goimports"))

(provide 'init-go)
;;; init-go.el ends here
