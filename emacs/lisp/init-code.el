;;; init-code.el --- Setup language server protocol -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package lsp-mode)

(use-package smartparens
  :delight
  :config
  (require 'smartparens-config)
  :hook ((python-mode emacs-lisp-mode yaml-mode) . smartparens-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :bind (:map yaml-mode-map
              ("\C-m" . newline-and-indent)))

(provide 'init-code)
;;; init-code.el ends here
