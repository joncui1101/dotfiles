;;; init-code.el --- Setup language server protocol -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :hook (((python-mode go-mode yaml-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-l")
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :requires lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-lens-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t))

(use-package go-mode
  :mode "\\.go\\'")

(use-package smartparens
  :delight
  :config
  (require 'smartparens-config)
  :hook ((lsp-mode python-mode emacs-lisp-mode yaml-mode) . smartparens-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :bind (:map yaml-mode-map
              ("\C-m" . newline-and-indent)))

(use-package which-key
  :config
  (which-key-mode))

(provide 'init-code)
;;; init-code.el ends here
