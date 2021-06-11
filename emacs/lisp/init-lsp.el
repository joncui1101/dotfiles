;;; init-lsp.el --- Setup language server protocol -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook (((go-mode yaml-mode dockerfile-mode sh-mode python-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-diagnostics-provider :none)
  (lsp-enable-file-watchers nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-keymap-prefix "C-l")
  (lsp-lens-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-session-file (f-expand "lsp-session-v1" transient-directory))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t))

(provide 'init-lsp)
;;; init-lsp.el ends here
