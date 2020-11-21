;;; init-code.el --- Setup language server protocol -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package lsp-mode
  :hook (((go-mode yaml-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-l")
  (lsp-pyls-server-command "pyright")
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :requires lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-lens-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package python-mode
  :hook (python-mode . (lambda () (modify-syntax-entry ?_ "w" python-mode-syntax-table))))

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.config/pyenv/versions")
  :preface
  (defun jc/projectile-pyenv-mode-set ()
    (let ((pyenv-version-path (f-expand ".python-version" (projectile-project-root))))
      (if (f-exists? pyenv-version-path)
          (pyvenv-workon (car (s-lines (s-trim (f-read-text pyenv-version-path)))))
        (pyvenv-deactivate))))
  :hook
  (python-mode . pyvenv-mode)
  (projectile-after-switch-project . jc/projectile-pyenv-mode-set))

(use-package go-mode
  :mode "\\.go\\'")

(use-package smartparens
  :delight
  :config
  (require 'smartparens-config)
  :hook ((emacs-lisp-mode go-mode lsp-mode python-mode yaml-mode) . smartparens-strict-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :bind (:map yaml-mode-map
              ("\C-m" . newline-and-indent)))

(provide 'init-code)
;;; init-code.el ends here
