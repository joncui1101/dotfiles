;;; init-code.el --- Setup language server protocol -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook (((go-mode yaml-mode dockerfile-mode sh-mode python-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-enable-file-watchers nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-keymap-prefix "C-l")
  (lsp-lens-enable nil)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t))

(use-package python-mode
  :delight '(:eval (format " py[%s]" (pyenv-mode-version)))
  :hook (python-mode . (lambda () (modify-syntax-entry ?_ "w" python-mode-syntax-table))))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :bind (:map yaml-mode-map
              ("\C-m" . newline-and-indent)))

(use-package dockerfile-mode
  :mode "Dockerfile\\(?:\\.?.*\\)?\\'")

(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
         ("\\^.z.*\\'" . sh-mode)))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))

(use-package pyenv-mode
  :custom
  (pyenv-mode-mode-line-format nil)
  :preface
  (defun jc/projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((pyenv-version-path (f-expand ".python-version" (projectile-project-root))))
      (if (f-exists? pyenv-version-path)
          (pyenv-mode-set (car (s-lines (s-trim (f-read-text pyenv-version-path)))))
        (pyenv-mode-unset))))
  :hook
  (python-mode . pyenv-mode)
  (projectile-after-switch-project . jc/projectile-pyenv-mode-set))

(use-package display-line-numbers
  :preface
  (defun jc/line-numbers ()
    (display-line-numbers-mode 1)
    (setq display-line-numbers 'relative))
  :hook (((prog-mode text-mode) . jc/line-numbers)))

(use-package hl-line
  :ensure nil
  :hook ((prog-mode text-mode dired-mode) . hl-line-mode))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'bitmap)
  :hook (((python-mode yaml-mode go-mode) . highlight-indent-guides-mode)))

(provide 'init-code)
;;; init-code.el ends here
