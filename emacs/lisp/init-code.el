;;; init-code.el --- Setup language server protocol -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :bind (:map yaml-mode-map
              ("\C-m" . newline-and-indent)))

(use-package dockerfile-mode
  :mode "Dockerfile\\(?:\\.?.*\\)?\\'")

(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
         ("\\^.z.*\\'" . sh-mode)
         ("\\^zsh.*\\'" . sh-mode)
         ("\\zprofile\\'" . sh-mode)))

(use-package protobuf-mode
  :preface
  (defconst protobuf-style-work
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  :hook (protobuf-mode . (lambda () (c-add-style "work protobuf style" protobuf-style-work t)))
  :mode "\\.proto\\'")

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  :hook ((org-mode prog-mode) . smartparens-mode))

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
  :hook (((prog-mode) . highlight-indent-guides-mode)))

(use-package emacs-lisp
  :ensure nil
  :hook (emacs-lisp-mode . electric-indent-mode))

(require 'f)
(setenv "ASDF_DATA_DIR" (f-expand "~/.config/asdf"))

(provide 'init-code)
;;; init-code.el ends here
