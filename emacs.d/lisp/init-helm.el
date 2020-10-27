;;; init-helm.el --- Setup helm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-mini)
         ("C-x b" . helm-mini)
         ("C-s" . helm-occur)
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         ("C-h a" . helm-apropos)
         ("M-y" . helm-show-kill-ring)
         ("M-c" . helm-calcul-expression)
         :map evil-ex-map
         ("b" . helm-mini))
  :custom
  (helm-apropos-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-M-x-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-move-to-line-cycle-in-source t)
  (helm-split-window-inside-p t))

(provide 'init-helm)
;;; init-helm.el ends here
