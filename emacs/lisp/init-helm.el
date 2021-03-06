;;; init-helm.el --- Setup helm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package helm
  :diminish
  :config
  (require 'helm-config)
  (global-unset-key (kbd "C-x c"))
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-mini)
         ("C-x b" . helm-mini)
         ("C-s" . helm-occur)
         ("C-h a" . helm-apropos)
         ("M-y" . helm-show-kill-ring)
         ("M-c" . helm-calcul-expression)
         ("C-c h" . helm-command-prefix)
         ("C-x r b" . helm-bookmarks)
         :map evil-ex-map
         ("b" . helm-mini)
         :map helm-map
         ("C-j" . helm-next-line)
         ("C-k" . helm-previous-line)
         ("C-h" . helm-previous-source)
         ("C-l" . helm-next-source)
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-find-files-map
         ("C-h" . helm-find-files-up-one-level)
         ("C-l" . helm-find-files-down-last-level))
  :custom
  (helm-apropos-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-M-x-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-move-to-line-cycle-in-source t)
  (helm-window-prefer-horizontal-split t)
  (helm-split-window-inside-p t))

(use-package helm-projectile
  :requires (projectile helm)
  :config
  (helm-projectile-on))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode)
  :bind ("C-h b" . helm-descbinds))

(use-package helm-rg)

(provide 'init-helm)
;;; init-helm.el ends here
