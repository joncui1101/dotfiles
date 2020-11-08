;;; init-version-control.el --- Version Control Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defer 0.3
  :custom
  (magit-repository-directories '(("\~/workspace" . 1) ("\~/pws" . 1))))

(use-package git-gutter
  :defer 0.3
  :delight
  :config (global-git-gutter-mode +1)
  :custom
  (git-gutter:update-interval 2))

(provide 'init-version-control)
;;; init-version-control.el ends here
