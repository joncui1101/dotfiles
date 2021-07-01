;;; init-version-control.el --- Version Control Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defer 1
  :config
  (setq magit-branch-read-upstream-first 'fallback)
  (if (boundp 'jc/magit-repository-directories)
      (setq magit-repository-directories jc/magit-repository-directories)
    (setq magit-repository-directories '(("~/workspace" . 2)))))

(use-package git-gutter
  :defer 1
  :diminish
  :config (global-git-gutter-mode +1)
  :custom
  (git-gutter:update-interval 2))

(provide 'init-version-control)
;;; init-version-control.el ends here
