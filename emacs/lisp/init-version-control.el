;;; init-version-control.el --- Version Control Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defer 0.3)

(use-package git-gutter
  :defer 0.3
  :config (global-git-gutter-mode +1))

(provide 'init-version-control)
;;; init-version-control.el ends here
