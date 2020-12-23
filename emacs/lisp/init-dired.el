;;; init-dired.el --- Setup dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-single
  :defer 1)

(use-package dired-collapse
  :defer 1)

(provide 'init-dired)
;;; init-dired.el ends here
