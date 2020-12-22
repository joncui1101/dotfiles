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
  :defer 0.2)

(use-package dired-collapse
  :defer 0.2)

(provide 'init-dired)
;;; init-dired.el ends here
