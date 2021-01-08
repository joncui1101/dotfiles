;;; init-dired.el --- Setup dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ([remap dired-find-file] . dired-single-buffer)
         ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
         ([remap dired-up-directory] . dired-single-up-directory)))

(use-package dired-single
  :requires dired)

(use-package dired-collapse
  :defer 1)

(provide 'init-dired)
;;; init-dired.el ends here
