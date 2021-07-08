;;; init-dired.el --- Setup dired -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-load . dired-collapse-mode)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ([remap dired-find-file] . dired-single-buffer)
         ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
         ([remap dired-up-directory] . dired-single-up-directory)))

(use-package dired-single)

(use-package dired-collapse)

(provide 'init-dired)
;;; init-dired.el ends here
