;;; init-emacs-settings.el --- Settings emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Sets a custom file so it is not in the bottom of the init.el file.
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Sets directory to store backup files.
(setq backup-directory (expand-file-name "backups" user-emacs-directory))
(setq-default backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Deletes excess backup files silently.
(setq delete-old-versions t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Start with a blank canvas.
(setq initial-scratch-message "")

;; Load the newest version of a file.
(setq load-prefer-newer t)

;; Single space to end a sentence
(setq-default sentence-end-double-space nil)

;; Never insert tabs
(setq-default indent-tabs-mode nil)

;; Disables the bell ring.
(setq ring-bell-function 'ignore)

;; Allow y for yes.
(fset 'yes-or-no-p 'y-or-n-p)

(use-package exec-path-from-shell
  :defer 1
  :config (exec-path-from-shell-initialize))

(defun jc/reload-emacs-config ()
  (interactive)
  (load-file user-init-file))

(provide 'init-emacs-settings)
;;; init-emacs-settings.el ends here
