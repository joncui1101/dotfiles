;;; init-emacs-settings.el --- Settings emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Sets a custom file so it is not in the bottom of the init.el file.
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Sets directory to store backup files.
(setq backup-directory (expand-file-name "backups" user-emacs-directory))
(setq-default backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Add local bin to exec-path
(add-to-list 'exec-path "/usr/local/bin")

;; Fullscreen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Remove toolbar
(tool-bar-mode -1)

;; Remove scrollbar
(if window-system (scroll-bar-mode -1))

;; Show line numbers
(global-display-line-numbers-mode)

(setq-default
 delete-old-versions t         ; Deletes excess backup files silently.
 create-lockfiles nil          ; Disable lockfiles.
 load-prefer-newer t           ; Load the newest version of a file.
 indent-tabs-mode nil          ; Use space instead of tabs for indents.
 sentence-end-double-space nil ; Single space to end a sentence.
 ring-bell-function 'ignore    ; Disable bell ring.
 visible-bell nil              ; Disable visible bell.
 line-number-mode nil          ; Hide line number from mode line.
 dired-use-ls-dired nil        ; Don't use --dired with ls.
 initial-scratch-message ""    ; Start with a blank canvas.
)

;; Allow y for yes.
(fset 'yes-or-no-p 'y-or-n-p)

(defun jc/reload-emacs-config ()
  (interactive)
  (load-file user-init-file))

(provide 'init-emacs-settings)
;;; init-emacs-settings.el ends here
