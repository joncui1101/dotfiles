;;; init-emacs-settings.el --- Settings emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'f)

;; Sets a custom file so it is not in the bottom of the init.el file.
(setq-default custom-file (f-expand "custom.el" user-emacs-directory))

;; Add $PATH to exec-path
(use-package init-env
  :load-path "lisp"
  :config
  (load-env "env-file"))

;; Turn on visual line mode
(global-visual-line-mode t)

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; See matching pairs of parantheses and other characters
(show-paren-mode 1)

;; Show visible bell
(setq visible-bell t)

(setq-default
 tab-width 4                   ; Set default tab width to 4 spaces
 line-spacing 0
 make-backup-files nil         ; Stop creating backup~ files
 delete-old-versions t         ; Deletes excess backup files silently.
 create-lockfiles nil          ; Disable lockfiles.
 auto-save-default nil         ; Stop creating #autosave# files
 load-prefer-newer t           ; Load the newest version of a file.
 indent-tabs-mode nil          ; Use space instead of tabs for indents.
 sentence-end-double-space nil ; Single space to end a sentence.
 ring-bell-function 'ignore    ; Disable bell ring.
 visible-bell nil              ; Disable visible bell.
 line-number-mode nil          ; Hide line number from mode line.
 dired-use-ls-dired nil        ; Don't use --dired with ls.
 initial-scratch-message ""    ; Start with a blank canvas.
 inbibit-startup-message t     ; Don't show the startup message
 inhibit-startup-screen t      ; Don't show the startup screen
 confirm-kill-emacs 'y-or-n-p  ; y and n instead of yes and no when quitting
 )

(set-face-attribute 'default nil
                    :family "Hasklug Nerd Font"
                    :weight 'normal
                    :width 'normal
                    :height 130)

(use-package ligature
  :load-path "lisp-local"
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

(use-package which-key
  :diminish
  :init
  (which-key-mode))

;; ESC cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Allow y for yes.
(fset 'yes-or-no-p 'y-or-n-p)

(defun jc/reload-emacs-config ()
  (interactive)
  (load-file user-init-file))

(defun jc/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(provide 'init-emacs-settings)
;;; init-emacs-settings.el ends here
