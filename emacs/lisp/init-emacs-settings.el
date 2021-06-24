;;; init-emacs-settings.el --- Settings emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'f)

;; Sets a custom file so it is not in the bottom of the init.el file.
(setq-default custom-file (f-expand "custom.el" user-emacs-directory))
(load custom-file t)

(defvar transient-directory "~/.config/cache/emacs/")


;; Add $PATH to exec-path
(use-package init-env
  :load-path "lisp"
  :config
  (load-env "env-file")
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (message "Native comp is available")
    (message "Native comp is *not* available")))

;; Turn on visual line mode
(global-visual-line-mode t)

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; See matching pairs of parantheses and other characters
(show-paren-mode 1)

;; Show column numbers
(column-number-mode)

(setq backup-directory-alist `(("." . ,(f-expand "backups" transient-directory)))
      url-configuration-directory (f-expand "url" transient-directory)
      auto-save-list-file-prefix (f-expand "auto-save-list/.saves-" transient-directory)
      package-quickstart-file (f-expand "package-quickstart.el" transient-directory))

(setq-default
 tab-width 4                   ; Set default tab width to 4 spaces
 line-spacing 0
 delete-old-versions t         ; Deletes excess backup files silently.
 create-lockfiles nil          ; Disable lockfiles.
 load-prefer-newer t           ; Load the newest version of a file.
 backup-by-copying t           ; Use copying to create backup files.
 indent-tabs-mode nil          ; Use space instead of tabs for indents.
 backward-delete-char-untabify-method 'hungry ; Make backspace erase the tab instead of 1 space at a time
 sentence-end-double-space nil ; Single space to end a sentence.
 ring-bell-function 'ignore    ; Disable bell ring.
 visible-bell t                ; Enable visible bell.
 line-number-mode nil          ; Hide line number from mode line.
 dired-use-ls-dired nil        ; Don't use --dired with ls.
 initial-scratch-message nil   ; Start with a blank canvas.
 inbibit-startup-message t     ; Don't show the startup message
 inhibit-startup-screen t      ; Don't show the startup screen
 confirm-kill-emacs 'y-or-n-p  ; y and n instead of yes and no when quitting
 browse-url-handlers '(("www.youtube.com" . jc/browse-url-vlc)
                       ("." . jc/browse-url-firefox))
 org-directory "~/workspace/personal/notes")

(set-face-attribute 'default nil
                    :family "Hasklug Nerd Font"
                    :weight 'normal
                    :width 'normal
                    :height 130)

(use-package ligature
  :disabled
  :defer 1
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
  :defer 1
  :diminish
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(use-package savehist
  :ensure nil
  :custom
  (savehist-file "~/.config/cache/emacs/savehist")
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :hook (after-init . savehist-mode))

;; ESC cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Allow y for yes.
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-emacs-settings)
;;; init-emacs-settings.el ends here
