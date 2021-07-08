(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native comp is available")
  (message "Native comp is *not* available"))

(setq-default user-full-name "Jonathan Cui"
              user-mail-address "joncui1101@gmail.com")

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(unless package--initialized
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(unless (package-installed-p 'f)
  (package-refresh-contents)
  (package-install 'f))

(require 'f)

(setq-default custom-file (f-expand "custom.el" user-emacs-directory))
(load custom-file t)

(defun jc/reload-emacs-config ()
  "Reload Emacs config."
  (interactive)
  (load-file user-init-file))

(defun jc/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(if (f-exists-p "~/workspace/work/dotfiles/emacs/init-work.el")
    (progn
      (add-to-list 'load-path (f-expand "~/workspace/work/dotfiles/emacs"))
      (require 'init-work)))

(defvar transient-directory "~/.config/cache/emacs/")

(setq backup-directory-alist `(("." . ,(f-expand "backups" transient-directory)))
      url-configuration-directory (f-expand "url" transient-directory)
      auto-save-list-file-prefix (f-expand "auto-save-list/.saves-" transient-directory)
      package-quickstart-file (f-expand "package-quickstart.el" transient-directory))

(setq-default backup-by-copying t ; Use copying to create backup files.
              create-lockfiles nil ; Disable lockfiles.
              delete-old-versions t ; Deletes excess backup files silently.
              load-prefer-newer t) ; Load newest version of a file.

(defun jc/load-env (file)
  "Load the contents of the FILE as 'exec-path'."
  (let ((file-path (f-expand file user-emacs-directory)))
    (if (f-exists? file-path)
        (let ((paths (s-trim (f-read-text file-path))))
          (setenv "PATH" paths)
          (dolist (path (s-split ":" paths))
            (add-to-list 'exec-path path))))))

(jc/load-env "env-file")

(use-package which-key
  :defer 1
  :diminish
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  :custom
  (evil-symbol-word-search t)
  :config
  (evil-mode 1))

(use-package evil-surround
  :defer 1
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :defer 1
  :diminish evil-snipe-local-mode
  :requires evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  :custom
  (evil-snipe-scope 'buffer)
  (evil-snipe-repeat-scope 'buffer)
  (evil-snipe-spillover-scope 'whole-buffer))

(use-package evil-collection
  :defer 1
  :requires evil
  :config
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t))

;; Turn on visual line mode
(global-visual-line-mode t)

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; See matching pairs of parantheses and other characters
(show-paren-mode 1)

;; Show column numbers
(column-number-mode)

(setq-default confirm-kill-emacs 'y-or-n-p ; y or n instead of yes or no when quitting.
              split-height-threshold 90 ; Increase height threshold from 80 to 90 so split screens will favor horizontal splits
              initial-scratch-message nil ; Start with a blank canvas.
              inhibit-startup-screen t) ; Do not show the startup screen.

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 120))))
  (mode-line-inactive ((t (:height 120))))
  :custom
  (doom-modeline-env-enable-python nil)
  (doom-modeline-lsp t)
  (doom-modeline-modal-icon nil)
  (doom-modeline-vcs-max-length 50))

(use-package delight
  :delight
  (emacs-lisp-mode "elisp"))

(set-face-attribute 'default nil
                    :family "Hasklug Nerd Font"
                    :weight 'normal
                    :width 'normal
                    :height 130)

(use-package ligature
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

(setq-default tab-width 2
              evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(setq-default backward-delete-char-untabify-method 'hungry)

(setq-default sentence-end-double-space nil)

(defun jc/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (setq evil-auto-indent nil))

(use-package org
  :defer t
  :hook (org-mode . jc/org-mode-setup)
  :bind (:map org-mode-map
              ("C-<tab>" . org-indent-block))
  :init
  (setq org-directory "~/workspace/personal/notes"
        org-default-notes-file (f-expand "Inbox")))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defvar jc/init-org-file (f-expand "~/workspace/personal/dotfiles/emacs/init.org"))

(defun jc/tangle-on-save ()
  (when (equal (buffer-file-name)
               (expand-file-name jc/init-org-file))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)
      (message "early-init.el and init.el tangled from init.org"))))

(add-hook 'after-save-hook 'jc/tangle-on-save)

(use-package ivy
  :defer 1
  :bind (:map ivy-minibuffer-map
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-d" . ivy-switch-buffer-kill)
              ("C-k" . ivy-previous-line)
              :map ivy-reverse-i-search-map
              ("C-k" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode +1)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-wrap t))

(use-package counsel
  :requires ivy
  :init
  (counsel-mode +1)
  :bind (("C-x b" . counsel-ibuffer)
         ("M-c" . quick-calc)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :requires ivy
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-M-x
             '(:columns
               ((counsel-M-x-transformer (:width 50))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-switch-buffer-transformer (:width 50))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 0.18 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))))
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
        '((counsel-projectile-switch-project . ivy-posframe-display-at-frame-center)
          (counsel-projectile-find-file . ivy-posframe-display-at-frame-center)
          (counsel-M-x . ivy-posframe-display-at-frame-center)
          (counsel-projectile . ivy-posframe-display-at-frame-center)
          (counsel-projectile-switch-to-buffer . ivy-posframe-display-at-frame-center))
        ivy-posframe-min-width 115
        ivy-posframe-parameters '((left-fringe . 4)
                                  (right-fringe . 4)))
  (ivy-posframe-mode 1))

(use-package lsp-ivy
  :requires (lsp-mode ivy))

(use-package company
  :defer 1
  :config (global-company-mode 1)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1))

(use-package savehist
  :ensure nil
  :custom
  (savehist-file "~/.config/cache/emacs/savehist")
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :hook (after-init . savehist-mode))

(use-package yasnippet
  :hook ((prog-mode text-mode org-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-l" . yas-expand))
  :config
  (yas-reload-all))

(unless (package-installed-p 'yasnippet-snippets)
  (package-refresh-contents)
  (package-install 'yasnippet-snippets))

(defun jc/line-numbers ()
  (display-line-numbers-mode 1)
  (setq display-line-numbers 'relative))

(use-package display-line-numbers
  :hook (((prog-mode text-mode) . jc/line-numbers)))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  :hook ((org-mode prog-mode) . smartparens-mode))

(use-package hl-line
  :ensure nil
  :hook ((prog-mode text-mode dired-mode) . hl-line-mode))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'bitmap)
  :hook (((prog-mode) . highlight-indent-guides-mode)))

(if (executable-find "asdf")
    (setenv "ASDF_DATA_DIR" (f-expand "~/.config/asdf")))

(use-package flycheck
  :defer t
  :preface
  :hook ((ledger-mode emacs-lisp-mode lsp-mode) . flycheck-mode)
  :custom
  (flycheck-flake8-maximum-line-length 120)
  (flycheck-flake8-maximum-complexity 40))

(use-package flycheck-ledger
  :requires flycheck)

(use-package tree-sitter
  :hook (((go-mode python-mode ruby-mode) . tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

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

(use-package projectile
  :defer 1
  :diminish
  :init
  (projectile-mode 1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories ".github")
  :custom
  (projectile-enable-caching t)
  (projectile-require-project-root t)
  (projectile-indexing-method 'native)
  (projectile-project-root-functions '(projectile-root-local projectile-root-bottom-up projectile-root-top-down-recurring))
  (projectile-completion-system 'ivy)
  (projectile-known-projects-file (f-expand "projectile-bookmarks.eld" transient-directory))
  (projectile-cache-file (f-expand "projectile.cache" transient-directory))
  :config
  (if (boundp 'jc/projectile-project-search-path)
      (setq projectile-project-search-path projectile-project-search-path)
    (setq projectile-project-search-path '("~/workspace/personal"))))

(defun jc/search-specific-glob (glob)
  "Search in the project files given the GLOB (specific file type)."
  (interactive "sGlob?: ")
  (counsel-projectile-rg (s-concat "--glob " glob)))

(use-package counsel-projectile
  :requires (counsel projectile)
  :init
  (counsel-projectile-mode 1)
  :bind (("C-c p s f" . jc/search-specific-glob))
  :custom
  (counsel-projectile-find-file-matcher 'ivy--re-filter))

(use-package lsp-mode
  :hook (((go-mode yaml-mode dockerfile-mode sh-mode python-mode ruby-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-diagnostics-provider :none)
  (lsp-enable-file-watchers nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-keymap-prefix "C-l")
  (lsp-lens-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-session-file (f-expand "lsp-session-v1" transient-directory))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t))

(defun jc/lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . jc/lsp-go-install-save-hooks)
  :custom
  (gofmt-command "goimports"))

(defun jc/projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((pyenv-version-path (f-expand ".python-version" (projectile-project-root))))
    (if (f-exists? pyenv-version-path)
        (progn
          (pyenv-mode-set (car (s-lines (s-trim (f-read-text pyenv-version-path)))))
          (setq flycheck-python-flake8-executable (s-concat (pyenv-mode-full-path (pyenv-mode-version)) "/bin/python3")))
      (pyenv-mode-unset))))

(use-package python-mode
  :delight '(:eval (format " py[%s]" (pyenv-mode-version)))
  :hook (python-mode . (lambda () (modify-syntax-entry ?_ "w" python-mode-syntax-table))))

(use-package pyenv-mode
  :custom
  (pyenv-mode-mode-line-format nil)
  :hook
  (python-mode . pyenv-mode)
  (projectile-after-switch-project . jc/projectile-pyenv-mode-set))

(use-package ruby-mode
  :hook (ruby-mode . (lambda () (modify-syntax-entry ?_ "w" ruby-mode-syntax-table))))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package emacs-lisp
  :ensure nil
  :hook (emacs-lisp-mode . electric-indent-mode))

(use-package sh-script
  :mode (("\\.sh\\'" . sh-mode)
         ("\\^.z.*\\'" . sh-mode)
         ("\\^zsh.*\\'" . sh-mode)
         ("\\zprofile\\'" . sh-mode)))

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc"))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :bind (:map yaml-mode-map
              ("\C-m" . newline-and-indent)))

(use-package dockerfile-mode
  :mode "Dockerfile\\(?:\\.?.*\\)?\\'")

(use-package protobuf-mode
  :preface
  (defconst protobuf-style-work
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  :hook (protobuf-mode . (lambda () (c-add-style "work protobuf style" protobuf-style-work t)))
  :mode "\\.proto\\'")

(use-package treemacs
  :defer t
  :custom
  (treemacs-persist-file (f-expand "treemacs-persist" transient-directory))
  :bind (:map global-map
              ("C-x t t" . treemacs)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(setq-default dired-use-ls-dired nil)

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

(defun jc/browse-url-firefox (url &optional _new-window)
  "Open URL in firefox."
  (interactive (browse-url-interactive-arg "URL: "))
  (pcase system-type
    ('darwin (start-process (s-concat "open " url) nil "open" "-a" "firefox" url))
    ('windows-nt (message "Windows not available"))
    ('gnu/linux (start-process (s-concat "firefox " url) nil "firefox" url))))

(defun jc/browse-url-vlc (url &optional _new-window)
  "Open URL in vlc."
  (interactive (browse-url-interactive-arg "URL: "))
  (pcase system-type
    ('darwin (start-process (s-concat "open " url) nil "open" "-a" "vlc" url "--args" "--video-on-top"))
    ('windows-nt (message "Windows not available"))
    ('gnu/linux (start-process (s-concat "vlc " url) nil "vlc" url "--video-on-top"))))

(setq-default browse-url-handlers '(("www.youtube.com" . jc/browse-url-vlc)
                                    ("." . jc/browse-url-firefox)))

(defun jc/ledger-save-buffer ()
  "Automatically clean the ledger buffer at each save."
  (interactive)
  (ledger-mode-clean-buffer)
  (save-buffer))

(defun jc/ledger-evil-write ()
  "Automatically clean the ledger buffer at each save."
  (interactive)
  (ledger-mode-clean-buffer)
  (evil-write nil nil))

(use-package ledger-mode
  :mode "\\.journal\\'"
  :bind ((:map ledger-mode-map
               ("C-x C-s" . jc/ledger-save-buffer))
         ([remap evil-write] . jc/ledger-evil-write))
  :custom
  (ledger-binary-path "hledger")
  (ledger-mode-should-check-version nil)
  (ledger-init-file-name "")
  (ledger-default-date-format "%m/%d")
  (ledger-post-amount-alignment-at :decimal)
  (ledger-highlight-xact-under-point nil))

(use-package evil-ledger
  :requires (evil ledger-mode)
  :hook (ledger-mode . evil-ledger-mode)
  :custom
  (evil-ledger-sort-key "S"))

(use-package company-ledger
  :requires company
  :init
  (add-to-list 'company-backends 'company-ledger))

(use-package elfeed
  :preface
  (defun jc/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed)
    (elfeed-db-load)
    (elfeed-search-update--force)
    (elfeed-update))
  (defun jc/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (kill-buffer (current-buffer)))
  (defun jc/elfeed-evil-collection-remap (_mode _mode-keymaps &rest _rest)
    (evil-collection-define-key 'normal 'elfeed-search-mode-map
      (kbd "RET") 'elfeed-search-browse-url
      (kbd "S-<return>") 'elfeed-search-show-entry
      "q" 'jc/elfeed-save-db-and-bury
      "Q" 'jc/elfeed-save-db-and-bury))
  :hook (evil-collection-setup . jc/elfeed-evil-collection-remap)
  :bind (("C-x w" . jc/elfeed-load-db-and-open))
  :custom
  (elfeed-db-directory "~/.config/cache/emacs/elfeed")
  (elfeed-feeds
   '(("https://techcrunch.com/rss" tech)
     ("https://lifehacker.com/rss" tech)
     ("https://gizmodo.com/rss" tech)
     ("http://rss.desiringgod.org/" christian)
     ("http://amiquiettimes.wordpress.com/feed/" christian)
     ("http://feeds.feedburner.com/HighScalability" dev)
     ("http://feeds.dzone.com/java" dev java)
     ("http://feeds.dzone.com/performance" dev)
     ("https://medium.com/feed/paypal-engineering" dev)
     ("https://martinfowler.com/feed.atom" dev)
     ("https://www.8bitmen.com/feed/" dev)
     ("https://netflixtechblog.com/feed" dev)
     ("http://www.daemonology.net/hn-daily/index.rss" dev)
     ("https://blog.codinghorror.com/rss/" dev)
     ("http://www.allthingsdistributed.com/atom.xml" dev)
     ("https://dave.cheney.net/feed/atom" dev go)
     ("https://eng.uber.com/feed/" dev)
     ("https://codeascraft.com/atom" dev)
     ("https://instagram-engineering.com/feed" dev)
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("https://github.blog/feed" dev)
     ("https://blog.golang.org/feeds/posts/default" dev go)
     ("https://developer.squareup.com/blog/rss.xml" dev)
     ("https://stripe.com/blog/feed.rss" dev)
     ("https://smittenkitchen.com/atom" cooking)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" emacs vid) ;; system crafters
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC6x7GwJxuoABSosgVXDYtTw" maker vid) ;; i like to make stuff
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKwM-7sO1_Tw9EmYhKfpBBw" maker vid) ;; perkins builder brothers
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCex87CEaoqw365i63aKxepQ" life vid) ;; ice1cube
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUT8RoNBTJvwW1iErP6-b-A" amongus vid) ;; disguised toast
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBh2UCIk9In7uf87GJU6qgg" civ vid) ;; the game mechanic
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqqJQ_cXSat0KIAVfIfKkVA" cooking vid) ;; kenji
     ("https://vimeo.com/andrewrk/videos/rss" dev vid)
     ("https://www.gobeyond.dev/rss" dev go)
     ("https://cprss.s3.amazonaws.com/golangweekly.com.xml" dev go)
     ("https://sec.report/CIK/0001811210.rss" spac) ;; cciv
     ("https://sec.report/CIK/0001807846.rss" spac) ;; fuse
     ("https://sec.report/CIK/0001809987.rss" spac) ;; gsah
     ("https://sec.report/CIK/0001794717.rss" spac) ;; scvx
     ("https://sec.report/CIK/0001816090.rss" spac) ;; ftoc
     ("https://sec.report/CIK/0001820630.rss" spac) ;; actc
     ("https://sec.report/CIK/0001815086.rss" spac) ;; btwn
     ("https://sec.report/CIK/0001829426.rss" spac) ;; fpac
     ("https://sec.report/CIK/0001818873.rss" spac) ;; ipof
     ("https://sec.report/CIK/0001823575.rss" spac) ;; lnfa
     ("https://sec.report/CIK/0001811882.rss" spac) ;; psth
     ("https://world.hey.com/joaoqalves/feed.atom" dev)
     ))
  (elfeed-search-filter "+unread ")
  (elfeed-search-title-max-width 100))

(use-package vterm
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell "/usr/bin/zsh"))
