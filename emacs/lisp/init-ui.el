;;; init-ui.el --- Setup theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
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

(use-package smooth-scrolling
  :defer 1
  :config (smooth-scrolling-mode 1))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1) ;; keyboard scroll one line at a time

(provide 'init-ui)
;;; init-ui.el ends here
