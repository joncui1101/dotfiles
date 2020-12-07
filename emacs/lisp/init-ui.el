;;; init-ui.el --- Setup theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 120))))
  (mode-line-inactive ((t (:height 120))))
  :custom
  (doom-modeline-lsp t))

(provide 'init-ui)
;;; init-ui.el ends here
