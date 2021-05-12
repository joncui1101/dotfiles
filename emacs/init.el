;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap config

(require 'init-packages) ;; Calls (package-initialize)
(require 'init-functions)
(require 'init-user-settings)
(require 'init-emacs-settings)
(require 'init-ui)

;; Load configs for specific features and modes

(require 'init-dired)
(require 'init-ledger)
(require 'init-evil)
(require 'init-version-control)
(require 'init-code)
(require 'init-flycheck)
(require 'init-go)
(require 'init-javascript)
(require 'init-python)
(require 'init-company)
(require 'init-projectile)
(require 'init-markdown)
(require 'init-ivy)
;; (require 'init-helm)
(require 'init-elfeed)
(require 'init-treemacs)
(require 'init-vterm)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
