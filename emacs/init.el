;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 2 1000 1000))
      (init-gc-cons-threshold (* 5 1000 1000)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config

(require 'init-packages) ;; Calls (package-initialize)
(require 'init-user-settings)
(require 'init-emacs-settings)
(require 'init-theme)

;; Load configs for specific features and modes

(require 'init-ledger)
(require 'init-evil)
(require 'init-version-control)
(require 'init-code)
(require 'init-company)
(require 'init-projectile)
(require 'init-helm)

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
