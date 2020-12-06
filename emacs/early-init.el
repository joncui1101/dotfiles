;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 2 1000 1000))
      (init-gc-cons-threshold (* 5 1000 1000)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(tool-bar-mode -1)   ;; Remove toolbar
(scroll-bar-mode -1) ;; Remove scrollbar
(menu-bar-mode -1)   ;; Remove menubar

;; Fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
