;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; 'package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil
      package-quickstart t)

;; Adjust garbage collection thresholds during startup, and thereafter
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

(tool-bar-mode -1)   ;; Remove toolbar
(scroll-bar-mode -1) ;; Remove scrollbar
(menu-bar-mode -1)   ;; Remove menubar

;; Fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
