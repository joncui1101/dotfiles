(setq package-quickstart t)
(setq frame-inhibit-implied-resize t)

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

;; Remove tool bar
(tool-bar-mode -1)

;; Remove scroll bar
(scroll-bar-mode -1)

;; Remove menu bar
(menu-bar-mode -1)

;; Fullscreen on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
