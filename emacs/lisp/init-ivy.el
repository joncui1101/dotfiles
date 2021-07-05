;;; init-ivy.el --- Setup ivy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun jc/search-specific-glob (glob)
  "Search in the project files given the GLOB (specific file type)."
  (interactive "sGlob?: ")
  (counsel-projectile-rg (concat "--glob " glob)))

(use-package ivy
  :defer 1
  :diminish
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

(use-package counsel
  :requires ivy
  :diminish
  :init
  (counsel-mode +1)
  :bind (("C-x b" . counsel-ibuffer)
         ("M-c" . quick-calc)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :requires (counsel projectile)
  :init
  (counsel-projectile-mode 1)
  :bind (("C-c p s f" . jc/search-specific-glob))
  :custom
  (counsel-projectile-find-file-matcher 'ivy--re-filter))

(provide 'init-ivy)
;;; init-ivy.el ends here
