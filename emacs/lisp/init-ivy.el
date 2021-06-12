;;; init-ivy.el --- Setup ivy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ivy
  :defer 1
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
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
  :init (ivy-rich-mode +1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-posframe
  :hook ivy-mode
  :init
  (setq ivy-posframe-display-functions-alist
        '((swiper . ivy-posframe-display-at-window-center)
          (counsel-projectile-switch-project . ivy-posframe-display-at-window-center)
          (counsel-projectile-find-file . ivy-posframe-display-at-window-center)
          (counsel-M-x . ivy-posframe-display-at-window-center)
          (counsel-projectile . ivy-posframe-display-at-window-center)
          (counsel-projectile-switch-to-buffer . ivy-posframe-display-at-window-center)))
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
  :custom
  (counsel-projectile-find-file-matcher 'ivy--re-filter))

(provide 'init-ivy)
;;; init-ivy.el ends here
