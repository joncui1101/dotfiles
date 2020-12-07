;;; init-ivy.el --- Setup ivy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ivy
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
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) "))

(use-package ivy-rich
  :init (ivy-rich-mode +1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :diminish
  :init
  (counsel-mode +1)
  :bind (("C-x b" . counsel-ibuffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :requires projectile
  :init
  (counsel-projectile-mode 1)
  :custom
  (counsel-projectile-find-file-matcher 'ivy--re-filter))

(provide 'init-ivy)
;;; init-ivy.el ends here
