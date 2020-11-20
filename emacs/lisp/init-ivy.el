;;; init-ivy.el --- Setup helm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ivy
  :delight
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
  :config
  (ivy-mode +1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) "))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-ibuffer)
         ("C-M-l" . counsel-imenu)
         ([remap apropos-command] . counsel-apropos)
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-symbol] . counsel-describe-symbol)
         ([remap describe-function] . counsel-describe-function)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :requires projectile
  :demand
  :config
  (counsel-projectile-mode 1))

(provide 'init-ivy)
;;; init-ivy.el ends here
