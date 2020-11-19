;;; init-ivy.el --- Setup helm -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ivy
  :delight
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         ("C-k" . ivy-previous-line))
  :config
  (ivy-mode +1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) "))

(use-package counsel
  :bind (:map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(provide 'init-ivy)
;;; init-ivy.el ends here
