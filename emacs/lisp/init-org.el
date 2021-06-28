;;; init-org.el --- Setup Org-Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun jc/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (electric-indent-mode -1)
  (setq evil-auto-indent nil))

(use-package org
  :defer t
  :hook (org-mode . jc/org-mode-setup)
  :bind (:map org-mode-map
              ("C-<tab>" . org-indent-block))
  :init
  (setq org-directory "~/workspace/personal/notes"
        org-default-notes-file (f-expand "Inbox")))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'init-org)
;;; init-org.el ends here
