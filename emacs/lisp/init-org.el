;;; init-org.el --- Setup Org-Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun jc/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (setq evil-auto-indent nil))

(use-package org
  :defer t
  :hook (org-mode . jc/org-mode-setup)
  :init
  (setq org-directory "~/workspace/personal/notes"
        org-default-notes-file (f-expand "Inbox")))

(provide 'init-org)
;;; init-org.el ends here
