;;; init-org.el --- Setup Org-Mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package org
  :defer t
  :init
  (setq org-directory "~/workspace/personal/notes"
        org-default-notes-file (f-expand "Inbox")))

(provide 'init-org)
;;; init-org.el ends here
