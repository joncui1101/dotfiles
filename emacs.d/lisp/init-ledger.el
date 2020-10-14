;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package ledger-mode
  :mode "\\.journal\\'"
  :bind ((:map ledger-mode-map
               ("C-x C-s" . jc/ledger-save))
         ([remap evil-write] . jc/ledger-save)
         ([remap evil-save-and-close] . jc/ledger-save)
         ([remap evil-save-and-quit] . jc/ledger-save))
  :hook (ledger-mode . ledger-flymake-enable)
  :preface
  (defun jc/ledger-save ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (ledger-mode-clean-buffer)
    (save-buffer))
  :custom
  (ledger-binary-path "hledger")
  (ledger-mode-should-check-version nil)
  (ledger-init-file-name "")
  (ledger-default-date-format "%m/%d")
  (ledger-highlight-xact-under-point nil))

(provide 'init-ledger)
;;; init-ledger.el ends here
