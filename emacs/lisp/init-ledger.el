;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ledger-mode
  :mode "\\.journal\\'"
  :bind ((:map ledger-mode-map
               ("C-x C-s" . jc/ledger-save-buffer))
         ([remap evil-write] . jc/ledger-evil-write))
  :hook (ledger-mode . ledger-flymake-enable)
  :preface
  (defun jc/ledger-save-buffer ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (ledger-mode-clean-buffer)
    (save-buffer))
  (defun jc/ledger-evil-write ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (ledger-mode-clean-buffer)
    (evil-write nil nil))
  :custom
  (ledger-binary-path "hledger")
  (ledger-mode-should-check-version nil)
  (ledger-init-file-name "")
  (ledger-default-date-format "%m/%d")
  (ledger-post-amount-alignment-at :decimal)
  (ledger-highlight-xact-under-point nil))

(provide 'init-ledger)
;;; init-ledger.el ends here
