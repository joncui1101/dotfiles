;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'ledger-mode)
(setq ledger-binary-path "hledger"
      ledger-mode-should-check-version nil
      ledger-init-file-name ""
      ledger-default-date-format "%m/%d"
      ledger-highlight-xact-under-point nil)

(add-to-list 'auto-mode-alist '("\\.journal$" . ledger-mode))

;;;(add-hook 'ledger-mode-hook 'goto-address-prog-mode)

(provide 'init-ledger)
;;; init-ledger.el ends here
