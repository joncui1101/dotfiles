;;; init-flycheck.el --- Setup flycheck environment -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :defer t
  :preface
  :hook ((ledger-mode emacs-lisp-mode lsp-mode) . flycheck-mode)
  :custom
  (flycheck-flake8-maximum-line-length 120)
  (flycheck-flake8-maximum-complexity 40))

(use-package flycheck-ledger
  :requires flycheck)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
