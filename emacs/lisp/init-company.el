;;; init-company.el --- Setup company mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package company
  :delight
  :config (global-company-mode 1)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1))

(provide 'init-company)
;;; init-company.el ends here
