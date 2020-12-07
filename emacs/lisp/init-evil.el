;;; init-evil.el --- Settings for evil mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  :custom
  (evil-symbol-word-search t)
  :config
  (evil-mode 1))

(use-package evil-ledger
  :requires (evil ledger-mode)
  :hook (ledger-mode . evil-ledger-mode)
  :custom
  (evil-ledger-sort-key "S"))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :requires evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  :custom
  (evil-snipe-scope 'buffer)
  (evil-snipe-repeat-scope 'buffer)
  (evil-snipe-spillover-scope 'whole-buffer))

(use-package evil-collection
  :requires evil
  :config
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t))

(provide 'init-evil)
;;; init-evil.el ends here
