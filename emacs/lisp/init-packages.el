;;; init-packages.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;; Standard package repositories

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
             ("gnu" . "https://elpa.gnu.org/packages/")))

(unless package--initialized
  (package-initialize))

;; Bootstrap use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package delight
  :delight
  (emacs-lisp-mode "elisp"))

(provide 'init-packages)
;;; init-packages.el ends here
