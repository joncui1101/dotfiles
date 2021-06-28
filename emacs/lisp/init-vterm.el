;;; init-vterm.el --- vTerm Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package vterm
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell "/usr/bin/zsh"))

(provide 'init-vterm)
;;; init-vterm.el ends here
