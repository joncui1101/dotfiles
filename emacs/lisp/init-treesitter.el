;;; init-treesitter.el --- Setup treesitter -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package tree-sitter
  :hook (((go-mode python-mode ruby-mode) . tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(provide 'init-treesitter)
;;; init-treesitter.el ends here
