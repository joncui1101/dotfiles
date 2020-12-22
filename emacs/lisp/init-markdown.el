;;; init-markdown.el --- Setup markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc"))

(provide 'init-markdown)
;;; init-markdown.el ends here
