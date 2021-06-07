;;; init-yasnippet.el --- Setup yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
