;;; init-yasnippet.el --- Setup yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package yasnippet
  :hook ((prog-mode text-mode org-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-l" . yas-expand))
  :config
  (yas-reload-all))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
