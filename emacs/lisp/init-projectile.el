;;; init-projectile.el --- Projectile settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :delight
  :init
  (projectile-mode 1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  ;; (projectile-completion-system 'helm)
  (projectile-project-search-path '("~/workspace/work/" "~/workspace/personal")))

(provide 'init-projectile)
;;; init-projectile.el ends here
