;;; init-projectile.el --- Projectile settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :demand
  :config
  (projectile-mode 1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-enable-caching t)
  (projectile-project-search-path '("~/workspace/" "~/dotfiles/")))

(use-package helm-projectile
  :requires (projectile helm)
  :config
  (helm-projectile-on))

(provide 'init-projectile)
;;; init-projectile.el ends here
