;;; init-projectile.el --- Projectile settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :demand
  :config
  (projectile-mode 1)
  :custom
  (projectile-enable-caching t)
  (projectile-project-search-path '("~/workspace/" "~/dotfiles/")))

(use-package helm-projectile
  :requires (projectile helm)
  :config
  (helm-projectile-on))

(provide 'init-projectile)
;;; init-projectile.el ends here
