;;; init-projectile.el --- Projectile settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :defer 1
  :diminish
  :init
  (projectile-mode 1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories ".github")
  :custom
  (projectile-enable-caching t)
  (projectile-require-project-root t)
  (projectile-indexing-method 'native)
  (projectile-completion-system 'ivy)
  ;; (projectile-completion-system 'helm)
  (projectile-project-search-path '("~/workspace/work/" "~/workspace/personal")))

(provide 'init-projectile)
;;; init-projectile.el ends here
