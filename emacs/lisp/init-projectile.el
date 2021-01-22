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
  (projectile-project-root-functions '(projectile-root-local projectile-root-bottom-up projectile-root-top-down-recurring))
  (projectile-completion-system 'ivy)
  ;; (projectile-completion-system 'helm)
  :config
  (pcase system-type
    ('darwin (setq projectile-project-search-path '("~/workspace/work/" "~/workspace/personal")))
    ('windows-nt (message "Windows not available"))
    ('gnu/linux (setq projectile-project-search-path '("~/workspace/personal"))))
  )

(provide 'init-projectile)
;;; init-projectile.el ends here
