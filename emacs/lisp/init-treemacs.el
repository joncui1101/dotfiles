;;; init-treemacs.el --- Setup treemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package treemacs
  :defer t
  :bind (:map global-map
              ("C-x t t" . treemacs)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
