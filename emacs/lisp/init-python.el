;;; init-python.el --- Setup python packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python-mode
  :delight '(:eval (format " py[%s]" (pyenv-mode-version)))
  :hook (python-mode . (lambda () (modify-syntax-entry ?_ "w" python-mode-syntax-table))))

(use-package pyenv-mode
  :custom
  (pyenv-mode-mode-line-format nil)
  :preface
  (defun jc/projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((pyenv-version-path (f-expand ".python-version" (projectile-project-root))))
      (if (f-exists? pyenv-version-path)
          (progn
            (pyenv-mode-set (car (s-lines (s-trim (f-read-text pyenv-version-path)))))
            (setq flycheck-python-flake8-executable (s-concat (pyenv-mode-full-path (pyenv-mode-version)) "/bin/python3")))
        (pyenv-mode-unset))))
  :hook
  (python-mode . pyenv-mode)
  (projectile-after-switch-project . jc/projectile-pyenv-mode-set))

(provide 'init-python)
;;; init-python.el ends here
