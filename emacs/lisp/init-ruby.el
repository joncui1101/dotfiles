;;; init-ruby.el --- Setup ruby packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ruby-mode
  :ensure nil)

(use-package yard-mode
  :disabled
  :hook ruby-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
