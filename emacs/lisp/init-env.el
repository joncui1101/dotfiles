;;; init-env.el --- Env settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 's)
(require 'f)

(defun load-env (file)
  "Load the contents of the FILE as 'exec-path'."
  (let ((file-path (f-expand file user-emacs-directory)))
    (if (f-exists? file-path)
        (let ((paths (s-trim (f-read-text file-path))))
          (setenv "PATH" paths)
          (dolist (path (s-split ":" paths))
            (add-to-list 'exec-path path))))))

(provide 'init-env)
;;; init-env.el ends here
