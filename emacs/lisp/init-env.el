;;; init-env.el --- Env settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun load-env (file)
  (let (file-path (f-expand file user-emacs-directory))
    (if (f-exists? file-path)
        (let (paths (-last-item (s-split "=" (car (s-lines (s-trim (f-read-text file-path)))))))
          (setenv "PATH" paths)
          (dolist (path (s-split ":" paths))
            (add-to-list 'exec-path path))))))

(provide 'init-env)
;;; init-env.el ends here
