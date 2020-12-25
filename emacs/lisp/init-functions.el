(defun jc/reload-emacs-config ()
  (interactive)
  (load-file user-init-file))

(defun jc/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun jc/browse-url-firefox (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (pcase system-type
    ('darwin (start-process (concat "open " url) nil "open" "-a" "firefox" url))
    ('windows-nt (message "Windows not available"))
    ('gnu/linux (message "Linux not available"))))

(defun jc/browse-url-vlc (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (pcase system-type
    ('darwin (start-process (concat "open " url) nil "open" "-a" "vlc" url "--args" "--video-on-top"))
    ('windows-nt (message "Windows not available"))
    ('gnu/linux (message "Linux not available"))))

(provide 'init-functions)
