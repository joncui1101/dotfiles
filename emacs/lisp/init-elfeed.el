;;; init-elfeed.el --- Setup elfeed -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elfeed
  :preface
  (defun jc/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed)
    (elfeed-db-load)
    (elfeed-search-update--force)
    (elfeed-update))
  (defun jc/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (elfeed-kill-buffer))
  (defun jc/elfeed-evil-collection-remap (_mode mode-keymaps &rest _rest)
    (evil-collection-define-key 'normal 'elfeed-search-mode-map
      (kbd "RET") 'elfeed-search-browse-url
      (kbd "S-<return>") 'elfeed-search-show-entry
      "q" 'jc/elfeed-save-db-and-bury
      "Q" 'jc/elfeed-save-db-and-bury))
  :hook (evil-collection-setup . jc/elfeed-evil-collection-remap)
  :bind (("C-x w" . jc/elfeed-load-db-and-open))
  :custom
  (elfeed-db-directory "~/.config/cache/emacs/elfeed")
  (elfeed-feeds
   '(("https://techcrunch.com/rss" tech)
     ("https://lifehacker.com/rss" tech)
     ("https://gizmodo.com/rss" tech)
     ("http://rss.desiringgod.org/" christian)
     ("http://amiquiettimes.wordpress.com/feed/" christian)
     ("http://feeds.feedburner.com/HighScalability" dev)
     ("http://feeds.dzone.com/java" dev)
     ("http://feeds.dzone.com/performance" dev)
     ("https://medium.com/feed/paypal-engineering" dev)
     ("https://martinfowler.com/feed.atom" dev)
     ("https://www.8bitmen.com/feed/" dev)
     ("https://netflixtechblog.com/feed" dev)
     ("http://www.daemonology.net/hn-daily/index.rss" dev)
     ("https://blog.codinghorror.com/rss/" dev)
     ("http://www.allthingsdistributed.com/atom.xml" dev)
     ("https://dave.cheney.net/feed/atom" dev)
     ("https://eng.uber.com/feed/" dev)
     ("https://codeascraft.com/atom" dev)
     ("https://instagram-engineering.com/feed" dev)
     ("https://planet.emacslife.com/atom.xml" dev emacs)
     ("https://github.blog/feed" dev)
     ("https://blog.golang.org/feeds/posts/default" dev)
     ("https://developer.squareup.com/blog/rss.xml" dev)
     ("https://stripe.com/blog/feed.rss" dev)
     ("https://smittenkitchen.com/atom" cooking)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" emacs youtube) ;; system crafters
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC6x7GwJxuoABSosgVXDYtTw" maker youtube) ;; i like to make stuff
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKwM-7sO1_Tw9EmYhKfpBBw" maker youtube) ;; perkins builder brothers
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCex87CEaoqw365i63aKxepQ" life youtube) ;; ice1cube
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUT8RoNBTJvwW1iErP6-b-A" amongus youtube) ;; disguised toast
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBh2UCIk9In7uf87GJU6qgg" civ youtube) ;; the game mechanic
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqqJQ_cXSat0KIAVfIfKkVA" cooking youtube) ;; kenji
     ))
  (elfeed-search-filter "+unread "))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
