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
    (kill-buffer (current-buffer)))
  (defun jc/elfeed-evil-collection-remap (_mode _mode-keymaps &rest _rest)
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
     ("http://feeds.dzone.com/java" dev java)
     ("http://feeds.dzone.com/performance" dev)
     ("https://medium.com/feed/paypal-engineering" dev)
     ("https://martinfowler.com/feed.atom" dev)
     ("https://www.8bitmen.com/feed/" dev)
     ("https://netflixtechblog.com/feed" dev)
     ("http://www.daemonology.net/hn-daily/index.rss" dev)
     ("https://blog.codinghorror.com/rss/" dev)
     ("http://www.allthingsdistributed.com/atom.xml" dev)
     ("https://dave.cheney.net/feed/atom" dev go)
     ("https://eng.uber.com/feed/" dev)
     ("https://codeascraft.com/atom" dev)
     ("https://instagram-engineering.com/feed" dev)
     ("https://planet.emacslife.com/atom.xml" dev emacs)
     ("https://github.blog/feed" dev)
     ("https://blog.golang.org/feeds/posts/default" dev go)
     ("https://developer.squareup.com/blog/rss.xml" dev)
     ("https://stripe.com/blog/feed.rss" dev)
     ("https://smittenkitchen.com/atom" cooking)
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" emacs vid) ;; system crafters
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UC6x7GwJxuoABSosgVXDYtTw" maker vid) ;; i like to make stuff
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKwM-7sO1_Tw9EmYhKfpBBw" maker vid) ;; perkins builder brothers
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCex87CEaoqw365i63aKxepQ" life vid) ;; ice1cube
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUT8RoNBTJvwW1iErP6-b-A" amongus vid) ;; disguised toast
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBh2UCIk9In7uf87GJU6qgg" civ vid) ;; the game mechanic
     ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqqJQ_cXSat0KIAVfIfKkVA" cooking vid) ;; kenji
     ("https://vimeo.com/andrewrk/videos/rss" dev vid)
     ("https://www.gobeyond.dev/rss" dev go)
     ("https://cprss.s3.amazonaws.com/golangweekly.com.xml" dev go)
     ("https://sec.report/CIK/0001811210.rss" spac) ;; cciv
     ("https://sec.report/CIK/0001807846.rss" spac) ;; fuse
     ("https://sec.report/CIK/0001809987.rss" spac) ;; gsah
     ("https://sec.report/CIK/0001794717.rss" spac) ;; scvx
     ("https://sec.report/CIK/0001816090.rss" spac) ;; ftoc
     ("https://sec.report/CIK/0001820630.rss" spac) ;; actc
     ("https://sec.report/CIK/0001815086.rss" spac) ;; btwn
     ("https://sec.report/CIK/0001829426.rss" spac) ;; fpac
     ("https://sec.report/CIK/0001818873.rss" spac) ;; ipof
     ("https://sec.report/CIK/0001823575.rss" spac) ;; lnfa
     ("https://sec.report/CIK/0001811882.rss" spac) ;; psth
     ("https://sec.report/CIK/0001779474.rss" spac) ;; sspk
     ("https://world.hey.com/joaoqalves/feed.atom" dev)
     ))
  (elfeed-search-filter "+unread ")
  (elfeed-search-title-max-width 100))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
