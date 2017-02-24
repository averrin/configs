
(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))


(defun averrin/elfeed-config ()

  (elfeed-goodies/setup)
  (with-eval-after-load 'elfeed
    (evilified-state-evilify-map elfeed-search-mode-map
      :mode elfeed-search-mode
      :eval-after-load elfeed-search
      :bindings
      ",c"  'elfeed-db-compact
      ",gr" 'elfeed-update
      ",gR" 'elfeed-search-update--force
      ",gu" 'elfeed-unjam
      "R" 'elfeed-mark-all-as-read
      "c" nil
      "gr" nil
      "gR" nil
      "gu" nil
      "o" 'elfeed-search-show-entry
      "O" 'elfeed-search-browse-url
      "q"  'quit-window
      "w"  'nil
      "W"  'nil))

  (with-eval-after-load 'elfeed
    (evilified-state-evilify-map elfeed-show-mode-map
      :mode elfeed-show-mode
      :eval-after-load elfeed-search
      :bindings
      "n"  'elfeed-goodies/split-show-next
      "p"  'elfeed-goodies/split-show-prev
      "f"  'elfeed-goodies/show-ace-link
      ))

  (defun elfeed-search-format-date (date)
    (format-time-string "%d.%m.%y %H:%M" (seconds-to-time date)))

  (define-key elfeed-show-mode-map (kbd "C-SPC") 'evil-scroll-page-down)
  (define-key elfeed-show-mode-map (kbd "C-S-SPC") 'evil-scroll-page-up)

  (define-key elfeed-search-mode-map (kbd "C-SPC") 'evil-scroll-page-down)
  (define-key elfeed-search-mode-map (kbd "C-S-SPC") 'evil-scroll-page-up)

  (averrin/elfeed-set-feeds)

  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 days ago"
                                :remove 'unread))

  )

(defun averrin/elfeed-set-feeds()
  (setq elfeed-feeds
    '(("http://4gophers.com/rss" go)
        ("http://planet.emacsen.org/atom.xml" emacs)
        ("http://pragmaticemacs.com/feed/" emacs)
        ("http://mysku.ru/rss/index/" mysku)
        ("http://feeds.feedburner.com/geekcity/geekfeed" comics)
        ("http://spidermedia.ru/rss.xml" comics)
        ("http://starkindustries.ru/news/rss/" comics)
        ("http://www.strangearts.ru/rss.xml" comics)
        ("http://dccomics.ru/rss.xml" comics)
        ("http://ageofgeeks.com/feed/" comics)
        ("http://geektimes.ru/rss/best/" main)
        ("http://feeds.feedburner.com/org/LOR" main)
        ("http://www.opennet.ru/opennews/opennews_all_noadv.rss" main)
        ("http://feeds.feedburner.com/inoblogger?format=xml" main)
        ("http://habrahabr.ru/rss/hubs/" main)
        ("http://www.ancientdomainsofmystery.com/feeds/posts/default" main)
        ("https://ultimatehackingkeyboard.com/feed" main)
        ("https://vivaldi.net/blogs/teamblog?format=feed&type=rss" main)
        ("http://android.mobile-review.com/feed/" main)
        ("http://gagadget.com/rss/" main)
        ("http://feeds2.feedburner.com/webupd8" main)
  )))
