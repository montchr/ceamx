;;  -*- lexical-binding: t; -*-

;; FIXME: OPML instead of weird lists

(defconst ceamx-reading-dir (concat ceamx-home-dir "Documents/reading/"))

(package! elfeed
  (keymap-set ceamx-launch-prefix "f" #'elfeed))

(package! elfeed-org
  (require 'elfeed-org)

  ;; When `elfeed' starts, `elfeed-org' will read the configuration.
  (elfeed-org)

  (setopt rmh-elfeed-org-files (list (locate-user-emacs-file "feeds.org")
                                     ;; (file-name-concat ceamx-reading-dir "000-feeds.org")
                                     )))

(provide 'ceamx-init-news)
;;; ceamx-init-news.el ends here
