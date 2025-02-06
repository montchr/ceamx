;;  -*- lexical-binding: t; -*-

(require 'ceamx-paths)

(package! elfeed
  (keymap-set ceamx-launch-prefix "f" #'elfeed))

(package! elfeed-org
  (require 'elfeed-org)

  ;; When `elfeed' starts, `elfeed-org' will read the configuration.
  (elfeed-org)

  (setopt rmh-elfeed-org-files
          (list
           (file-name-concat ceamx-reading-feed-dir "index.org")
           (locate-user-emacs-file "feeds.org"))))

(provide 'ceamx-init-news)
;;; ceamx-init-news.el ends here
