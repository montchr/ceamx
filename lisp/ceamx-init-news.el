;;  -*- lexical-binding: t; -*-

(require 'ceamx-paths)

(package! elfeed
  (after! elfeed
    (require 'ceamx-news)

    (make-directory elfeed-db-directory t)

    (setopt elfeed-search-filter "@1-week-ago +unread")
    (setopt elfeed-show-entry-switch #'pop-to-buffer
            elfeed-show-entry-delete #'ceamx-news/delete-pane))

  (after! popper
    (add-to-list 'popper-reference-buffers "^\\*elfeed-entry")))

(package! elfeed-goodies
  (after! elfeed
    (require 'elfeed-goodies)
    (elfeed-goodies/setup))
  ;; (after! elfeed-goodies
  ;;   )
  )

(package! elfeed-org
  ;; HACK: Must be set prior to feature load.
  ;; <https://github.com/doomemacs/doomemacs/blob/2bc052425ca45a41532be0648ebd976d1bd2e6c1/modules/app/rss/config.el#L74-L75>
  (setq rmh-elfeed-org-files
        (list (file-name-concat ceamx-reading-feed-dir "index.org")))

  (after! elfeed
    (require 'elfeed-org)
    (elfeed-org)))

(package! elfeed-tube
  (after! elfeed
    (require 'elfeed-tube)
    (keymap-set elfeed-show-mode-map "F" #'elfeed-tube-fetch)
    (keymap-set elfeed-show-mode-map "<remap> <save-buffer>" #'elfeed-tube-save)
    (keymap-set elfeed-search-mode-map "F" #'elfeed-tube-fetch)
    (keymap-set elfeed-search-mode-map "<remap> <save-buffer>" #'elfeed-tube-save))
  (after! elfeed-tube
    (setopt elfeed-tube-auto-save-p nil
            elfeed-tube-auto-fetch-p t)
    (elfeed-tube-setup)))

(package! elfeed-tube-mpv
  (after! elfeed
    (keymap-set elfeed-show-mode-map "C-c C-f" #'elfeed-tube-mpv-follow-mode)
    (keymap-set elfeed-show-mode-map "C-c C-w" #'elfeed-tube-mpv-where)))

(package! elfeed-score
  (after! elfeed
    (elfeed-score-enable))
  (after! elfeed-score
    ;; <https://www.unwoundstack.com/doc/elfeed-score/curr>
    (setopt elfeed-score-serde-score-file
            (file-name-concat ceamx-reading-feed-dir "elfeed-scores.eld")))
  (after! (elfeed elfeed-score)
    (keymap-set elfeed-search-mode-map "=" elfeed-score-map)))

(provide 'ceamx-init-news)
;;; ceamx-init-news.el ends here
