;;;; Previous

;; (def-arm! ceamx-go-prev-map "[" "[Prev]"
;;   "TAB" #'tab-previous
;;   "["   #'previous-buffer
;;   "b"   #'previous-buffer
;;   "e"   #'flycheck-previous-error
;;   "F"   #'previous-window-any-frame
;;   "t"   #'tab-previous)

;;;; Next

;; (def-arm! ceamx-go-next-map "]" "[Next]"
;;   "TAB" #'tab-next
;;   "]"   #'next-buffer
;;   "b"   #'next-buffer
;;   "e"   #'flycheck-next-error
;;   "F"   #'next-window-any-frame
;;   "t"   #'tab-next)

;;;; Goto

;; TODO: make this more convenient

;; (def-map! ceamx-goto-map
;;   "d" '("definition" . xref-find-definitions)
;;   "r" '("references" . xref-find-references))

;;; "B" => Bookmarks

;; (def-arm! ceamx-bookmark-map "B" "[Bookmarks]"
;;   "F" #'burly-bookmark-frames
;;   "W" #'burly-bookmark-windows)
;;; "F" => Frames

;; (defvar-keymap ceamx-frame-map
;;   "b" '("save layout..." . burly-bookmark-frames)
;;   "F" '("switch to..." . select-frame-by-name)
;;   "n" '("create" . make-frame-on-current-monitor)
;;   "N" '("create on monitor..." . make-frame-on-monitor)
;;   "o" '("other" . other-frame)
;;   "R" '("rename..." . set-frame-name)
;;   "[" '("prev" . previous-window-any-frame)
;;   "]" '("next" . next-window-any-frame))


;;; "g" => Git

;; TODO: disabled to try out vanilla keybinds with meow keypad defaults
;; (def-arm! ceamx-git-map "g" "[Git]"
;;   "b" #'magit-branch
;;   "B" #'magit-blame
;;   "f" #'magit-find-file
;;   "g" #'magit-status
;;   "G" #'magit-dispatch
;;   "l" #'magit-log-buffer-file
;;   "s" #'magit-stage-file
;;   "S" #'magit-unstage-file
;;   "t" #'git-timemachine)


  ;; TODO: add as command
  ;; FIXME: create directory if not exists
  ;; "H" `("cheatsheet..." . ,(cmd!!
  ;;                            #'ido-find-file-in-dir
  ;;                            current-prefix-arg
  ;;                            ceamx-cheatsheets-dir))
  ;; TODO:      example:   (cl-find-if #'fboundp '(harper-dad-joint
  ;; helpful-at-point describe-key))



;; (def-arm! ceamx-notes-map "n" "[Note]"
;;   "b" #'denote-backlinks
;;   "c" #'org-capture
;;   "d" #'denote-date
;;   "f" '("[find]" . (keymap))
;;   "f f" #'denote-find-link
;;   "f b" #'denote-find-backlink
;;   "i" #'denote-link                     ; "insert" mnemonic
;;   "I" #'denote-add-links
;;   "j" #'my-denote-journal               ; our custom command
;;   "n" #'denote
;;   "N" #'denote-type
;;   "o" '("[Org-Mode]" . ceamx-org-map)
;;   ;; Note that `denote-rename-file' can work from any context, not just
;;   ;; Dired buffers.  That is why we bind it here to the `global-map'.
;;   "r" #'denote-rename-file
;;   "R" #'denote-rename-file-using-front-matter
;;   "s" #'denote-subdirectory
;;   "t" #'denote-template
;;   ;; "zettelkasten" mnemonic
;;   "z" #'denote-signature)

;;; "s" => Search

(def-arm! ceamx-search-map "s" "[Search]"
  "d" `("directory..." . ,(cmd! (consult-ripgrep
                                  (file-name-directory buffer-file-name))))
  "h" '("history..." . consult-isearch-history)
  "j" '("symbols (f)..." . consult-lsp-file-symbols)
  "J" '("symbols (g)..." . consult-lsp-symbols)
  "l" '("library..." . (lambda () (interactive "P")
                         (call-interactively
                           (if %
                             #'find-library-other-window
                             #'find-library))))
  "o" '("outline (f)..." . consult-outline)
  ;; TODO: use thing-at-point as default value like `projectile-ripgrep' (which cannot find ripgrep)

  "p" '("grep (p)..." . consult-ripgrep)
  "R" #'project-query-replace-regexp
  ;; "R" '("replace (p)..." . projectile-replace)
  "s" '("line (f)..." . consult-line)
  "v" '("variable" . find-variable-at-point)
  "V" '("variable..." . find-variable)
  ;; "x" '("refs (p)" . projectile-find-references)


  ;; (def-arm! ceamx-tab-map "TAB" "[Tab]"
;;   "TAB"  '("other" . tab-recent)
;;   "d"    '("delete" . tab-close)
;;   "h"    '("prev" . tab-previous)
;;   "l"    '("next" . tab-next)
;;   "n"    '("new" . tab-new)
;;   "t"    '("other" . tab-recent)
;;   "x"    '("close" . tab-close))
;;; "O" => Open

;; (def-arm! ceamx-open-map "o" "[Open]"
;;   "d" #'dired
;;   "e" #'eshell
;;   "l" '("link-at-point" . link-hint-open-link-at-point)
;;   "L" '("link..." . link-hint-open-link)
;;   "m" '("mail" . compose-mail)
;;   "n" '("news" . newsticker-show-news)
;;   "s" #'suggest)


;; (def-arm! ceamx-session-map "q" "[Session]"
;;   "f" '("font..." . fontaine-set-preset)
;;   "p" '("packages" . ceamx-packages-map)
;;   "q" '("close frame" . delete-frame)
;;   "Q" '("save+quit" . save-buffers-kill-emacs)
;;   "r" '("restart" . restart-emacs)
;;   "t" '("theme..." . consult-theme))
