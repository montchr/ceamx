;; -*- lexical-binding: t; -*-

(elpaca-wait)

(require 'org-capture)
(require 'doct)

(setopt doct-default-entry-type 'entry)
(setopt doct-warnings t)

(setopt org-capture-templates
        (doct
         '(("Inbox item" :keys "c"
            :file ceamx-default-todo-file
            :headline "Inbox"
            :template ("* TODO %?"
                       "%i %a")
            :icon ("checklist" :set "octicon" :color "green"))

           ("Note" :keys "n"
            :file denote-last-path
            :type plain
            :template denote-org-capture
            :no-save t
            :immediate-finish nil
            :kill-buffer t
            :jump-to-captured t)

           ("Journal entry" :keys "j"
            :file denote-journal-extras-path-to-new-or-existing-entry
            :template ("* %U %?"
                       "%i %a")
            :kill-buffer t
            :empty-lines 1)

           ;; ("Emacs" :keys "e"
           ;;  :file ceamx-literate-config-file
           ;;  :template "* TODO "
           ;;  )
           )))

(after! org-super-agenda
  ;; XXX: `setq' to avoid type error with `setopt'
  (setq org-super-agenda-groups
        '((:name "Today"
                 :time-grid t
                 :todo "TODAY")

          (:name "Important"
                 :tag "financial"
                 :priority "A")

          (:order-multi
           (2 (:name "Personal"
                     :habit t
                     :tag ("personal" "home"))
              (:name "Shopping"
                     :tag "shopping"
                     :tag "buy")))

          (:todo "WAITING" :order 8)
          (:todo ("SOMEDAY" "TO-READ" "TO-LISTEN" "TO-WATCH")
                 :order 9)
          (:priority<= "B" :order 1))))

(define-keymap :keymap ceamx-web-prefix-map
  "i" #'org-web-tools-insert-link-for-url
  "O" #'org-web-tools-read-url-as-org)

(after! org-mode
  (keymap-set org-mode-map "C-c W a" #'org-web-tools-archive-attach)
  (keymap-set org-mode-map "C-c W c" #'org-web-tools-convert-links-to-page-entries))

(provide 'ceamx-init-flows)
;;; ceamx-init-flows.el ends here
