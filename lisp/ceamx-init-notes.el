;; -*- lexical-binding: t; -*-

(require 'f)

(require 'ceamx-lib)
(require 'ceamx-note)

(use-feature! ceamx-note
  :config
  ;; Ensure essential directories exist to prevent errors.
  (dolist (dir (list ceamx-note-dir ceamx-note-journal-dir))
    (f-mkdir-full-path dir)))

(package! consult-notes)

(after! consult-notes
  (setopt consult-notes-file-dir-sources
          `(("Default" ?D ,ceamx-note-default-dir)
            ("Work" ?w ,ceamx-note-work-dir)
            ("Journal" ?j ,ceamx-note-journal-dir :hidden t)))
  (setopt consult-notes-org-headings-files
          (append (f-entries ceamx-note-default-dir (##f-ext-p % "org") t)))

  (consult-notes-org-headings-mode)

  (when (locate-library "denote")
    (setopt consult-notes-denote-files-function
            (lambda () (denote-directory-files nil t t)))
    (consult-notes-denote-mode)))

;; via <https://github.com/mclear-tools/consult-notes#embark-support>
;; (after! (consult-notes embark)
;; 	(defun ceamx/consult-notes-embark-action (cand)
;;     "Do something with CAND."
;;     (interactive "fNote: ")
;;     ;; FIXME: needs function
;;     ;;
;;     ;; > Note that Embark will run on the CAND at point, which will often return
;;     ;; > either a file name, or a file name plus other annotations, depending on
;;     ;; > what your sources are. So you’ll have to write a function to manipulate
;;     ;; > CAND to give you a viable path to the file or a directory containing
;;     ;; > the file.
;;     (my-function))

;;   (defvar-keymap consult-notes-map
;;     :doc "Keymap for Embark notes actions."
;;     :parent embark-file-map
;;     "m" #'ceamx/consult-notes-embark-action)

;;   (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

;;   ;; Make `embark-export' use dired for notes.
;;   (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired))

(package! denote
  (require 'denote)

  ;;
  ;; Integrations

  (add-hook 'find-file-hook #'denote-fontify-links-mode)

  (after! dired
    (add-hook 'dired-mode-hook #'denote-dired-mode)

    (define-keymap :keymap dired-mode-map
      "C-c C-d C-i" #'denote-link-dired-marked-notes
      "C-c C-d C-r" #'denote-dired-rename-marked-files
      "C-c C-d C-k" #'denote-dired-rename-marked-files-with-keywords
      "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter))

  (after! mouse
    (add-hook 'context-menu-functions #'denote-context-menu))

  (after! org
    (keymap-set org-mode-map "C-c n h" #'denote-org-extras-extract-org-subtree)))

(after! denote
  (setopt denote-directory ceamx-note-default-dir)
  (setopt denote-excluded-directories-regexp "\\.archive")
  (setopt denote-dired-directories
          (list denote-directory
                (thread-last denote-directory
                             (expand-file-name "attachments"))))

  (setopt denote-save-buffers nil)
  (setopt denote-infer-keywords t
          denote-sort-keywords t)
  (setopt denote-known-keywords
          '("emacs"
            "philosophy"
            "correspondence"
            "language"
            "jobwork"
            "journal"
            "blog"))
  (setopt denote-prompts '(title keywords))
  (setopt denote-org-capture-specifiers "%l\n%i\n%?")
  (setopt denote-date-prompt-use-org-read-date t)
  ;; also: `denote-link-backlinks-display-buffer-action'
  (setopt denote-backlinks-show-context t)

  ;; Auto-rename Denote buffers with `denote-rename-buffer-format'.
  (setopt denote-rename-confirmations '(modify-file-name
                                        rewrite-front-matter))
  (denote-rename-buffer-mode 1))

(after! (denote org-capture)
  ;; (appendopt! org-capture-templates
  ;;     (doct '(("New note" :keys "n"
  ;;              :type plain
  ;;              :file denote-last-path
  ;;              :template denote-org-capture
  ;;              :no-save t
  ;;              :immediate-finish nil
  ;;              :kill-buffer t
  ;;              :jump-to-captured t))))
  (add-to-list 'org-capture-templates
      '("n" "New note" plain
        (file denote-last-path)
        #'denote-org-capture
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t)))

(use-feature! ceamx-note
  :after denote
  :commands (ceamx-note/denote/pick-silo-then-command)
  :config
  (setopt ceamx-note-silo-directories
          (list ceamx-note-journal-dir
                ceamx-note-work-dir)))



(defun ceamx-note/create-or-visit-journal-entry ()
  "Invoke `denote-journal-extras-new-or-existing-entry' scoped to the
 Journal silo."
  (interactive)
  (let ((denote-directory ceamx-note-journal-dir))
    (call-interactively #'denote-journal-extras-new-or-existing-entry)))

(after! denote
  (define-keymap :keymap ceamx-capture-prefix-map
    ;; TODO: <https://protesilaos.com/emacs/denote#text-h:eb72086e-05be-4ae3-af51-7616999fc7c9>
    "r" #'denote-region))

(after! denote
  (require 'denote-journal-extras)

  (setopt denote-journal-extras-directory ceamx-note-journal-dir)
  (setopt denote-journal-extras-keyword '("journal"))

  (after! org-capture
    ;; (appendopt! org-capture-templates
    ;;             (doct "Journal" :keys "j"
    ;;                   :file denote-journal-extras-))
    ))

(package! consult-denote
  (define-keymap :keymap ceamx-note-prefix-map
    "f f" #'consult-denote-find
    "g" #'consult-denote-grep)

  (consult-denote-mode 1))

(provide 'ceamx-init-notes)
;;; ceamx-init-notes.el ends here
