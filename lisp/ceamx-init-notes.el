;; -*- lexical-binding: t; -*-

(require 'f)

(require 'ceamx-lib)
(require 'ceamx-note)

(use-feature! ceamx-note
  :demand t
  :config
  ;; Ensure essential directories exist to prevent errors.
  (dolist (dir (list ceamx-note-dir ceamx-note-journal-dir))
    (f-mkdir-full-path dir)))

(package! consult-notes
  (keymap-set ceamx-note-prefix-map "s" #'consult-notes)
  (keymap-global-set "M-s n" #'consult-notes))

(after! consult-notes
  (setopt consult-notes-file-dir-sources
        `(("Org" ?o ,ceamx-agenda-dir)
          ("Work" ?w ,ceamx-note-work-dir)
;;          ("Journal" ?j ,ceamx-note-journal-dir :hidden t)
          ))

  ;; (setopt consult-notes-org-headings-files
  ;;         (dolist (dir (list ceamx-note-default-dir
  ;;                            ceamx-note-work-dir))
  ;;           (append (f-entries dir (##f-ext-p % "org") t))))

  (setopt consult-notes-org-headings-files nil)

  (consult-notes-org-headings-mode 1)

  (when (locate-library "denote")
    (setopt consult-notes-denote-files-function
            (##denote-directory-files nil t t))
    (consult-notes-denote-mode 1)))

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

  (add-hook 'find-file-hook #'denote-fontify-links-mode-maybe)
  (after! dired
    (add-hook 'dired-mode-hook #'denote-dired-mode))
  (after! mouse
    (add-hook 'context-menu-functions #'denote-context-menu))

  (define-keymap :keymap ceamx-capture-prefix-map
    ;; TODO: <https://protesilaos.com/emacs/denote#text-h:eb72086e-05be-4ae3-af51-7616999fc7c9>
    "r" #'denote-region)

  (keymap-global-set "C-c n" #'denote)

  (define-keymap :keymap ceamx-note-prefix-map
    "n" #'denote
    "d" #'denote-sort-dired
    "r" #'denote-rename-file
    "R" #'denote-rename-file-using-front-matter)

  (after! dired
    (define-keymap :keymap dired-mode-map
      "C-c C-d C-i" #'denote-link-dired-marked-notes
      "C-c C-d C-r" #'denote-dired-rename-files
      "C-c C-d C-k" #'denote-dired-rename-marked-files-with-keywords
      "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter))
  (after! org
    (define-keymap :keymap org-mode-map
      "C-c n h" #'denote-org-extras-extract-org-subtree
      "C-c n l" #'denote-link
      "C-c n L" #'denote-add-links
      "C-c n b" #'denote-backlinks)))

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
            "journal"))
  (setopt denote-prompts '(title keywords))
  (setopt denote-org-capture-specifiers "%l\n%i\n%?")
  (setopt denote-date-prompt-use-org-read-date t)
  (setopt denote-backlinks-show-context t)

  ;; Auto-rename Denote buffers with `denote-rename-buffer-format'.
  (setopt denote-rename-confirmations '(modify-file-name
                                        rewrite-front-matter))
  (denote-rename-buffer-mode 1))

(package! (denote-silo :host github :repo "protesilaos/denote-silo"))

(after! denote
  (require 'denote-silo)

  (setopt denote-silo-directories
          (list denote-directory
                ceamx-note-journal-dir
                ceamx-note-work-dir)))

(package! (denote-journal :host github :repo "protesilaos/denote-journal"))

(after! denote
  (require 'denote-journal)

  (setopt denote-journal-directory ceamx-note-journal-dir)
  (setopt denote-journal-keyword '("journal")))

(use-feature! ceamx-note
  :commands (ceamx-note/create-or-visit-journal-entry))

(package! consult-denote
  (define-keymap :keymap ceamx-note-prefix-map
    "f f" #'consult-denote-find
    "g" #'consult-denote-grep)

  (consult-denote-mode 1))

(package! denote-explore
  (require 'denote-explore)

  (setopt denote-explore-network-directory
          (file-name-as-directory (concat ceamx-note-dir "graphs")))
  (setopt denote-explore-network-filename "denote-network"))

(provide 'ceamx-init-notes)
;;; ceamx-init-notes.el ends here
