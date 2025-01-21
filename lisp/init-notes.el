;;; init-notes.el --- Notetaking features  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2025  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'f)
(require 'ceamx-paths)

(dolist (dir (list ceamx-notes-dir ceamx-journal-dir ceamx-agenda-dir))
  (f-mkdir-full-path dir))
(package! consult-notes)

(after! consult-notes
  (setopt consult-notes-file-dir-sources
          `(("Default" ?D ,ceamx-notes-default-dir)
            ("Org" ?o ,org-directory)))

  (setopt consult-notes-org-headings-files org-agenda-files)
  (consult-notes-org-headings-mode)

  ;; Integrate with Denote if available.
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  (setopt consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))
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
(require 'ceamx-paths)

(package! denote
  (require 'denote)
  (require 'denote-journal-extras)

  (define-prefix-command 'ceamx-notes-prefix 'ceamx-notes-prefix-map)
  (define-prefix-command 'ceamx-journal-prefix 'ceamx-journal-prefix-map)

  (keymap-global-set "C-c j" #'ceamx-notes-prefix)
  (define-keymap :keymap ceamx-notes-prefix-map
    "j" #'ceamx-journal-prefix
    "j j" #'denote-journal-extras-new-entry)

  ;; Integrations

  (add-hook 'find-file-hook #'denote-fontify-links-mode)
  (after! dired
    (add-hook 'dired-mode-hook #'denote-dired-mode))
  (after! mouse
    (add-hook 'context-menu-functions #'denote-context-menu))

  ;; Customizations

  (setopt denote-directory ceamx-notes-default-dir)
  (setopt denote-save-buffers nil)
  (setopt denote-known-keywords '("emacs" "philosophy" "correspondence" "language" "work" "journal" "blog"))
  (setopt denote-infer-keywords t)
  (setopt denote-sort-keywords t)
  (setopt denote-file-type nil)         ; Org is the default
  (setopt denote-prompts '(title keywords))
  (setopt denote-excluded-directories-regexp "\\.archive")
  (setopt denote-rename-confirmations '(modify-file-name
                                        rewrite-front-matter))

  ;; Pick dates, where relevant, with Org's advanced interface.
  (setopt denote-date-prompt-use-org-read-date t)

  ;; Use a file-type-specific date format.
  (setopt denote-date-format nil)

  ;; also: `denote-link-backlinks-display-buffer-action'
  (setopt denote-backlinks-show-context t)

  (setopt denote-dired-directories
          (list denote-directory
                (thread-last denote-directory (expand-file-name "attachments"))))

  ;; Automatically rename Denote buffers using the
  ;; `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))
(with-eval-after-load 'dired
  (define-keymap :keymap dired-mode-map
    "C-c C-d C-i" #'denote-link-dired-marked-notes
    "C-c C-d C-r" #'denote-dired-rename-marked-files
    "C-c C-d C-k" #'denote-dired-rename-marked-files-with-keywords
    "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter))

(with-eval-after-load 'org-capture
  (setopt denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
      '("n" "New note (with denote.el)" plain
        (file denote-last-path)
        #'denote-org-capture
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t)))

(provide 'init-notes)
;;; init-notes.el ends here
