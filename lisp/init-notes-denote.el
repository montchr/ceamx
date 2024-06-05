;;; init-notes-denote.el --- Denote configuration          -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; <https://protesilaos.com/emacs/denote>

;;; Code:

(require 'ceamx-paths)

(use-package denote
  :config
  (setopt denote-directory ceamx-notes-default-dir)
  (setopt denote-known-keywords '("emacs"))
  (setopt denote-infer-keywords t)
  (setopt denote-sort-keywords t)
  (setopt denote-prompts '(title keywords))
  ;; TODO: exclude ".archive"
  ;; (setopt denote-excluded-directories-regexp nil)
  (setopt denote-excluded-keywords-regexp nil)
  ;; Pick dates, where relevant, with Org's advanced interface:
  (setopt denote-date-prompt-use-org-read-date t)
  (setopt denote-allow-multi-word-keywords t)
  (setopt denote-date-format nil)       ; read doc string
  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  ;; Also see ~denote-link-backlinks-display-buffer-action~ which is a bit
  ;; advanced.
  (setopt denote-backlinks-show-context t)
  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  ;; We use different ways to specify a path for demo purposes.
  (setopt denote-dired-directories
    (list denote-directory
      (thread-last denote-directory (expand-file-name "attachments"))))
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;; Alternatively:
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Here is a custom, user-level command from one of the examples we
  ;; showed in this manual.  We define it here and add it to a key binding
  ;; below.
  (defun my-denote-journal ()
    "Create an entry tagged 'journal' with the date as its title.
If a journal for the current day exists, visit it.  If multiple
entries exist, prompt with completion for a choice between them.
Else create a new file."
    (interactive)
    (let* ((today (format-time-string "%A %e %B %Y"))
           (string (denote-sluggify today))
           (files (denote-directory-files-matching-regexp string)))
      (cond
       ((> (length files) 1)
        (find-file (completing-read "Select file: " files nil :require-match)))
       (files
        (find-file (car files)))
       (t
        (denote
         today
         '("journal"))))))

  ;; Key bindings specifically for Dired.
  (define-keymap :keymap dired-mode-map
    ;; FIXME: avoid C-c C-* for stuff like this
    "C-c C-d C-i" #'denote-link-dired-marked-notes
    "C-c C-d C-r" #'denote-dired-rename-marked-files
    "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter)

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

  ;; Also check the commands ~denote-link-after-creating~,
  ;; ~denote-link-or-create~.  You may want to bind them to keys as well.

  ;; If you want to have Denote commands available via a right click
  ;; context menu, use the following and then enable
  ;; ~context-menu-mode~.
  (add-hook 'context-menu-functions #'denote-context-menu))


;;
;;; denote-menu <https://github.com/namilus/denote-menu>
;;

(use-package denote-menu
  :after (denote)
  :commands (list-denotes
              denote-menu-clear-filters
              denote-menu-filter
              denote-menu-filter-by-keyword
              denote-menu-filter-out-keyword
              denote-menu-export-to-dired)
  :config
  (keymap-global-set "C-c z" #'list-denotes)

  ;; TODO: `define-keymap'
  (keymap-set denote-menu-mode-map "c" #'denote-menu-clear-filters)
  (keymap-set denote-menu-mode-map "/ r" #'denote-menu-filter)
  (keymap-set denote-menu-mode-map "/ k" #'denote-menu-filter-by-keyword)
  (keymap-set denote-menu-mode-map "/ o" #'denote-menu-filter-out-keyword)
  (keymap-set denote-menu-mode-map "e" #'denote-menu-export-to-dired))

;;
;;; Integrations
;;

;; <https://github.com/mclear-tools/consult-notes#denote>
(after! (denote consult-notes)
  (consult-notes-denote-mode)
  ;; Search only for text files in Denote dir.
  (setopt consult-notes-denote-files-function
        (function denote-directory-text-only-files)))



(provide 'init-notes-denote)
;;; init-notes-denote.el ends here
