;;; init-org.el --- org-mode initialisation -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;;  Configuration for `org-mode'.

;;;; Resources

;; <https://github.com/james-stoup/emacs-org-mode-tutorial>

;;; Investigate:

;; TODO: <https://orgmode.org/worg/org-contrib/org-choose.html>
;; TODO: <https://github.com/tarsius/org-elisp-help/blob/main/org-elisp-help.el>

;;; Code:

(require 'lib-common)

(require 'config-notes)
(require 'config-org)

;;; Configuration of important paths

;; Most notes will be stored in `ceamx-notes-dir'.
;;
;; The value of `org-directory' will be used as a default destination for new
;; notes, especially as they relate to tasks and agendas. For that reason, use
;; the `ceamx-agenda-dir'.
;;
;; Must be set before loading Org-Mode.
(defvar org-directory ceamx-agenda-dir)
(f-mkdir-full-path org-directory)

;;; Configure general Org-Mode settings

;; <https://github.com/minad/org-modern#configuration>

(use-feature! org
  :demand t
  :commands (org-priority-up org-priority-down)

  :init
  (add-hook 'org-mode-hook #'prettify-symbols-mode)


  (def-hook! +org-mode-init-keys-h () 'org-mode-hook
    "Adjust global keybindings in `org-mode' buffers."
    (keymap-global-set "M-g o" #'consult-org-heading))

  :config
  (setopt org-agenda-files
    (append
      (f-glob "*.org" ceamx-agenda-dir)
      (f-glob "*.org" ceamx-work-notes-dir)))

;;;; Editing settings

  (setopt org-auto-align-tags nil)
  (setopt org-tags-column 0)
  (setopt org-catch-invisible-edits 'show-and-error)
  (setopt org-special-ctrl-a/e nil)
  (setopt org-insert-heading-respect-content t)
  ;; Indenting code blocks by default is unnecessary and confusing.
  (setopt org-edit-src-content-indentation 0)
  (setopt org-src-preserve-indentation t)

;;;; Appearance settings

  (setopt org-ellipsis "…")
  (setopt org-hide-emphasis-markers nil)
  (setopt org-image-actual-width 300)
  (setopt org-indent-indentation-per-level 4)
  (setopt org-pretty-entities t)
  (setopt org-startup-folded 'show2levels)
  ;; Avoid unnecessary indentation effects unless specified in file header.
  (setopt org-startup-indented nil)
  (setopt org-startup-with-inline-images t)

;;;; Workflow and state settings

  (setopt org-log-done 'time)
  (setopt org-todo-keywords
    '((sequence
        "TODO(t)"
        "INPRG(i@/!)"
        "BLOCKED(b@)"
        "HOLD(h@)"
        "PROJ(p)"
        "|"
        "DONE(d!)"
        "CANCELLED(x@/!)")))

;;;; Keybinding settings

  (setopt org-return-follows-link t)

  (keys! org-mode-map
    "C-c <up>" #'org-priority-up
    "C-c <down>" #'org-priority-down
    "C-c a" #'org-agenda
    "C-c l" #'org-store-link
    "C-c c" #'org-capture)

;;;; Agenda settings

;;;;; Agenda appearance

  (setopt org-agenda-tags-column 0)
  (setopt org-agenda-block-separator ?─)
  (setopt org-agenda-time-grid
    '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setopt org-agenda-current-time-string
    "⭠ now ─────────────────────────────────────────────────"))

;;; Configurate `org-capture' templates with the help of `doct'

;; <https://github.com/progfolio/doct>

(use-package doct
  :demand t
  :autoload (doct))

(use-feature! org-capture
  :after (doct)
  :config
  (setopt org-capture-templates
    (doct `(("Inbox"
              :keys "t"
              ;; TODO: make sure this icon spec is up to date with 2024
              :icon ("checklist" :set "octicon" :color "green")
              ;; TODO: should this be evaled/expanded?
              :file ceamx-org-capture-default-file
              :prepend t
              :headline "Inbox"
              :type entry
              :template ("* TODO %?"
                          "%i %a"))))))

;;; org-ql :: <https://github.com/alphapapa/org-ql>

;; > A searching tool for Org-mode, including custom query languages, commands,
;; > saved searches and agenda-like views, etc.

(use-package org-ql)

;;; org-modern <https://github.com/minad/org-modern>

;; (use-package org-modern
;;   :commands (org-modern-mode org-modern-agenda)
;;   :init
;;   (add-hook 'org-mode-hook #'org-modern-mode)
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;;; Provide agenda improvements with `org-super-agenda'

(use-package org-super-agenda
  :demand t)

;;; Support drag-and-drop images to org-mode

;; <https://github.com/abo-abo/org-download>

(use-package org-download
  :ensure t
  :demand t
  :init
  (require 'org-download)
  (add-hook 'dired-mode-hook #'org-download-enable))

;;; Automatically surround pasted code with source block markup via `org-rich-yank'

;; <https://github.com/unhammer/org-rich-yank>

(use-package org-rich-yank
  :ensure t
  :demand t                             ; See README as to why
  :after (org-download)
  :config
  (keymap-set org-mode-map "C-M-y" #'org-rich-yank))

;;; Use `org-web-tools' to view, capture, and archive webpages in org-mode

(use-package org-web-tools
  :defer t)

(provide 'init-org)
;;; init-org.el ends here
