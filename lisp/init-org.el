;;; init-org.el --- org-mode initialisation -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;; Code:

;; FIXME: ensure exists
(defconst +path-notes-dir (file-name-as-directory "~/Documents/notes"))

(defconst +org-capture-default-file (concat +path-notes-dir "inbox.org"))

(use-package org
  :general
  (global-definer
    "X"  '(org-capture :which-key "capture..."))
  (global-leader
    "a" '(org-archive-subtree :which-key "archive subtree")
    "E" '(org-export-dispatch :which-key "export...")

    "l" '(:ignore true :wk "link")
    "l l" '(org-insert-link :wk "insert link")
    "l s" '(org-store-link :wk "store link")

    "r" '(org-refile :wk "refile...")
    "p" '(org-priority :wk "priority")
    "q" '(org-set-tags-command :wk "tag")
    "s" '(org-sort :wk "sort")

    "t" '(:ignore true :wk "todo")
    "t t" '(org-todo :wk "heading todo")
    "t s" '(org-schedule :wk "schedule")
    "t d" '(org-deadline :wk "deadline")
    
    "x" '(org-toggle-checkbox :wk "toggle checkbox"))

  :init
  ;; FIXME: use `+path-notes-dir'? 
  (setq org-directory "~/Documents/notes/")

  :config

  (setq org-image-actual-width 300)
  (setq org-startup-with-inline-images t)

  ;; via <https://github.com/minad/org-modern#configuration>
  ;; Edit settings
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-special-ctrl-a/e t)
  (setq org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-ellipsis "…")

    ;; Agenda styling
  ;; via <https://github.com/minad/org-modern#configuration>
  (setq org-agenda-tags-column 0)
  (setq org-agenda-block-separator ?─)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setq org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")


  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode)))

(use-package doct
  :defer t
  :commands (doct))

(use-feature org-capture
  :after (org-mode doct)
  :config
  (setq org-capture-templates
        (doct `(("Personal todo"
                 :keys "t"
                 :icon ("checklist" :set "octicon" :color "green")
                 :file +org-capture-default-file
                 :prepend t
                 :headline "Inbox"
                 :type entry
                 :template ("* TODO %?"
                            "%i %a"))))))

;;; org-modern <https://github.com/minad/org-modern>
(use-package org-modern
  :hook ((org-mode . org-modern-mode)))

(provide 'init-org)
;;; init-org.el ends here
