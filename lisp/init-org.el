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
  ;; FIXME: hydras
  ;; :general
  ;; (global-definer
  ;;   "X"  '("capture..." . org-capture))
  ;; (+general-global-application
  ;;   "o" '(:ignore t :which-key "org...")

  ;;   "oc" 'org-capture
  ;;   ;; TODO
  ;;   ;; "oC" '+org-capture-again
  ;;   "oi" 'org-insert-link
  ;;   ;; TODO
  ;;   ;; "oj" 'org-chronicle

  ;;   "ok" '(:ignore t :which-key "clock...")
  ;;   "okg" 'org-clock-goto
  ;;   "oki" 'org-clock-in-last
  ;;   ;; TODO: not available? needs to be running?
  ;;   ;; "okj" 'org-clock-jump-to-current-clock
  ;;   "oko" 'org-clock-out
  ;;   "okr" 'org-resolve-clocks

  ;;   "ol" 'org-store-link
  ;;   "om" 'org-tags-view
  ;;   "os" 'org-search-view
  ;;   "oT" 'org-todo-list)

  ;; (global-leader
  ;;   "a" '("archive subtree" . org-archive-subtree)
  ;;   "E" '("export..." . org-export-dispatch)

  ;;   "l" '(:ignore true :wk "link")
  ;;   "l l" '("insert link" . org-insert-link)
  ;;   "l s" '("store link" . org-store-link)

  ;;   "r" '("refile..." . org-refile)
  ;;   "p" '("priority" . org-priority)
  ;;   "q" '("tag" . org-set-tags-command)
  ;;   "s" '("sort" . org-sort)

  ;;   "t" '(:ignore true :wk "todo")
  ;;   "t t" '("heading todo" . org-todo)
  ;;   "t s" '("schedule" . org-schedule)
  ;;   "t d" '("deadline" . org-deadline)
  
  ;;   "x" '("toggle checkbox" . org-toggle-checkbox))

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
