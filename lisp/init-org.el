;;; init-org.el --- org-mode initialisation -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

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

;;; Code:

(defconst +org-capture-default-file
  (expand-file-name
   (concat +path-notes-dir "inbox.org"))
  "Path to default inbox file for new org-capture entries.")

(use-package org
  :init
  (setopt org-directory (expand-file-name +path-notes-dir))

  :config
  (setopt org-image-actual-width 300)
  (setopt org-startup-with-inline-images t)

  ;; via <https://github.com/minad/org-modern#configuration>
  ;; Edit settings
  (setopt org-auto-align-tags nil)
  (setopt org-tags-column 0)
  (setopt org-catch-invisible-edits 'show-and-error)
  (setopt org-special-ctrl-a/e t)
  (setopt org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (setopt org-hide-emphasis-markers t)
  (setopt org-pretty-entities t)
  (setopt org-ellipsis "…")

  ;; Agenda styling
  ;; via <https://github.com/minad/org-modern#configuration>
  (setopt org-agenda-tags-column 0)
  (setopt org-agenda-block-separator ?─)
  (setopt org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setopt org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")

  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode)))

(use-package doct
  :defer t
  :commands (doct))

(use-feature org-capture
  :after (org doct)
  :config
  (setopt org-capture-templates
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
  :after (org)
  :commands (global-org-modern-mode)
  :config
  (global-org-modern-mode))

;;
;;; org-cliplink :: <https://github.com/rexim/org-cliplink>
;;

;; NOTE: abandoned, many open issues, likely unreliable

(use-package org-cliplink
  :after org
  :commands org-cliplink)

;;
;;; `org-rich-yank' :: <https://github.com/unhammer/org-rich-yank>
;;
;;  Automatically surround code in src block markup upon paste.

;; TODO: bindings
(use-package org-rich-yank
  :after org
  :commands org-rich-yank)

(provide 'init-org)
;;; init-org.el ends here
