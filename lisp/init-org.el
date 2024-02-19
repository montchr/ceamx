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

;;; Code:

(require 'lib-common)

(require 'config-notes)
(require 'config-org)

;; Most notes will be stored in `ceamx-notes-dir'.
;;
;; The value of `org-directory' will be used as a default destination for new
;; notes, especially as they relate to tasks and agendas. For that reason, use
;; the `ceamx-agenda-dir'.
;;
;; Must be set before loading Org-Mode.
(defvar org-directory ceamx-agenda-dir)
(f-mkdir-full-path org-directory)

(use-feature! org
  :init
  (add-hook 'org-mode-hook #'prettify-symbols-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)

  :config
  (setopt org-agenda-files
    (append
      (f-glob "*.org" ceamx-agenda-dir)
      (f-glob "*.org" ceamx-work-notes-dir)))

  ;; via <https://github.com/minad/org-modern#configuration>
  ;; Editing settings
  (setopt org-auto-align-tags nil)
  (setopt org-tags-column 0)
  (setopt org-catch-invisible-edits 'show-and-error)
  (setopt org-special-ctrl-a/e t)
  (setopt org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (setopt org-ellipsis "…")
  (setopt org-hide-emphasis-markers t)
  (setopt org-image-actual-width 300)
  (setopt org-pretty-entities t)
  (setopt org-startup-folded 'show2levels)
  (setopt org-startup-indented t)
  (setopt org-startup-with-inline-images t)

  ;; Agenda styling
  ;; via <https://github.com/minad/org-modern#configuration>
  (setopt org-agenda-tags-column 0)
  (setopt org-agenda-block-separator ?─)
  (setopt org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (setopt org-agenda-current-time-string
          "⭠ now ─────────────────────────────────────────────────"))

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
                   :file ceamx-org-capture-default-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))))))

;;; org-ql :: <https://github.com/alphapapa/org-ql>

;; > A searching tool for Org-mode, including custom query languages, commands,
;; > saved searches and agenda-like views, etc.

(use-package org-ql
  :commands (org-ql-find org-ql-search))

;;; org-modern <https://github.com/minad/org-modern>
(use-package org-modern
  :after (org)
  :commands (org-modern-mode org-modern-agenda)
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; FIXME: not in nixpkgs
;;(use-package org-modern-indent
;;  ;; ;; FIXME: :elpaca (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
 ;; :config
;;  (setopt org-startup-indented t)
;;  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

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
