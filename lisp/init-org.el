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

(require 'config-notes)
(require 'config-org)

;; Must be set before loading Org-Mode.
(defvar org-directory cmx-notes-dir)

(unless (file-directory-p org-directory)
  (make-directory org-directory))

(use-package org
  :init
  (add-hook 'org-mode-hook #'prettify-symbols-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)

  :config

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
          "⭠ now ─────────────────────────────────────────────────")
)

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
  :commands (org-modern-mode org-modern-agenda)
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package org-modern-indent
  :elpaca (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
  :config
  (setopt org-startup-indented t)
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

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
