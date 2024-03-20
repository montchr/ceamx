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

;;; Requirements

(require 'ceamx-keymaps)

(require 'config-notes)
(require 'config-org)

(require 'lib-common)

;;; Configuration of important files/directories

;; Must be set before loading Org-Mode or any of its sub-features are used.

;; Most notes will be stored in `ceamx-notes-dir'.
;;
;; The value of `org-directory' will be used as a default destination for new
;; notes, especially as they relate to tasks and agendas.  For that reason, use
;; the `ceamx-agenda-dir'.
;;
;; FIXME: Because these directories are managed by Syncthing, creating them
;; automatically is not a great idea if they do not already exist.  A better
;; workaround to the issue of Org-Mode failing to load might be setting the
;; default target to a subdirectory of `user-emacs-directory'.
;;
;; Why?  Because you do not want to end up with two Syncthing entries with the
;; same intended target path but with different IDs.  If Syncthing is not set up
;; yet, then any directory created via the Emacs configuration will probably
;; result in a conflict when Syncthing tries to take control of these paths.

(defvar org-directory ceamx-agenda-dir)
;; TODO: I would prefer to check for the directory's existence explicitly --
;; this feels strange at the top-level.
(f-mkdir-full-path org-directory)

(setopt org-agenda-files (append
                          (f-glob "*.org" ceamx-agenda-dir)
                          (f-glob "*.org" ceamx-work-notes-dir)))

;;; Configure general Org-Mode settings

;; <https://github.com/minad/org-modern#configuration>

(add-hook 'org-mode-hook #'prettify-symbols-mode)

;;;; Editing settings

;;;;; Tags

(setopt org-auto-align-tags nil)
(setopt org-tags-column 0)

(setopt org-fold-catch-invisible-edits 'smart)

;;;;; Headings / List Items

;; Prevent TAB behavior oddities at the end of headlines.
;; When nil, pressing TAB at the end of a headline whose content is folded
;; will act on the folded (non-visible) area instead of the headline, which
;; may cause unexpected changes to the content (depending on the setting of `org-catch-invisible-edits'.
(setopt org-special-ctrl-a/e t)

;; Instead of forcing this always, use the function
;; `org-insert-heading-respect-content' directly, bound to [C-<return>].
(setopt org-insert-heading-respect-content nil)

(setopt org-M-RET-may-split-line nil)
(setopt org-cycle-emulate-tab t)
(setopt org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

;;;;; Source code blocks

;; Changing the indentation of source code is unhelpful and destructive.
(setopt org-edit-src-content-indentation 0)

(setopt org-edit-src-persistent-message nil)
(setopt org-src-fontify-natively t)
(setopt org-src-preserve-indentation t)
(setopt org-src-tab-acts-natively t)
;; TODO: current window when narrow/short frame, but otherwise reorganize-frame is good
(setopt org-src-window-setup 'current-window)
(setopt org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))

;;;; Appearance settings

;;;;; Text formatting

(setopt org-ellipsis "…")
(setopt org-hide-emphasis-markers nil)
(setopt org-pretty-entities t)

;;;;; Images

(setopt org-image-actual-width 300)
(setopt org-startup-with-inline-images t)

;;;;; Folding and indentation

(setopt org-indent-indentation-per-level 2)
(setopt org-startup-folded 'content)

;; Avoid unnecessary indentation effects unless specified in file header.
(setopt org-startup-indented nil)

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

;;;; Agenda settings

;;;;; Agenda appearance

(setopt org-agenda-tags-column 0)
(setopt org-agenda-block-separator ?─)
(setopt org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setopt org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")

;;;; Keybindings

(setopt org-return-follows-link t)

(with-eval-after-load 'org
  (defvar org-mode-map)
  (declare-function consult-org-heading "consult-org")

  (global-keys!
    "C-c a" #'org-agenda
    "C-c c" #'org-capture)

  (keys! org-mode-map
    "C-c <up>" #'org-priority-up
    "C-c <down>" #'org-priority-down

    ;; "C-c l" #'org-store-link

    ;; Swap these around, as I am more likely to adjust subtree than insert an
    ;; arbitrary date from the calendar.
    "C-c <" #'org-promote-subtree
    "C-c C-<" #'org-date-from-calendar
    "C-c >" #'org-demote-subtree
    "C-c C->" #'org-goto-calendar

    ;; Override earlier binding to `consult-outline'.
    "M-g o" #'consult-org-heading)

  ;; Mnemonic is the global key to goto definition/references.
  (keymap-set org-mode-map "M-." #'org-edit-special) ; also: C-c '
  ;; FIXME: Does not seem to work in `emacs-lisp-mode' Org Src buffers
  ;; (keymap-set org-src-mode-map "M-," #'org-edit-src-exit)
  )

;;; Configurate `org-capture' templates with the help of `doct'

;; <https://github.com/progfolio/doct>

(package! doct
  (require 'doct))

(with-eval-after-load 'doct
  (declare-function doct "doct")

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

;;; Install the `org-ql' library

;; <https://github.com/alphapapa/org-ql>

(package! org-ql)

;;; org-modern <https://github.com/minad/org-modern>

(package! org-modern
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;;; Provide agenda improvements with `org-super-agenda'

;; <https://github.com/alphapapa/org-super-agenda>

;; TODO: Configure

(package! org-super-agenda
  ;; FIXME: probably can use autoloads instead
  (require 'org-super-agenda))

;;; Support dragging-and-dropping images into Org buffers

;; <https://github.com/abo-abo/org-download>

(package! org-download
  (require 'org-download)
  (add-hook 'dired-mode-hook #'org-download-enable))

;;; Surround pasted code with src-block markup

;; <https://github.com/unhammer/org-rich-yank>

;; Paraphrasing from the README:

;; This package should be loaded immediately. We never know when the user will
;; hit =C-M-y=, so we always have to store the current buffer on kills. You can
;; remove the `require' and have lazy/deferred loading, but then the first
;; time you hit =C-M-y= after startup, you’ll get a message that you have to
;; kill the selection again.

(package! org-rich-yank
  (with-eval-after-load 'org-download
    (require 'org-rich-yank)

    (keymap-set org-mode-map "C-M-y" #'org-rich-yank)))

;;; Use `org-web-tools' to view, capture, and archive webpages in org-mode

(package! org-web-tools
  (keymap-set ceamx-insert-map "l" #'org-web-tools-insert-link-for-url))

;;; Automatically tangle literate Org files with `auto-tangle-mode'

(package! (auto-tangle-mode
           :host github
           :repo "progfolio/auto-tangle-mode.el")
  (autoload 'auto-tangle-mode "auto-tangle-mode"))

(provide 'init-org)
;;; init-org.el ends here
