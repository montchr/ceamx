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

;;;; Investigate

;; TODO: <https://orgmode.org/worg/org-contrib/org-choose.html>
;; TODO: <https://github.com/tarsius/org-elisp-help/blob/main/org-elisp-help.el>

;;; Code:

;; Requirements


;; [[file:../config.org::*Requirements][Requirements:1]]
(require 'ceamx-keymaps)

(require 'config-notes)
(require 'config-org)

(require 'lib-common)
;; Requirements:1 ends here

;; Configuration of important files/directories

;; Must be set before loading Org-Mode or any of its sub-features are used.

;; Most notes will be stored in `ceamx-notes-dir'.

;; The value of `org-directory' will be used as a default destination for new
;; notes, especially as they relate to tasks and agendas.  For that reason, use
;; the `ceamx-agenda-dir'.

;; FIXME: Because these directories are managed by Syncthing, creating them
;; automatically is not a great idea if they do not already exist.  A better
;; workaround to the issue of Org-Mode failing to load might be setting the
;; default target to a subdirectory of `user-emacs-directory'.

;; Why?  Because you do not want to end up with two Syncthing entries with the
;; same intended target path but with different IDs.  If Syncthing is not set up
;; yet, then any directory created via the Emacs configuration will probably
;; result in a conflict when Syncthing tries to take control of these paths.


;; [[file:../config.org::*Configuration of important files/directories][Configuration of important files/directories:1]]
(defvar org-directory ceamx-agenda-dir)

;; TODO: I would prefer to check for the directory's existence explicitly --
;; this feels strange at the top-level.
(f-mkdir-full-path org-directory)

(setopt org-agenda-files ceamx-default-agenda-files)
;; Configuration of important files/directories:1 ends here

;; General settings

;; <https://github.com/minad/org-modern#configuration>


;; [[file:../config.org::*General settings][General settings:1]]
(add-hook 'org-mode-hook #'prettify-symbols-mode)
;; General settings:1 ends here

;; Tags


;; [[file:../config.org::*Tags][Tags:1]]
(setopt org-auto-align-tags nil)
(setopt org-tags-column 0)

(setopt org-fold-catch-invisible-edits 'error) ; default: smart
;; Tags:1 ends here

;; Headings / List Items

;; Prevent TAB behavior oddities at the end of headlines.

;; When nil, pressing TAB at the end of a headline whose content is folded
;; will act on the folded (non-visible) area instead of the headline, which
;; may cause unexpected changes to the content (depending on the setting of `org-catch-invisible-edits'.


;; [[file:../config.org::*Headings / List Items][Headings / List Items:1]]
(setopt org-special-ctrl-a/e t)
;; Headings / List Items:1 ends here

;; [[file:../config.org::*Headings / List Items][Headings / List Items:2]]
;; Instead of forcing this always, use the function
;; `org-insert-heading-respect-content' directly, bound to [C-<return>].
(setopt org-insert-heading-respect-content nil)

(setopt org-M-RET-may-split-line nil)
(setopt org-cycle-emulate-tab t)
(setopt org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(setopt org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
;; Headings / List Items:2 ends here

;; Source code blocks


;; [[file:../config.org::*Source code blocks][Source code blocks:1]]
;; Changing the indentation of source code is unhelpful and destructive.
(setopt org-edit-src-content-indentation 0)

(setopt org-edit-src-persistent-message nil)
(setopt org-src-fontify-natively t)
(setopt org-src-preserve-indentation t)
(setopt org-src-tab-acts-natively t)
;; TODO: current window when narrow/short frame, but otherwise reorganize-frame is good
;; (setopt org-src-window-setup 'current-window)
(setopt org-src-window-setup 'reorganize-frame)
(setopt org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))
;; Source code blocks:1 ends here

;; Text formatting


;; [[file:../config.org::*Text formatting][Text formatting:1]]
(setopt org-ellipsis "…")
(setopt org-hide-emphasis-markers nil)
(setopt org-pretty-entities t)
;; Text formatting:1 ends here

;; Images


;; [[file:../config.org::*Images][Images:1]]
(setopt org-image-actual-width 300)
(setopt org-startup-with-inline-images t)
;; Images:1 ends here

;; Folding and indentation


;; [[file:../config.org::*Folding and indentation][Folding and indentation:1]]
(setopt org-indent-indentation-per-level 2)
(setopt org-startup-folded 'content)

;; Avoid unnecessary indentation effects unless specified in file header.
(setopt org-startup-indented nil)
;; Folding and indentation:1 ends here

;; Workflow and state settings


;; [[file:../config.org::*Workflow and state settings][Workflow and state settings:1]]
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
;; Workflow and state settings:1 ends here

;; Agenda appearance


;; [[file:../config.org::*Agenda appearance][Agenda appearance:1]]
(setopt org-agenda-tags-column 0)
(setopt org-agenda-block-separator ?─)
(setopt org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setopt org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
;; Agenda appearance:1 ends here

;; Refiling


;; [[file:../config.org::*Refiling][Refiling:1]]
(setopt org-refile-targets '((org-agenda-files . (:maxlevel . 5))
                             (ceamx-default-todo-file . (:level . 1))
                             (nil . (:level . 1))))

(setopt org-refile-use-outline-path 'file)
(setopt org-outline-path-complete-in-steps nil)
(setopt org-refile-allow-creating-parent-nodes 'confirm)
(setopt org-refile-use-cache nil)

;; TODO: move this setting elsewhere
(setopt org-reverse-note-order t)       ; prepend new notes
;; Refiling:1 ends here

;; Keybindings


;; [[file:../config.org::*Keybindings][Keybindings:1]]
(setopt org-return-follows-link t)
;; Keybindings:1 ends here

;; [[file:../config.org::*Keybindings][Keybindings:2]]
(with-eval-after-load 'org
  (defvar org-mode-map)
  (declare-function consult-org-heading "consult-org")

  (global-keys!
    "C-c a" #'org-agenda
    "C-c c" #'org-capture)

  (define-keymap :keymap org-mode-map
    "C-c <up>" #'org-priority-up
    "C-c <down>" #'org-priority-down

    ;; FIXME: " key sequence starts with non-prefix key" -- that is, conflicts
    ;; with `ceamx-toggle-map'... maybe if that was not a prefix command but
    ;; instead was just a keymap...?
    ;; "C-c C-t l" #'org-toggle-link-display

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
;; Keybindings:2 ends here

;; Enforce the correct `tab-width' to prevent errors

;; <https://github.com/doomemacs/doomemacs/commit/43870bf8318f6471c4ce5e14565c9f0a3fb6e368>


;; [[file:../config.org::*Enforce the correct `tab-width' to prevent errors][Enforce the correct `tab-width' to prevent errors:1]]
(defun +org-mode--local-set-tab-width-h ()
  "Set the `tab-width' in `org-mode' buffers to 8 columns.
Any `tab-width' value other than 8 will result in an error.

This should be set as late as possible, after all other
`org-mode-hook' functions added by packages and
configurations.  Hence the use of `after-change-major-mode-hook',
which runs at the very end of major-mode activation.

Intended for use as a local hook function on
`after-change-major-mode-hook' as added within `org-mode-hook'."

  ;; This check is necessary to handle, for example, `org-edit-src-code', which
  ;; clones the `org-mode' buffer and changes its major-mode.
  (when (derived-mode-p 'org-mode)
    (setq tab-width 8)))

(def-hook! +org-mode-enforce-tab-width-h ()
  'org-mode-hook
  "Add a local hook to control `tab-width' on `after-change-major-mode-hook'."
  (add-hook 'after-change-major-mode-hook #'+org-mode--local-set-tab-width-h 0 t))
;; Enforce the correct `tab-width' to prevent errors:1 ends here

;; ~doct~: `org-capture' template definer

;; <https://github.com/progfolio/doct>


;; [[file:../config.org::*~doct~: `org-capture' template definer][~doct~: `org-capture' template definer:1]]
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
                   :file ceamx-default-capture-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))))))
;; ~doct~: `org-capture' template definer:1 ends here

;; ~org-ql~

;; <https://github.com/alphapapa/org-ql>


;; [[file:../config.org::*~org-ql~][~org-ql~:1]]
(package! org-ql)
;; ~org-ql~:1 ends here

;; ~org-modern~

;; <https://github.com/minad/org-modern>


;; [[file:../config.org::*~org-modern~][~org-modern~:1]]
(package! org-modern
  ;; Ensure leading stars are replaced by spaces.
  (setopt org-modern-hide-stars ?\s)

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))
;; ~org-modern~:1 ends here

;; ~org-super-agenda~

;; <https://github.com/alphapapa/org-super-agenda>

;; TODO: Configure


;; [[file:../config.org::*~org-super-agenda~][~org-super-agenda~:1]]
(package! org-super-agenda
  ;; FIXME: probably can use autoloads instead
  (require 'org-super-agenda))
;; ~org-super-agenda~:1 ends here

;; ~org-download~: support dragging-and-dropping images into Org buffers

;; <https://github.com/abo-abo/org-download>


;; [[file:../config.org::*~org-download~: support dragging-and-dropping images into Org buffers][~org-download~: support dragging-and-dropping images into Org buffers:1]]
(package! org-download
  (require 'org-download)
  (add-hook 'dired-mode-hook #'org-download-enable))
;; ~org-download~: support dragging-and-dropping images into Org buffers:1 ends here

;; ~org-rich-yank~: surround pasted code with src-block markup

;; <https://github.com/unhammer/org-rich-yank>

;; Paraphrasing from the README:

;; This package should be loaded immediately. We never know when the user will
;; hit =C-M-y=, so we always have to store the current buffer on kills. You can
;; remove the `require' and have lazy/deferred loading, but then the first
;; time you hit =C-M-y= after startup, you’ll get a message that you have to
;; kill the selection again.


;; [[file:../config.org::*~org-rich-yank~: surround pasted code with src-block markup][~org-rich-yank~: surround pasted code with src-block markup:1]]
(package! org-rich-yank
  (with-eval-after-load 'org-download
    (require 'org-rich-yank)

    (keymap-set org-mode-map "C-M-y" #'org-rich-yank)))
;; ~org-rich-yank~: surround pasted code with src-block markup:1 ends here

;; ~org-web-tools~: view, capture, and archive webpages in org-mode


;; [[file:../config.org::*~org-web-tools~: view, capture, and archive webpages in org-mode][~org-web-tools~: view, capture, and archive webpages in org-mode:1]]
(package! org-web-tools
  (keymap-set ceamx-insert-map "l" #'org-web-tools-insert-link-for-url))
;; ~org-web-tools~: view, capture, and archive webpages in org-mode:1 ends here

;; ~auto-tangle-mode~: automatically tangle literate Org files


;; [[file:../config.org::*~auto-tangle-mode~: automatically tangle literate Org files][~auto-tangle-mode~: automatically tangle literate Org files:1]]
(package! (auto-tangle-mode
           :host github
           :repo "progfolio/auto-tangle-mode.el")
  (autoload 'auto-tangle-mode "auto-tangle-mode"))
;; ~auto-tangle-mode~: automatically tangle literate Org files:1 ends here

;; ~org-sidebar~: provide a sidebar for Org buffers

;; <https://github.com/alphapapa/org-sidebar>


;; [[file:../config.org::*~org-sidebar~: provide a sidebar for Org buffers][~org-sidebar~: provide a sidebar for Org buffers:1]]
(package! org-sidebar)
;; ~org-sidebar~: provide a sidebar for Org buffers:1 ends here

;; ~org-bookmark-heading~: Support heading bookmarks


;; [[file:../config.org::*~org-bookmark-heading~: Support heading bookmarks][~org-bookmark-heading~: Support heading bookmarks:1]]
(package! org-bookmark-heading
  (setopt org-bookmark-jump-indirect t)
  (after! org
    (require 'org-bookmark-heading)))
;; ~org-bookmark-heading~: Support heading bookmarks:1 ends here

(provide 'init-org)
;;; init-org.el ends here
