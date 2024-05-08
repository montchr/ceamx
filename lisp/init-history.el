;;; init-history.el --- History management           -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local, lisp

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

;; " Nothing here now but the recordings... "

;; Configuration for session history like undo/redo, edits, kill-ring,
;; recent files, and so on.

;;; Code:

;;; Requirements

(require 'cl-lib)

(require 'ceamx-paths)
(require 'ceamx-lib)

;;; Record some variables' values with ~savehist~ [builtin]

(use-feature! savehist
  :init
  (savehist-mode)

  :config
  (cl-dolist (save '(kill-ring
                      regexp-search-ring
                      search-ring))
    (cl-pushnew save savehist-additional-variables))

  (setopt savehist-autosave-interval 60))

;;; Record point position in buffers with ~saveplace~ [builtin]

(use-feature! saveplace
  :init
  (save-place-mode))

;;; Record recently-accessed files with ~recentf~ [builtin]

(use-feature! recentf
  :init
  (recentf-mode)

  :config
  (setopt recentf-max-saved-items 50)   ; default => 20
  (setopt recentf-max-menu-items 15)    ; default => 10

  ;; Disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files.
  (setopt recentf-auto-cleanup 'never)

  ;; Exclude internal plumbing files.
  (dolist (path '(ceamx-etc-dir ceamx-var-dir))
    (add-to-list 'recentf-exclude path)))

;;; Return to previously-visited buffer positions with ~dogears~

;; <https://github.com/alphapapa/dogears.el>

(package! dogears
  (add-hook 'on-first-buffer-hook #'dogears-mode)

  ;; Also see `ceamx/dogears-dispatch'.
  (global-keys!
    ;; TODO: find a new binding maybe
    ;; "M-g d" #'dogears-go
    "M-g M-b" #'dogears-back
    "M-g M-f" #'dogears-forward
    "M-g M-d" #'dogears-list
    "M-g M-D" #'dogears-sidebar)

  ;; Persist `dogears-list' between Emacs sessions.
  ;; via <https://github.com/alphapapa/dogears.el/issues/4>
  (after! savehist
    (when (boundp 'savehist-additional-variables)
      (add-to-list 'savehist-additional-variables #'dogears-list))))

;; TODO: provide a little more context in transient (label for dogears, links maybe...)
(after! (transient dogears)
  (transient-define-prefix ceamx/dogears-dispatch ()
    "Transient menu for `dogears' history navigation commands."
    [["Navigate"
       ("b" "back" dogears-back :transient transient--do-stay)
       ("f" "forward" dogears-forward :transient transient--do-stay)]
      ;; TODO: when quit one of these Find commands, return to transient
      ["Find"
        ("d" "go..." dogears-go)
        ("l" "list" dogears-list)
        ("S" "sidebar" dogears-sidebar)]])

  (defer-until! (fboundp 'ceamx/dogears-dispatch)
    (keymap-global-set "M-g d" #'ceamx/dogears-dispatch)))

;;; Undo/redo

;;;; Increase undo history limits

;; Advice from the author of ~undo-fu~:
;;
;; > The default undo limits for emacs are quite low _(0.15mb at time of
;; > writing)_ undo-tree for example increases these limits.
;; >
;; > On modern systems you may wish to use much higher limits.
;; >
;; > This example sets the limit to 64mb, 1.5x (96mb) for the strong
;; > limit and 10x (960mb) for the outer limit. Emacs uses 100x for the
;; > outer limit but this may be too high when using increased limits.
;;
;; via <https://codeberg.org/ideasman42/emacs-undo-fu#undo-limits>

(setopt undo-limit 67108864) ; 64mb.
(setopt undo-strong-limit 100663296) ; 96mb.
(setopt undo-outer-limit 1006632960) ; 960mb.

;;;; Support optional linear undo/redo with ~undo-fu~

;; <https://codeberg.org/ideasman42/emacs-undo-fu>

(package! undo-fu
  (keymap-global-set "C-z" #'undo-fu-only-undo)
  (keymap-global-set "C-S-z" #'undo-fu-only-redo))

;;;; Record undo/redo steps across Emacs sessions with ~undo-fu-session~

;; <https://codeberg.org/ideasman42/emacs-undo-fu-session>

;; NOTE: This is *NOT* just for use with ~undo-fu~! It's an essential
;; enhancement to the builtin Emacs undo system as well.

(defvar undo-fu-session-directory
  (expand-file-name "undo-fu-session" ceamx-var-dir))

(package! undo-fu-session
  (setopt undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setopt undo-fu-session-ignore-temp-files t)
  (setopt undo-fu-session-ignore-encrypted-files t)

  (setopt undo-fu-session-compression 'zst)

  (undo-fu-session-global-mode))

;;;; Visualize the Emacs undo tree with ~vundo~ (visual undo)

;; <https://github.com/casouri/vundo>

(package! vundo
  (keymap-global-set "C-x u" #'vundo))

(after! vundo
  (defvar vundo-unicode-symbols)
  (setopt vundo-glyph-alist vundo-unicode-symbols))

(provide 'init-history)
;;; init-history.el ends here
