;;; init-history.el --- History management           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; Configuration for session history like undo/redo, edits, kill-ring,
;; recent files, and so on.

;;; Code:

;;; `savehist' (internal)
(use-feature! savehist
  :config
  (setopt savehist-additional-variables '( search-ring
                                           regexp-search-ring
                                           kill-ring))
  (setopt savehist-autosave-interval 60)
  ;; NOTE: Also configured by `no-littering'.
  (setopt savehist-file (expand-file-name "savehist" cmx-local-dir))
  (savehist-mode +1))

;;; `recentf' (internal)
(use-feature! recentf
  :config
  (setopt recentf-max-saved-items 500) ; default => 20
  (setopt recentf-max-menu-items 15)   ; default => 10
  ;; Disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files.
  (setopt recentf-auto-cleanup 'never)
  (dolist (path '(cmx-etc-dir cmx-var-dir))
    (add-to-list 'recentf-exclude path))
  (recentf-mode +1))

;;
;;; Undo/redo
;;

;; Advice from the author of `undo-fu':
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
;; via <https://github.com/emacsmirror/undo-fu?tab=readme-ov-file#undo-limits>
(setopt undo-limit 67108864) ; 64mb.
(setopt undo-strong-limit 100663296) ; 96mb.
(setopt undo-outer-limit 1006632960) ; 960mb.

;;; undo-fu :: <https://codeberg.org/ideasman42/emacs-undo-fu>
;;  Simple, stable linear undo with redo for Emacs.
(use-package undo-fu
  :config
  (keymap-global-unset "C-z")
  (keymap-global-set "C-z" #'undo-fu-only-undo)
  (keymap-global-set "C-S-z" #'undo-fu-only-redo)

  (after! [evil]
    (setopt evil-undo-system 'undo-fu)))

;;; undo-fu-session :: <https://codeberg.org/ideasman42/emacs-undo-fu-session>
;;  Save & recover undo steps between Emacs sessions.
(use-package undo-fu-session
  :after undo-fu

  :init
  (setopt undo-fu-session-directory (expand-file-name "undo-fu-session" cmx-var-dir))

  :config
  (setopt undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setopt undo-fu-session-ignore-temp-files t)
  (setopt undo-fu-session-ignore-encrypted-files t)

  (undo-fu-session-global-mode))

;;; vundo (visual undo) :: <https://github.com/casouri/vundo>
(use-package vundo
  :commands (vundo)
  :defines (vundo-unicode-symbols vundo-glyph-alist)
  :config
  (setopt vundo-glyph-alist vundo-unicode-symbols))

(provide 'init-history)
;;; init-history.el ends here
