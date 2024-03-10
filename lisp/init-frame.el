;;; init-frame.el --- Frame modifications -*- lexical-binding: t -*-

;; Copyright (c) 2023-2024  Chris Montgomery <chris@cdom.io>

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

;; FIXME: unreadable childframes sometimes. e.g. command output, embark.
;; at least make sure these childframes seriously have modeline disabled and
;; their padding reduced

;;; Code:

;;; Requirements

(require 'config-env)

(require 'lib-frame)
(require 'lib-common)

;;; Configure frame decorations

(unless +sys-mac-p
  ;; Hide window decorations.
  (add-to-list 'default-frame-alist '(undecorated . t)))

;;;; Handle macOS-specific workarounds

(when +sys-mac-p
  ;; `undecorated-round' is macOS-specific.
  (add-to-list 'default-frame-alist '(undecorated-round . t))

  ;; GUI menu bar is necessary otherwise Emacs will be treated as a
  ;; non-application OS window (e.g. no focus capture).
  ;; <https://github.com/doomemacs/doomemacs/blob/d657be1744a1481dc4646d0b62d5ee1d3e75d1d8/lisp/doom-start.el#L118-L128>
  (def-hook! ceamx-frame--maybe-restore-gui-menu-bar-h (&optional frame)
    '(after-make-frame-functions window-setup-hook)
    "TODO: Provide source for this approach (Doom?), and why it does what it does."
    (when-let (frame (or frame (selected-frame)))
      (when (display-graphic-p frame)
        (set-frame-parameter frame 'menu-bar-lines 1))))

  ;; Stop C-z from minimizing windows.
  (keymap-global-unset "C-z" t))


;;; General frame configuration

;;;; Disable the frame menu bar by default

(menu-bar-mode -1)

;;;; Enable the resurrection of frames with `undelete-frame'

(undelete-frame-mode 1)

(provide 'init-frame)
;;; init-frame.el ends here
