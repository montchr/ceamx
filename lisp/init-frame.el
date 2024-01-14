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

(require 'lib-frame)

(use-feature! emacs
  :config
  ;; Disable menu bar by default.
  (menu-bar-mode -1))

;; macOS has terrible window management UX and it always has.
;; and it is only getting *worse*.
;; this used to be fine, but then they introduced
;; "stage manager", a crippling load of unusable horseshit.
;; it's a wonder that yabai is still possible!
(if +sys-mac-p
    ;; GUI menu bar is necessary otherwise Emacs will be treated as a
    ;; non-application OS window (e.g. no focus capture). Menu bar is, however,
    ;; ugly in terminal frames.
    ;;
    ;; NOTE: It seems that frame decorations are also necessary now...? See above.
    (progn
      (defun cmx--restore-gui-menu-bar-hook (&optional frame)
        (when-let (frame (or frame (selected-frame)))
          (when (display-graphic-p frame)
            (set-frame-parameter frame 'menu-bar-lines 1))))
      (add-hook 'window-setup-hook #'cmx--restore-gui-menu-bar-hook)
      (add-hook 'after-make-frame-functions #'cmx--restore-gui-menu-bar-hook))
  ;; Hide window decorations.
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; macOS: Stop C-z from minimizing windows.
(keymap-global-set "C-z" #'cmx/maybe-suspend-frame)

(provide 'init-frame)
;;; init-frame.el ends here
