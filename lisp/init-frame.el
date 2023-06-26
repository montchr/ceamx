;;; init-frame.el --- Frame modifications -*- lexical-binding: t -*-

;; Copyright (c) 2023  Chris Montgomery <chris@cdom.io>

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

;;  

;;; Code:

(use-feature emacs
  :init
  (when +sys-mac-p
    ;; macOS: Show window decorations.
    (add-to-list 'default-frame-alist '(undecorated . nil))

    ;; macOS: GUI menu bar is necessary otherwise Emacs will be treated as a
    ;; non-application OS window (e.g. no focus capture). Menu bar is, however,
    ;; ugly in terminal frames.
    (defun cmx/restore-gui-menu-bar-h (&optional frame)
      (when-let (frame (or frame (selected-frame)))
        (when (display-graphic-p frame)
          (set-frame-parameter frame 'menu-bar-lines 1))))
    (add-hook 'window-setup-hook #'cmx/restore-gui-menu-bar-h)
    (add-hook 'after-make-frame-functions #'cmx/restore-gui-menu-bar-h)))

(provide 'init-frame)
;;; init-frame.el ends here
