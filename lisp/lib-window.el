;;; lib-window.el --- Helpers for window management  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

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

;; Helper functions for `init-window'.

;;; Code:

(declare-function 'popper-popup-status "popper")
(declare-function 'popper-toggle "popper")

;; TODO: add buffers tracking files in nix store, which are only useful for
;; reference purposes, often invoked when viewing definition of low-level
;; Emacs internals defined in C code (e.g. `string-equal')
(defun +popper-current-buffer-popup-p (buf)
  "Whether the buffer BUF should be considered a popup.
This is intended for use as a predicate in `popper-reference-buffers'."
    (with-current-buffer buf
      (and (derived-mode-p 'fundamental-mode)
           (not (bound-and-true-p scratch-buffer))
           ;; Less than `max-lines' but not empty.
           (let ((lines (count-lines (point-min) (point-max)))
                 (max-lines 10))
             (and (not (zerop lines))
               (< lines max-lines))))))

(defun +popper-close-focused (&rest _)
  "Close any focused `popper' popup.
Intended, but not necessarily for use as a hook."
  ;; (autoload 'popper-toggle "popper")
  (when (bound-and-true-p popper-popup-status)
    (popper-toggle)))

(provide 'lib-window)
;;; lib-window.el ends here
