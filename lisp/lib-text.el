;;; lib-text.el --- Text utilities                   -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
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

;;;; Sources:

;; <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/lisp/lib/text.el>

;;; Code:

;;;###autoload
(defvar ceamx-point-in-comment-functions ()
  "List of functions to run to determine if point is in a comment.

Each function takes one argument: the position of the point. Stops on the first
function to return non-nil.

Used by `ceamx-point-in-comment-p'.")

;;;###autoload
(defun ceamx-point-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.
POS defaults to the current position."
  (let ((pos (or pos (point))))
    (if ceamx-point-in-comment-functions
        (run-hook-with-args-until-success 'ceamx-point-in-comment-functions pos)
      (nth 4 (syntax-ppss pos)))))

(provide 'lib-text)
;;; lib-text.el ends here
