;;; ceamx-lib.el --- Ceamx :: Core Library           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'cl-lib)

;;;; Filesystem

(defun ceamx-subdirs (parent-dir)
  "Return every non-hidden subdirectory of PARENT-DIR."
  (cl-remove-if-not
   #'file-directory-p
   (directory-files
    (expand-file-name parent-dir) t "^[^\\.]")))

;;;; General

(defmacro setq! (&rest settings)
  "A more sensible `setopt' for setting customizable variables.

This can be used as a drop-in replacement for `setq' and *should* be
used instead of `setopt'.  Unlike `setq', this triggers custom setters
on variables.  Unlike `setopt', this won't needlessly pull in
dependencies."
  (declare (debug setq))
  (macroexp-progn
    (cl-loop for (var val) on settings by 'cddr
      collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                 ',var ,val))))

(cl-defmacro appendnew! (item place &rest keys)
  "Append ITEM to PLACE if it's not already present.
Accepts the same keyword arguments as `cl-pushnew', which see."
  `(unless (cl-member ,item ,place ,@keys)
     (setf ,place (append ,place (list ,item)))))

(provide 'ceamx-lib)
;;; ceamx-lib.el ends here
