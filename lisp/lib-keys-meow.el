;;; lib-keys-meow.el --- Helpers for Meow            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

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

;;

;;; Code:

(require 'lib-common)

(defun ceamx-normalize-char (char)
  "Normalize CHAR to a valid character matching `characterp'.
CHAR may either be a valid character or a string convertable to a
character with `string-to-char'. If CHAR is already a character
matching `characterp', then it will be returned as-is.

When CHAR is a string containing more than one character, only
the first character will be transformed. See `string-to-char' for
more info.

This function is impure because the interpretation of CHAR can
vary based on... various reasons?"
  (declare (side-effect-free t))
  (cl-assert (char-or-string-p char) t)
  (if (stringp char)
    (cond ((length= char 0)
            (user-error "Character string `%s' is empty" char))
      ((length> char 1)
        (user-error "Character string `%s' should only contain a single character" char))
      (t
        (string-to-char char)))
    char))

(defmacro meow-pair! (thing char begin end)
  "Register a new Meow THING as a pair of BEGIN and END, and map it to CHAR.
This macro simplifies `meow-thing-register' by assuming that the
INNER and BOUNDS arguments of `meow-thing-register' will be an
identical pair expression. This macro also handles the additional
and necessary step of adding the newly-registered THING to
`meow-char-thing-table' as CHAR.

THING is a symbol for registering the new thing with
`meow-thing-register'. It may be quoted or unquoted.

BEGIN is a string delimiting the beginning or opening of the pair
while END is a string delimiting the end or closing of the pair.

The THING will be added to `meow-char-thing-table' as CHAR. CHAR
may either be a character constant matching `characterp' (e.g.
`?a' or `97'), or a string which can be converted to a character
with `string-to-char'. See Info node `(elisp) Basic Char Syntax)'
and Info node `(elisp) String Conversion' for more info."
  (declare (indent defun))
  (cl-assert (char-or-string-p char) t)
  (cl-assert (stringp begin) t)
  (cl-assert (stringp end) t)
  (let* ((sym (cmx-unquote thing))
          (char (ceamx-normalize-char char))
          (open (list begin))
          (close (list end))
          (pair `(pair ,open ,close)))
    (cl-assert (symbolp sym) t)
    `(progn
       (meow-thing-register ',sym
         ',pair
         ',pair)
       (ceamx-meow-bind-thing ',sym ,char))))

(defun ceamx-meow-bind-thing (thing char)
  "Add pre-registered THING to `meow-char-thing-table' as CHAR."
  (defvar meow-char-thing-table '())
  (let ((thing (cmx-unquote thing))
         (char (ceamx-normalize-char ,char)))
    (add-to-list 'meow-char-thing-table `(,char . ,thing))))

(defun ceamx-meow-unbind-thing (char)
  "Remove the character association for character CHAR.
This function will destructively modify the alist
`meow-char-thing-table' by removing the association whose key
matches CHAR.

Note that despite its name, `meow-char-thing-table' is an alist,
not a character table.

CHAR may be a string or character constant, which will be passed
as the CHAR argument to `ceamx-normalize-char'."
  (defvar meow-char-thing-table '())
  (assoc-delete-all (ceamx-normalize-char char) meow-char-thing-table #'eq))

(provide 'lib-keys-meow)
;;; lib-keys-meow.el ends here