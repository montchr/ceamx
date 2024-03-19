;;; lib-keys.el --- Keybinding helpers               -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

;; FIXME: add copyright+license

;; Helper functions and macros for keybindings.

;; <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/lisp/doom-keybinds.el#L93C1-L109C56>
;; <https://github.com/casouri/lunarymacs/blob/cd1f34449038e5ec371b1277941c529ea1fb4e9e/site-lisp/luna-key.el>

;; TODO: add utility to apply prefix to provided keys and bind in maps,
;; deferring until maps are defined if necessary. see
;; <https://github.com/casouri/lunarymacs/blob/master/site-lisp/luna-key.el>

;;; Code:

(require 'cl-lib)

(require 'config-keys)

;; NOTE: all in-use definitions have been moved to `lib-common'
(require 'lib-common)

;; FIXME: interface still doesn't feel right...
;; TODO: account for remapping (don't prefix)
;; (defun ceamx--key-with-prefix (prefix key &optional sep)
;;   "Return a key sequence string with PREFIX prepended to KEY.
;; The arguments PREFIX and KEY must be strings satisfying `key-valid-p'.

;; The optional argument SEP may specify a separator between PREFIX
;; and KEY. If nil, a single whitespace character will be used as
;; the default separator."
;;   (cl-assert (key-valid-p prefix))
;;   (cl-assert (key-valid-p key))
;;   (let* ((sep (or sep " "))
;;           (prefixed-key (concat prefix sep key)))
;;     (cl-assert (key-valid-p prefixed-key))
;;     prefixed-key))

;; FIXME: while logging something simple works, this does not yet return
;; anything useful -- it should return a modified version of DEFS
;; TODO: rework since this doesn't need to be about keydefs
;; (defun ceamx-map-keydefs (func &rest defs)
;;   "Apply the function FUNC to key/definition pairs DEFS.

;; FUNC should accept args KEY and DEF. KEY should be a string
;; matching `key-valid-p'. DEF is anything that can be a key
;; definition. See the docstring for `keymap-set' for more info on
;; accepted values for DEF.

;; This function does not validate the validity of each KEY/DEF pair
;; in DEFS. However, this function will throw when there is an
;; uneven number of items in DEFS, or when there are duplicate
;; definitions for KEY.

;; This function is mostly copied from the source of
;; `define-keymap'."
;;   (let (seen-keys)
;;     (while defs
;;       (let ((key (pop defs)))
;;         (unless defs
;;           (error "Uneven number of key/definition pairs"))
;;         (let ((def (pop defs)))
;;           (if (member key seen-keys)
;;               (error "Duplicate definition for key: %S" key)
;;             (push key seen-keys))
;;           (funcall func key def))))))

;; (defmacro with-prefix! (prefix &rest defs)
;;   "Prepend string PREFIX to each key in key/definition pairs DEFS.
;; PREFIX must satisfy `key-valid-p'. DEFS is composed of keybinding
;;  pairs of the form accepted by `define-keymap'.

;; When PREFIX is prepended to each key string, a single space will
;;  be added between PREFIX and the value for the key. If PREFIX
;;  already includes trailing whitespace, that whitespace will be
;;  stripped and replaced by a single space unless the trailing
;;  whitespace is already a single space character."
;;   (declare (indent defun))
;;   (let* ((prefix (string-clean-whitespace prefix))
;;          ;; FIXME: how to return modified KEY and DEF? or find another way...
;;          (fn (lambda (key def) (format "%s %s" prefix key))))))

(provide 'lib-keys)
;;; lib-keys.el ends here
