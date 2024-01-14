;;; lib-keys.el --- Keybinding helpers               -*- lexical-binding: t; -*-

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

;; FIXME: add copyright+license

;; Helper functions and macros for keybindings.

;; <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/lisp/doom-keybinds.el#L93C1-L109C56>
;; <https://github.com/casouri/lunarymacs/blob/cd1f34449038e5ec371b1277941c529ea1fb4e9e/site-lisp/luna-key.el>

;;; Code:

(require 'cl-lib)

(require 'config-keys)

;;
;;; Keybindings

;; TODO: add utility to apply prefix to provided keys and bind in maps,
;; deferring until maps are defined if necessary. see
;; <https://github.com/casouri/lunarymacs/blob/master/site-lisp/luna-key.el>

;; TODO: account for remapping (don't prefix)
;; TODO: validate final result
(defun cmx-key--normalize-with-prefix (prefix key)
  "Produce a normalized key sequence from the concatenation of PREFIX and KEY."
  (cl-assert (key-valid-p prefix))
  (cl-assert (key-valid-p key))
  (format "%s %s" prefix key))

(defmacro def-map! (command &rest defs)
  "Define a new keymap and prefix COMMAND composed of keybindings DEFS."
  (declare (indent (defun)))
  `(progn
     (unless (commandp (quote ,command))
       (define-prefix-command (quote ,command)))
     (define-keymap
       :keymap ,command
       ,@defs)))

(defmacro leader-key! (key def)
  "Bind DEF to KEY in the leader map.
If `meow-leader-define-key' is available, then that function will
handle the binding. Otherwise, binding will be handled with
`keymap-set' into `mode-specific-map'.

Note that as of writing, `meow' bindings do not seem to support
descriptions in a way that is comprehensible to `which-key' --
the documentation for `meow-keypad-describe-keymap-function'
acknowledges this incompatibility."
  (declare (indent defun))
  `(if (fboundp 'meow-leader-define-key)
      (meow-leader-define-key (cons ,key ,def))
    (keymap-set mode-specific-map ,key ,def)))

(defmacro def-arm! (keymap key description &rest defs)
  "Define KEYMAP with DEFS bound to KEY with DESCRIPTION in the appropriate leader.
See `leader-key!' for more info about leader behavior."
  (declare (indent defun))
  `(progn
     (define-prefix-command (quote ,keymap))
     (define-keymap :keymap ,keymap ,@defs)
     (leader-key! ,key '(,description . ,keymap))))

;; TODO: not yet practical or functional
;; (defmacro def-mode-arm! (mode description &rest defs)
;;   "Define the mode-specific leader arm for MODE with DESCRIPTION and bindings DEFS."
;;   (declare (doc-string 2)
;;            (indent defun))
;;   (progn
;;     (let* ((mode-name (symbol-name mode))
;;            (keymap-sym (intern (format "cmx-%s-specific-map" mode-name)))
;;            (description description)
;;            (defs (or defs nil)))
;;       `(def-arm! ,keymap-sym "m"
;;          ,description
;;          ,@defs))))

(provide 'lib-keys)
;;; lib-keys.el ends here
