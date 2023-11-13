;;; lib-keys-meow.el --- Meow helper functions and macros  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(defvar meow-keymap-alist)

;; TODO: should probably be macro
(defun cmx-meow-define-keys (state &rest keybinds)
  "Define KEYBINDS in STATE.

Adapted from `meow-define-keys' for an interface similar to `defvar-keymap'.

Example usage:
  (cmx-meow-define-keys
    \\'normal
    \"a\" #\\'meow-append"
  (declare (indent defun))
  (let ((map (alist-get state meow-keymap-alist)))
    (apply 'define-keymap :keymap map keybinds)))

(defalias 'cmx-meow-normal-define-key
  (apply-partially 'cmx-meow-define-keys 'normal))


(provide 'lib-keys-meow)
;;; lib-keys-meow.el ends here
