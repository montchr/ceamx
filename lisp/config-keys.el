;;; config-keys.el --- Keybindings variables         -*- lexical-binding: t; -*-

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

;; Keybindings something something.

;; Q: Is it safe to `defvar-keymap' here since `config-*' files should be idempotent?
;;
;; A: Yes. For the same reason that it is safe to invoke `defvar' for the same
;;    symbol multiple times in order to, for example, appease the byte-compiler.
;;
;;    I dug into the C source code for `defvar' and found these notes:
;;
;;    > A simple (defvar foo) with lexical scoping does "nothing" except
;;    > declare that var to be dynamically scoped *locally* (i.e. within
;;    > the current file or let-block).
;;
;;    > Simple (defvar <var>) should not count as a definition at all.
;;    > It could get in the way of other definitions, and unloading this
;;    > package could try to make the variable unbound.

;;; Code:

;;
;;; Options
;;

(defcustom ceamx-keybinding-scheme 'meow
  "Keybinding scheme.
Valid values are the symbols `evil', `meow', and `emacs',
corresponding to the `evil-mode' modal vim-emulation scheme, the
`meow' modal scheme, or the Emacs default keybinding scheme.

A nil value, in addition to `emacs', also means retain
vanilla Emacs keybindings."
  :group 'ceamx
  :type '(choice :tag "Keybinding scheme to use." :value meow
           (const :tag "The `evil' vim-like modal keybinding scheme" evil)
           (const :tag "The `meow' kakoune- and Emacs-like modal keybinding scheme" meow)
           (const :tag "The vanilla Emacs keybinding scheme" emacs)
           (const :tag "Do not load an additional keybinding scheme" nil)))

(defcustom cmx-leader-key "SPC"
  "Leader prefix key sequence for use in modal schemes."
  :type '(key)
  :group 'ceamx)

(defcustom cmx-leader-alt-key "M-SPC"
  "Alternative leader prefix key sequence.
Note that the default value conflicts with a keybinding in the
GNOME desktop environment. This conflict must be resolved outside
of Emacs."
  :type '(key)
  :group 'ceamx)

(defcustom cmx-mode-specific-arm-key "m"
  "Key sequence for major-mode-specific leader arm maps, relative to leader."
  :type '(key)
  :group 'ceamx)

;;
;;; Keymaps
;;

;;; Interceptions

;; FIXME: Mode removed, delete.
(defvar-keymap cmx-intercept-mode-map
  :doc "High-precedence user-defined keymap.")

;;; Leader

(unless (commandp 'cmx-leader-map)
  (define-prefix-command 'cmx-leader-map))
(defvar-keymap cmx-leader-map
  :doc "User-defined leader keymap."
  :parent mode-specific-map)

(provide 'config-keys)
;;; config-keys.el ends here
