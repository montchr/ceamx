;;; init-keys-reference.el --- Support for keybindings help, cheatsheets, and other references  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local, help

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

;; This configuration file is intended to be loaded after every keybinding is in
;; place, in an effort to help these packages reference the final state of all
;; keybindings.

;; For example, `which-key' does not seem to like the way that `meow' handles
;; keybindings -- see the documentation for
;; `meow-keypad-describe-keymap-function' -- displaying only
;; `which-key-prefix-prefix'. `general.el' behaves similarly, from what I can
;; recall. I suspect this has something to do with key translations, a subject
;; with which I am currently unfamiliar.

;;; Code:

;;
;;; which-key
;;

;; (use-package which-key
;;   :demand t
;;   :blackout
;;   :commands ( which-key-mode
;;               which-key-setup-side-window-right-bottom)

;;   :config
;;   (setopt which-key-add-column-padding 1)
;;   ;; Determine whether keys have been rebound, considering the active keymaps.
;;   ;; NOTE: Does not seem to work reliably -- see Commentary section above.
;;   ;; (setopt which-key-compute-remaps t)
;;   (setopt which-key-idle-delay 1.00)
;;   (setopt which-key-prefix-prefix "")
;;   (setopt which-key-separator " ")
;;   (setopt which-key-side-window-max-width 0.33)
;;   ;; Sort non-prefix-keys above prefix keys.
;;   (setopt which-key-sort-order 'which-key-prefix-then-key-order)
;;   (setopt which-key-sort-uppercase-first nil)

;;   (which-key-setup-side-window-bottom)

;;   (which-key-mode +1))

(provide 'init-keys-reference)
;;; init-keys-reference.el ends here
