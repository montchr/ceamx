;;; init-keys-which-key.el --- Support for which-key  -*- lexical-binding: t; -*-

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

(use-package which-key
  :blackout t
  :commands ( which-key-mode
              which-key-setup-side-window-right-bottom)

  :config
  ;; Determine whether keys have been rebound, considering the active keymaps.
  ;; NOTE: Does not seem to work reliably -- see Commentary section above.
  (setopt which-key-compute-remaps t)
  (setopt which-key-idle-delay 1.00)

  ;; Sort non-prefix-keys above prefix keys.
  (setopt which-key-sort-order 'which-key-prefix-then-key-order)

  (setopt which-key-sort-uppercase-first nil))

(provide 'init-keys-which-key)
;;; init-keys-which-key.el ends here