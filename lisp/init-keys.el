;;; init-keys.el --- Keybindings -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Keybindings configuration.

;; TODO: <https://github.com/jwiegley/dot-emacs/blob/master/init.org#smart-newline>
;; TODO: <https://github.com/ainame/smart-newline.el/tree/c50ab035839b307c66d439083b6761cb7db5e972>

;;; Code:

;;; Requirements

(require 'config-env)
(require 'lib-common)

;;; Bind some CUA-like hotkeys for editing operations

;; "C-S" prefix is inspired by the use of this mod combo in terminal emulators,
;; where "C-c" for example would kill the current process.

;; FIXME: move to bindings file

(define-keymap :keymap (current-global-map)
  "C-S-c" #'kill-ring-save
  "C-S-v" #'yank
  "C-S-x" #'kill-region)

;;; macOS: Remap modifier keys for the Apple keyboard layout

(when (and +sys-mac-p (display-graphic-p))
  (setopt mac-control-modifier 'control)
  (setopt mac-option-modifier 'meta)
  (setopt ns-option-modifier 'meta)
  (setopt mac-command-modifier 'super)
  (setopt ns-command-modifier 'super)
  ;; Free up the right-side option key for character composition.
  (setopt mac-right-option-modifier 'none)
  (setopt ns-right-option-modifier 'none)
  ;; Common system hotkeys.
  (define-keymap :keymap (current-global-map)
    "s-c" #'kill-ring-save
    "s-v" #'yank
    "s-x" #'kill-region
    "s-q" #'save-buffers-kill-emacs))

;;; Enable and configure `repeat-mode'

(setopt repeat-exit-key "ESC")
(setopt repeat-exit-timeout 5)
(setopt repeat-on-final-keystroke t)
(setopt repeat-keep-prefix nil)

;; Related, but not technically part of `repeat-mode'.
(setopt set-mark-command-repeat-pop t)

;; Avoid running mode-hooks too early.
(add-hook 'ceamx-after-init-hook #'repeat-mode)

;;; Show free keybindings for modkeys or prefixes with `free-keys'

;; <https://github.com/Fuco1/free-keys>

;; > If called with prefix argument C-u, you can specify a prefix map to be
;; > used, such as C-c or C-c C-x (these are specified as a string).

(package! free-keys)

;;; Expand the existing repeat-map for outline navigation

(after! [repeat outline]
  (define-keymap :keymap outline-navigation-repeat-map
    "C-x" #'foldout-exit-fold
    "x" #'foldout-exit-fold
    "C-z" #'foldout-zoom-subtree
    "z" #'foldout-zoom-subtree
    "C-a" #'outline-show-all
    "a" #'outline-show-all
    "C-c" #'outline-hide-entry
    "c" #'outline-hide-entry
    "C-d" #'outline-hide-subtree
    "C-e" #'outline-show-entry
    "e" #'outline-show-entry
    "TAB" #'outline-show-children
    "C-k" #'outline-show-branches
    "k" #'outline-show-branches
    "C-l" #'outline-hide-leaves
    "l" #'outline-hide-leaves
    "RET" #'outline-insert-heading
    "C-o" #'outline-hide-other
    "o" #'outline-hide-other
    "C-q" #'outline-hide-sublevels
    "q" #'outline-hide-sublevels
    "C-s" #'outline-show-subtree
    "s" #'outline-show-subtree
    "C-t" #'outline-hide-body
    "t" #'outline-hide-body
    "@" #'outline-mark-subtree)

  (ceamx-repeatify-keymap 'outline-navigation-repeat-map))

(provide 'init-keys)
;;; init-keys.el ends here
