;;; init-keys.el --- Keybindings  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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
;;; Code:

(require 'config-env)
(require 'ceamx-lib)
(require 'lib-keys)
(define-keymap :keymap (current-global-map)
  "C-S-c" #'kill-ring-save
  "C-S-v" #'yank
  "C-S-x" #'kill-region)
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
(setopt repeat-exit-timeout 15)
(setopt repeat-on-final-keystroke t)
(setopt repeat-keep-prefix nil)
(setopt repeat-exit-key nil)
(setopt set-mark-command-repeat-pop t)
(add-hook 'ceamx-after-init-hook #'repeat-mode)
;;

;; > If called with prefix argument C-u, you can specify a prefix map to be
;; > used, such as C-c or C-c C-x (these are specified as a string).

(package! free-keys)

(provide 'init-keys)
;;; init-keys.el ends here
