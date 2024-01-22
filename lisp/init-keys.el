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

(require 'lib-common)
(require 'lib-keys)

(autoload 'burly-bookmark-frames "burly")
(autoload 'burly-bookmark-windows "burly")
(autoload 'vundo "vundo")

(autoload 'cmx-hydra/main/body "init-hydras")

(defvar +sys-mac-p)

;; Common system hotkeys, complicated for cross-platform usability.
;;
;; "C-S" prefix is inspired by the use of this mod combo in terminals, where
;; `C-c' for example would kill the current process.
(define-keymap :keymap (current-global-map)
  "C-S-c" #'kill-ring-save
  "C-S-v" #'yank
  "C-S-x" #'kill-region)

;; macOS: Remap modifier keys.
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

(use-feature! repeat
  :config
  ;; TODO: make sure this doesn't have some unintended consequences
  (setopt repeat-exit-key "ESC")
  (repeat-mode 1))

(provide 'init-keys)
;;; init-keys.el ends here
