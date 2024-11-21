;;; init-keys.el --- Keybindings  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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

;; Requirements


;; [[file:../config.org::*Requirements][Requirements:1]]
(require 'config-env)
(require 'ceamx-lib)
(require 'lib-keys)
;; Requirements:1 ends here

;; Bind some CUA-like hotkeys for editing operations

;; "C-S" prefix is inspired by the use of this mod combo in terminal emulators,
;; where "C-c" for example would kill the current process.


;; [[file:../config.org::*Bind some CUA-like hotkeys for editing operations][Bind some CUA-like hotkeys for editing operations:1]]
(define-keymap :keymap (current-global-map)
  "C-S-c" #'kill-ring-save
  "C-S-v" #'yank
  "C-S-x" #'kill-region)
;; Bind some CUA-like hotkeys for editing operations:1 ends here

;; macOS: Remap modifier keys for the Apple keyboard layout


;; [[file:../config.org::*macOS: Remap modifier keys for the Apple keyboard layout][macOS: Remap modifier keys for the Apple keyboard layout:1]]
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
;; macOS: Remap modifier keys for the Apple keyboard layout:1 ends here

;; Enable and configure ~repeat-mode~


;; [[file:../config.org::*Enable and configure ~repeat-mode~][Enable and configure ~repeat-mode~:1]]
(setopt repeat-exit-timeout 15)
(setopt repeat-on-final-keystroke t)
(setopt repeat-keep-prefix nil)
;; Enable and configure ~repeat-mode~:1 ends here



;; Allow any key sequence to exit ~repeat-mode~:


;; [[file:../config.org::*Enable and configure ~repeat-mode~][Enable and configure ~repeat-mode~:2]]
(setopt repeat-exit-key nil)
;; Enable and configure ~repeat-mode~:2 ends here



;; Related, but not technically part of ~repeat-mode~:


;; [[file:../config.org::*Enable and configure ~repeat-mode~][Enable and configure ~repeat-mode~:3]]
(setopt set-mark-command-repeat-pop t)
;; Enable and configure ~repeat-mode~:3 ends here



;; Enable ~repeat-mode~, avoiding running mode-hooks too early:


;; [[file:../config.org::*Enable and configure ~repeat-mode~][Enable and configure ~repeat-mode~:4]]
(add-hook 'ceamx-after-init-hook #'repeat-mode)
;; Enable and configure ~repeat-mode~:4 ends here

;; ~free-keys~: Show free keybindings for modkeys or prefixes :package:

;; - pkgsrc :: <https://github.com/Fuco1/free-keys>


;; [[file:../config.org::*~free-keys~: Show free keybindings for modkeys or prefixes][~free-keys~: Show free keybindings for modkeys or prefixes:1]]
;;

;; > If called with prefix argument C-u, you can specify a prefix map to be
;; > used, such as C-c or C-c C-x (these are specified as a string).

(package! free-keys)
;; ~free-keys~: Show free keybindings for modkeys or prefixes:1 ends here

(provide 'init-keys)
;;; init-keys.el ends here
