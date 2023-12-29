;;; init-keys.el --- Keybindings -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

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

;; HACK: Emacs cannot distinguish between C-i from TAB. This is largely a
;;   byproduct of its history in the terminal, which can't distinguish them
;;   either, however, when GUIs came about Emacs greated separate input events
;;   for more contentious keys like TAB and RET. Therefore [return] != RET,
;;   [tab] != TAB, and [backspace] != DEL.
;;
;;   In the same vein, this keybind adds a [C-i] event, so users can bind to it.
;;   Otherwise, it falls back to regular C-i keybinds.
;;
;; Source: <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/lisp/doom-keybinds.el#L47-L67>
;; Further reading: <https://emacs.stackexchange.com/questions/17509/how-to-distinguish-c-i-from-tab>
(define-key
 key-translation-map
 [?\C-i]
 (cmd! (if (let ((keys (this-single-command-raw-keys)))
             (and keys
                  (not (cl-position 'tab    keys))
                  (not (cl-position 'kp-tab keys))
                  (display-graphic-p)
                  ;; Fall back if no <C-i> keybind can be found, otherwise
                  ;; we've broken all pre-existing C-i keybinds.
                  (let ((key (cmx-lookup-key
                              (vconcat (cl-subseq keys 0 -1) [C-i]))))
                    (not (or (numberp key) (null key))))))
           [C-i] [?\C-i])))

(with-eval-after-load 'eldoc
  (eldoc-add-command 'cmx/escape))




(provide 'init-keys)
;;; init-keys.el ends here
