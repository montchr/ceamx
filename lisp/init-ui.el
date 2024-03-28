;;; init-ui.el --- General user interface customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

;; For user interface things that are shared by many other things.

;;; Code:

(require 'lib-common)

;;; General

;; Modal keybinding systems will change the cursor dynamically to indicate current state.
;; This default value matches what I expect in an "insert" mode.
(setq-default cursor-type 'bar)

;; Enable cursor blinking.
(blink-cursor-mode 1)

;; Seeing a cursor in a window other than the active window is pretty confusing.
(setq-default cursor-in-non-selected-windows nil)

;; Improve visual contrast between focused/non-focused windows.
(setopt highlight-nonselected-windows nil)

;;; Customization buffer and menu interface

(setopt custom-theme-allow-multiple-selections nil)

(setopt custom-unlispify-menu-entries nil)
(setopt custom-unlispify-tag-names nil)
(setopt custom-unlispify-remove-prefixes nil)

;;; Provide commonly-used interface libraries

;; Required as dependencies for many packages, either as more recent versions
;; than those available in Emacs (e.g. `transient 'IIRC), or, including some
;; (like `nix-mode') who don't seem to declare them.

(package! transient)

(with-eval-after-load 'transient
  (defvar transient-map)
  (declare-function transient-quit-one "transient")

  ;; Always close transient with ESC
  ;; FIXME: meow overrides this. waiting until it loads does not help.
  (keymap-set transient-map "ESC" #'transient-quit-one))

(package! magit-section)

(package! nerd-icons
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono")
  (require 'nerd-icons))

(package! svg-lib)

(package! hydra)

;;; ~pretty-hydra~

;; <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra>

(package! pretty-hydra)

;;; ~avy~

;; <https://github.com/abo-abo/avy>

;; <https://karthinks.com/software/avy-can-do-anything/>

(package! avy
  (setopt avy-all-windows t)
  (setopt avy-all-windows-alt t)
  ;; Prevent conflicts with themes.
  (setopt avy-background nil)
  (setopt avy-style 'at-full)
  ;; (setopt avy-style 'de-bruijn)
  (setopt avy-timeout-seconds 0.2)

  (keymap-global-set "M-j" #'avy-goto-char-2)

  (after! lispy
    (defvar lispy-mode-map)
    (declare-function lispy-join "lispy")
    ;; Prevent conflict with newly-added M-j binding for `avy-goto-char-2'.
    (keymap-set lispy-mode-map "M-J" #'lispy-join)))

;;; ~page-break-lines~: improve appearance of form feed characters

;; <https://github.com/purcell/page-break-lines/blob/master/README.md>

(package! page-break-lines
  (global-page-break-lines-mode))

(provide 'init-ui)
;;; init-ui.el ends here
