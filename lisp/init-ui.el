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

(require 'elpaca-autoloads)

(require 'lib-common)
(require 'lib-hydras)

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

;;; Provide commonly-used interface libraries

;; Required as dependencies for many packages, either as more recent versions
;; than those available in Emacs (e.g. `transient 'IIRC), or, including some
;; (like `nix-mode') who don't seem to declare them.

(elpaca transient)

(elpaca magit-section)

(elpaca nerd-icons
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono")
  (require 'nerd-icons))

(use-package svg-lib
  :demand t)

(use-package hydra
  :commands defhydra)

;;; pretty-hydra :: <https://github.com/jerrypnz/major-mode-hydra.el/#pretty-hydra>

(use-package pretty-hydra
  :after (hydra))

;;
;;; Avy

;; <https://github.com/abo-abo/avy>

;; <https://karthinks.com/software/avy-can-do-anything/>

(use-package avy
  :commands ( avy-goto-char
              avy-goto-char-2
              avy-goto-char-timer)
  :init
  (keymap-global-set "M-j" #'avy-goto-char-2)
  (after! 'lispy
    (defvar lispy-mode-map)
    (declare-function lispy-join "lispy")
    (keymap-set lispy-mode-map "M-J" #'lispy-join))

  :config
  (setopt avy-all-windows t)
  (setopt avy-all-windows-alt t)
  ;; Prevent conflicts with themes.
  (setopt avy-background nil)
  (setopt avy-style 'at-full)
  ;; (setopt avy-style 'de-bruijn)
  (setopt avy-timeout-seconds 0.2))

(provide 'init-ui)
;;; init-ui.el ends here
