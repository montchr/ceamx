;;; init-input-methods.el --- Configuration for input methods  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
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

;;

;;; Code:

;;
;;; Language environment

(set-language-environment "UTF-8")

;; `set-language-environment' also presumptively sets `default-input-method'.
(setopt default-input-method nil)

;; Disable bidirectional text scanning, because I don't need it.
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;;
;;; Mouse input

(setopt mouse-yank-at-point t)

;; Avoid collision of mouse with point.
(mouse-avoidance-mode 'exile)

;; "More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling."
;;(setopt fast-but-imprecise-scrolling nil)

(setopt use-file-dialog nil)
(setopt use-dialog-box nil)

(provide 'init-input-methods)
;;; init-input-methods.el ends here
