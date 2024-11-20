;;; config-common.el --- Common and miscellaneous configuration settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery
;; Copyright (C) 2020-2023  Protesilaos Stavrou

;; Author: Chris Montgomery <chmont@proton.me>
;;         Protesilaos Stavrou <info@protesilaos.com>
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

;;; Sources:

;; <https://github.com/protesilaos/dotfiles/blob/df9834d8db815920bfd7aacfaf11ef16fa089c53/emacs/.emacs.d/prot-lisp/prot-common.el>

;;; Code:

;; via <https://github.com/protesilaos/dotfiles/blob/df9834d8db815920bfd7aacfaf11ef16fa089c53/emacs/.emacs.d/prot-lisp/prot-common.el>
(defconst ceamx-common-url-regexp
  (concat
    "~?\\<\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]*\\)"
    "[.@]"
    "\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]+\\)\\>/?")
  "Regular expression to match (most?) URLs or email addresses.")

(provide 'config-common)
;;; config-common.el ends here
