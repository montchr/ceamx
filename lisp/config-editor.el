;;; config-editor.el --- User options for editing  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@protonmail.com>

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

(defcustom ceamx-format-on-save-disabled-modes
  '(emacs-lisp-mode                     ; conflict with `lispy' indent
    org-msg-edit-mode)
  "A list of major modes in which to not reformat the buffer upon saving.
When nil, buffers will always be formatted upon save. When
non-nil, buffers will never be formatted upon save."
  :group 'ceamx
  :type '(choice boolean (repeat symbol)))
;; As of <2024-05-24 Fri>
;; <https://biomejs.dev/internals/language-support/>
(defconst ceamx-editor-format-biome-modes-list
  '(javascript-mode js-mode js-ts-mode js3-mode
    typescript-mode typescript-ts-mode
    js-jsx-mode tsx-ts-mode
    json-mode json-ts-mode)
  "List of major-mode symbols for the languages supported by the Biome formatter.")
(defcustom ceamx-structured-editing-style 'lispy
  "The structured editing provider."
  :group 'ceamx
  :type '(choice :tag "Structured editing style" :value lispy
          (const :tag "Lispy" lispy)
          (const :tag "Puni" puni)))

(provide 'config-editor)
;;; config-editor.el ends here
