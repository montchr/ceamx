;;; ceamx-editor.el --- Ceamx editor support library  -*- lexical-binding: t;  -*-

;; Copyright (C) 2023-2025  Chris Montgomery

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
;;; Code:

(defgroup ceamx-editor nil
  "Ceamx editor support"
  :group 'ceamx)

;;;; Customization

(defcustom ceamx-editor-biome-supported-modes-list
  '( css-base-mode js-base-mode typescript-ts-base-mode
     json-mode json-ts-mode)
  "List of major modes for the languages supported by the Biome formatter.
Refer to <https://biomejs.dev/internals/language-support/> for Biome's
currently-supported languages.

The default value for this setting was updated from the Biome
documentation as of <2025-07-11 Fri>."
  :type '(repeat symbol)
  :group 'ceamx-editor)

;;;; Functions

;;;###autoload
(defun ceamx-editor-biome-supported-modes-hooks ()
  "List of mode hooks for the modes in `ceamx-editor-biome-supported-modes-list'."
  (mapcar (lambda (mode)
            (intern (concat (symbol-name mode))))
    ceamx-editor-biome-supported-modes-list))

;;;; Commands

;;;###autoload
(defun ceamx/cycle-string-inflection ()
  "Cycle through `string-inflection' styles appropriate to the major-mode."
  (interactive)
  (pcase major-mode
    (`emacs-lisp-mode (string-inflection-all-cycle))
    (`python-mode (string-inflection-python-style-cycle))
    (`java-mode (string-inflection-java-style-cycle))
    (`elixir-mode (string-inflection-elixir-style-cycle))
    (_ (string-inflection-ruby-style-cycle))))

(provide 'ceamx-editor)
;;; ceamx-editor.el ends here
