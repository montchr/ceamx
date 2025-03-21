;;; ceamx-editor.el --- Ceamx editor support library  -*- lexical-binding: t;  -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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
