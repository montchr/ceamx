;;; lib-editor.el --- Editor support library  -*- lexical-binding: t;  -*-

;; Copyright (C) 2023-2024  Chris Montgomery
;; Copyright (C) 2016–2022  Radian LLC and contributors

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; Author: Chris Montgomery <chmont@protonmail.com>
;;         Radon Rosborough <radon@intuitiveexplanations.com>
;; Keywords: local


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

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Commentary:
;;; Code:

(defun ceamx/backward-kill-word ()
  "Kill the previous word, smartly.
This operation will respect the following rules:

1. If the cursor is at the beginning of line, delete the '\n'.
2. If there is *only* whitespace, delete only to beginning of line.
3. If there is *some* whitespace, delete whitespace and check 4-5.
4. If there are other characters instead of words, delete one only char.
5. If it's a word at point, delete it."
  (interactive)
  (if (bolp)
      ;; 1
      (delete-char -1)
    (if (string-match-p "^[[:space:]]+$"
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))
        ;; 2
        (delete-horizontal-space)
      (when (thing-at-point 'whitespace)
        ;; 3
        (delete-horizontal-space))

      (if (thing-at-point 'word)
          ;; 5
          (let ((start (car (bounds-of-thing-at-point 'word)))
                (end (point)))
            (if (> end start)
                (delete-region start end)
              (delete-char -1)))
        ;; 4
        (delete-char -1)))))
(defun ceamx/cycle-string-inflection ()
  "Cycle through `string-inflection' styles appropriate to the major-mode."
  (interactive)
  (pcase major-mode
    (`emacs-lisp-mode (string-inflection-all-cycle))
    (`python-mode (string-inflection-python-style-cycle))
    (`java-mode (string-inflection-java-style-cycle))
    (`elixir-mode (string-inflection-elixir-style-cycle))
    (_ (string-inflection-ruby-style-cycle))))

(provide 'lib-editor)
;;; lib-editor.el ends here
