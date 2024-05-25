;;; lib-editor.el --- Editor support library  -*- lexical-binding: t;  -*-

;; Copyright (C) 2023-2024  Chris Montgomery
;; Copyright (C) 2016–2022  Radian LLC and contributors

;; Author: Chris Montgomery <chmont@proton.me>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; Author: Chris Montgomery <chmont@proton.me>
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

(defun ceamx/continue-comment ()
  "Continue current comment, preserving trailing whitespace.
This differs from `default-indent-new-line' in the following way:

If you have a comment like \";; Some text\" with point at the end
of the line, then running `default-indent-new-line' will get you
a new line with \";; \", but running it again will get you a line
with only \";;\" (no trailing whitespace). This is annoying for
inserting a new paragraph in a comment. With this command, the
two inserted lines are the same."
  (interactive)
  ;; `default-indent-new-line' uses `delete-horizontal-space'
  ;; because in auto-filling we want to avoid the space character at
  ;; the end of the line from being put at the beginning of the next
  ;; line. But when continuing a comment it's not desired.
  (cl-letf (((symbol-function #'delete-horizontal-space) #'ignore))
    (default-indent-new-line)))
(defun ceamx-editor-format-maybe-inhibit-h ()
  "Check if formatting should be disabled for current buffer."
  (or (eq major-mode 'fundamental-mode)
      (string-blank-p (buffer-name))
      (eq ceamx-format-on-save-disabled-modes t)
      (not (null (memq major-mode ceamx-format-on-save-disabled-modes)))))

(provide 'lib-editor)
;;; lib-editor.el ends here
