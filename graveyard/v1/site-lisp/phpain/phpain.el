;;; phpain.el --- Painful Help Perhaps -- PHP Hell Potentialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
;; Keywords: languages, local, convenience

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

;; Work-in-progress helpers for PHP editing.

;;; Code:

;; FIXME:
(defun ceamx/mark-line ()
  "Select the current line."
  (interactive)
  (push-mark (line-beginning-position))
  (end-of-line))

(defun ceamx/comment-line ()
  "Comment the current line."
  (interactive)
  (ceamx/mark-line)
  (comment-region (region-beginning) (region-end)))

(defun ceamx/uncomment-line ()
  "Uncomment the current line."
  (interactive)
  (ceamx/mark-line)
  (uncomment-region (region-beginning) (region-end)))

(defun php-single-line-comment-to-phpdoc ()
  "TODO"
  (interactive)

  (kill-whole-line)
  (beginning-of-line)
  (yas-expand-snippet
   (yas-lookup-snippet "doc-comment-multiline" php-mode))
  (yank)
  (eglot-code-action-quickfix (point)))


(provide 'phpain)
;;; phpain.el ends here
