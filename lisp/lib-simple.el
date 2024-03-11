;;; lib-simple.el --- Common utility commands        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery
;; Copyright (C) 2020-2023  Protesilaos Stavrou
;; Copyright (c) 2023  Bruno Boal <egomet@bboal.com>

;; Author: Chris Montgomery <chris@cdom.io>
;;         Protesilaos Stavrou <info@protesilaos.com>
;;         Bruno Boal <egomet@bboal.com>
;; Keywords: local, convenience

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

;; <https://github.com/protesilaos/dotfiles/blob/df9834d8db815920bfd7aacfaf11ef16fa089c53/emacs/.emacs.d/ceamx-lisp/ceamx-simple.el>
;; <https://github.com/BBoal/emacs-config/blob/95520648c5f2ed0784d42e98afff035a6964fd2f/bb-lisp/bb-simple.el>

;;; Code:

;;; Requirements

(require 'config-common)

;;; Variables

(defgroup ceamx-simple ()
  "Generic utilities for editing."
  :group 'editing)

(defcustom ceamx-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `ceamx/insert-date'."
  :type 'string
  :group 'ceamx-simple)

(defcustom ceamx-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `ceamx/insert-date'."
  :type 'string
  :group 'ceamx-simple)

;;; Commands

;;;###autoload
(defun ceamx/insert-date (&optional arg)
  "Insert the current date as `ceamx-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `ceamx-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date ceamx-simple-date-specifier)
          (time ceamx-simple-time-specifier)
          (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

(defun ceamx-simple--pos-url-on-line (char)
  "Return position of `ceamx-common-url-regexp' at CHAR."
  (when (integer-or-marker-p char)
    (save-excursion
      (goto-char char)
      (re-search-forward ceamx-common-url-regexp (line-end-position) :noerror))))

;;;###autoload
(defun ceamx/escape-url-line (char)
  "Escape all URLs or email addresses on the current line.
When called from Lisp CHAR is a buffer position to operate from
until the end of the line.  In interactive use, CHAR corresponds
to `line-beginning-position'."
  (interactive
   (list
    (if current-prefix-arg
        (re-search-forward
         ceamx-common-url-regexp
         (line-end-position) :no-error
         (prefix-numeric-value current-prefix-arg))
      (line-beginning-position))))
  (when-let ((regexp-end (ceamx-simple--pos-url-on-line char)))
    (goto-char regexp-end)
    (unless (looking-at ">")
      (insert ">")
      (when (search-backward "\s" (line-beginning-position) :noerror)
        (forward-char 1))
      (insert "<"))
    (ceamx/escape-url-line (1+ regexp-end)))
  (goto-char (line-end-position)))

;;;###autoload
(defun ceamx/escape-url-region (&optional beg end)
  "Apply `ceamx/escape-url-line' on region lines between BEG and END."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (error "There is no region!")))
  (let ((beg (min beg end))
        (end (max beg end)))
    (save-excursion
      (goto-char beg)
      (setq beg (line-beginning-position))
      (while (<= beg end)
        (ceamx/escape-url-line beg)
        (beginning-of-line 2)
        (setq beg (point))))))

;;;###autoload
(defun ceamx/escape-url-dwim ()
  "Escape URL on the current line or lines implied by the active region.
Call the commands `ceamx/escape-url-line' and
`ceamx/escape-url-region' ."
  (interactive)
  (if (region-active-p)
    (ceamx/escape-url-region (region-beginning) (region-end))
    (ceamx/escape-url-line (line-beginning-position))))

(provide 'lib-simple)
;;; lib-simple.el ends here
