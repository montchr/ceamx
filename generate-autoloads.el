;;; generate-autoloads.el --- Generate autoloads from lib files  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local, lisp

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

(require 'f)

(defvar-local ceamx-autoloads-sources-dir
    (f-join user-emacs-directory "lisp"))

(defvar-local ceamx-autoloads-excludes
    (append (f-glob "config-*.el" ceamx-autoloads-sources-dir)
            (f-glob "init-*.el" ceamx-autoloads-sources-dir)))

(defvar-local ceamx-autoloads-dir
    (f-join user-emacs-directory "autoloads"))

(defvar-local ceamx-autoloads-defs-file
    (f-join ceamx-autoloads-dir "ceamx-autoloads.el"))

(message "%s" ceamx-autoloads-excludes)
(message "%s" ceamx-autoloads-dir)
(message "%s" ceamx-autoloads-defs-file)

(defun ceamx-autoloads--ensure-dir ()
  "Ensure the target directory exists."
  (unless (f-dir-p ceamx-autoloads-dir)
    (f-mkdir ceamx-autoloads-dir)))

(progn
  (ceamx-autoloads--ensure-dir)
  (loaddefs-generate ceamx-autoloads-sources-dir
                     ceamx-autoloads-defs-file
                     ceamx-autoloads-excludes))

(provide 'generate-autoloads)
;;; generate-autoloads.el ends here
