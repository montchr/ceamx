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

(defvar-local cmx-autoloads-sources-dir
    (f-join user-emacs-directory "lisp"))

(defvar-local cmx-autoloads-excludes
    (append (f-glob "config-*.el" cmx-autoloads-sources-dir)
            (f-glob "init-*.el" cmx-autoloads-sources-dir)))

(defvar-local cmx-autoloads-dir
    (f-join user-emacs-directory "autoloads"))

(defvar-local cmx-autoloads-defs-file
    (f-join cmx-autoloads-dir "ceamx-autoloads.el"))

(message "%s" cmx-autoloads-excludes)
(message "%s" cmx-autoloads-dir)
(message "%s" cmx-autoloads-defs-file)

(defun cmx-autoloads--ensure-dir ()
  "Ensure the target directory exists."
  (unless (f-dir-p cmx-autoloads-dir)
    (f-mkdir cmx-autoloads-dir)))

(progn
  (cmx-autoloads--ensure-dir)
  (loaddefs-generate cmx-autoloads-sources-dir
                     cmx-autoloads-defs-file
                     cmx-autoloads-excludes))

(provide 'generate-autoloads)
;;; generate-autoloads.el ends here
