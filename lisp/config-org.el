;;; config-org.el --- org-mode variables             -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords:

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

(require 'config-notes)

(defvar ceamx-default-agenda-files
  (f-glob "*.org" ceamx-agenda-dir)
  "List of absolute paths of all files that should be included in the agenda.")

(defvar ceamx-default-todo-file
  (expand-file-name "todo.org" ceamx-agenda-dir)
  "Absolute path to default file for active G2D.")

(defvar ceamx-default-capture-file
  (expand-file-name "inbox.org" ceamx-agenda-dir)
  "Absolute path to default inbox file for new G2D waiting to be processed.")

(provide 'config-org)
;;; config-org.el ends here
