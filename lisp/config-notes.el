;;; config-notes.el --- Notes settings               -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

(require 'f)

(require 'ceamx-paths)

(defvar ceamx-notes-dir
  (f-join ceamx-home-dir "Documents/notes")
  "Base directory for note storage.")

(defvar ceamx-agenda-dir
  (f-join ceamx-notes-dir "g2d"))

(defvar ceamx-dailies-dir
  (f-join ceamx-notes-dir "daily"))

(defvar ceamx-notes-default-dir
  (f-join ceamx-notes-dir "default"))

(defvar ceamx-journal-dir
  (f-join ceamx-notes-dir "journal"))

(defvar ceamx-work-notes-dir
  (f-join ceamx-notes-dir "work"))

(provide 'config-notes)
;;; config-notes.el ends here
