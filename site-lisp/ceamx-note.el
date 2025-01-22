;;; ceamx-note.el --- Ceamx: Notetaking helpers      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: files, local

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

;;;; Customization

(defgroup ceamx-note nil
  "Ceamx notetaking"
  :group 'ceamx)

(defcustom ceamx-note-dir
  (file-name-as-directory (concat ceamx-home-dir "Documents/notes"))
  "Base directory for note storage."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-dailies-dir
  (file-name-as-directory (concat ceamx-note-dir "daily"))
  "Base directory for daily logs."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-default-dir
  (file-name-as-directory (concat ceamx-note-dir "default"))
  "Base directory for the default notes scope."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-journal-dir
  (file-name-as-directory (concat ceamx-note-dir "journal"))
  "Base directory for the journal notes scope."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-work-dir
  (file-name-as-directory (concat ceamx-note-dir "work"))
  "Base directory for the work notes scope."
  :type 'directory
  :group 'ceamx-note)

;;; Footer

(provide 'ceamx-note)
;;; ceamx-note.el ends here
