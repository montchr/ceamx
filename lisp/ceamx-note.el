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

;;;; Requirements

(require 'ceamx-paths)

(autoload 'denote "denote")
(autoload 'denote-subdirectory "denote")
(autoload 'denote-template "denote")
(autoload 'denote-type "denote")
(autoload 'denote-journal-new-or-existing-entry "denote-journal")

;;;; Customization

(defgroup ceamx-note nil
  "Ceamx notetaking"
  :group 'ceamx)

(defcustom ceamx-note-dailies-dir
  (file-name-as-directory (concat ceamx-notes-dir "daily"))
  "Base directory for daily logs."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-default-dir
  (file-name-as-directory (concat ceamx-notes-dir "default"))
  "Base directory for the default notes scope."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-journal-dir
  (file-name-as-directory (concat ceamx-notes-dir "journal"))
  "Base directory for the Journal notes silo."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-work-dir
  (file-name-as-directory (concat ceamx-notes-dir "work"))
  "Base directory for the Work notes silo."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-silo-directories nil
  "List of filesystem directories pointing to the note silo roots."
  :type '(directory)
  :group 'ceamx-note)

(defcustom ceamx-note-denote-silo-commands
  '(denote
    denote-date
    denote-subdirectory
    denote-template
    denote-type)
  "List of Denote commands to call after selecting a silo.
This is a list of symbols that specify the note-creating interactive
functions that Denote provides."
  :type '(symbol)
  :group 'ceamx-note)

;;;; Functions

;;;; Commands

(defun ceamx-note/create-or-visit-journal-entry ()
  "Invoke `denote-journal-new-or-existing-entry' scoped to the
Ceamx Journal silo."
  (interactive)
  (let ((denote-directory ceamx-note-journal-dir))
    (call-interactively #'denote-journal-new-or-existing-entry)))

;;; Footer

(provide 'ceamx-note)
;;; ceamx-note.el ends here
