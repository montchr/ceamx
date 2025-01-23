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

(autoload 'denote "denote")
(autoload 'denote-subdirectory "denote")
(autoload 'denote-template "denote")
(autoload 'denote-type "denote")

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
  "Base directory for the Journal notes silo."
  :type 'directory
  :group 'ceamx-note)

(defcustom ceamx-note-work-dir
  (file-name-as-directory (concat ceamx-note-dir "work"))
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

(defun ceamx-note/denote/pick-silo-then-command (silo command)
  "Select SILO and run Denote COMMAND within.
SILO is a filesystem directory listed in `ceamx-note-silo-directories'.
COMMAND is a command listed in `ceamx-note-denote-silo-commands'."
  (interactive
   (list (completing-read "Select a silo: " ceamx-note-silo-directories nil t)
         (intern (completing-read
                  "Run command in silo: "
                  ceamx-note-denote-silo-commands nil t))))
  (let ((denote-directory silo))
    (call-interactively command)))

;;; Footer

(provide 'ceamx-note)
;;; ceamx-note.el ends here
