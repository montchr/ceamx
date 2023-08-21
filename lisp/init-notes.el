;;; init-notes.el --- Configuration for notetaking   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; Generic notetaking configurations.

;;; Code:

;;
;;; consult-notes <https://github.com/mclear-tools/consult-notes>
;;

(use-package consult-notes
  :elpaca (:host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes))

;; via <https://github.com/mclear-tools/consult-notes#embark-support>
;; (after! [consult-notes embark]
;; 	(defun cmx/consult-notes-embark-action (cand)
;;     "Do something with CAND."
;;     (interactive "fNote: ")
;;     ;; FIXME: needs function
;;     ;;
;;     ;; > Note that Embark will run on the CAND at point, which will often return
;;     ;; > either a file name, or a file name plus other annotations, depending on
;;     ;; > what your sources are. So you’ll have to write a function to manipulate
;;     ;; > CAND to give you a viable path to the file or a directory containing
;;     ;; > the file.
;;     (my-function))

;;   (defvar-keymap consult-notes-map
;;     :doc "Keymap for Embark notes actions."
;;     :parent embark-file-map
;;     "m" #'cmx/consult-notes-embark-action)

;;   (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

;;   ;; Make `embark-export' use dired for notes.
;;   (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired))


(provide 'init-notes)
;;; init-notes.el ends here

