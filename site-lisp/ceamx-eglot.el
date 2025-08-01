;;; ceamx-eglot.el --- Ceamx Eglot extensions        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: extensions

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

(defgroup ceamx-eglot nil
  "Customizations for the `ceamx-eglot' feature."
  :group 'ceamx)

;;;; Variables

(defvar ceamx-eglot-server-configurations-alist '()
  "Alist of language server initialization options as accepted in `eglot-server-programs'.")

;;;; Customization

(defcustom ceamx-eglot-json-schema-catalog-file
    (expand-file-name "data/json-schema/catalog.json" user-emacs-directory)
    "Path to file containing the full SchemaStore catalog.
The canonical source for the file can be found at the following location:

    <https://raw.githubusercontent.com/SchemaStore/schemastore/refs/heads/master/src/api/json/catalog.json>"
  :type '(file)
  :group 'ceamx-eglot)

;;;; Functions

;;;###autoload
(defun ceamx-eglot-json-schema-catalog ()
  "Return the JSON representation of `ceamx-eglot-json-schema-catalog-file'."
  (let* ((json-object-type 'plist)
          (json-array-type 'vector)
          (json-key-type 'keyword))
    (json-read-file ceamx-eglot-json-schema-catalog-file)))

;;;###autoload
(defun ceamx-eglot-server-default-settings (name)
  "Return the custom initialization options for the NAME language server."
  (alist-get name ceamx-eglot-server-configurations-alist nil nil #'string=))

;;;###autoload
(defun ceamx-eglot-server-contact (name &optional program &rest args)
  "Return a contact specification for the language server NAME.
NAME is a string of the \"<lang>-<program>\" format for naming
language servers in Ceamx.  This format is based on the format
commonly used by `lsp-mode'.

PROGRAM and ARGS are as in `eglot-server-programs', which see.

Unless PROGRAM is provided, the program name used in
`eglot-server-programs' will be the value of NAME with the \"<LANG>-\"
prefix removed.  For example, given the NAME \"toml-taplo\", where LANG
is \"toml\" and PROGRAM is \"taplo\", \"taplo\" is the name of the
command in `eglot-server-programs'."
  (let ((options (ceamx-eglot-server-default-settings name))
         ;; Remove the language identifier prefix.
         (program (or program (string-trim-left name "[[:alpha:]]+-"))))
    ;; The use of `append' here is significant because it will filter out a nil
    ;; value for `options'.
    (append (ensure-list program)
      args
      (when options (list :initializationOptions options)))))

(provide 'ceamx-eglot)
;;; ceamx-eglot.el ends here
