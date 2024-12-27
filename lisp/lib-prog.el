;;; lib-prog.el --- Assorted helper callables for programming modes  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(defvar ceamx-eglot-server-configurations-alist '()
  "Alist of language server initialization options as accepted in `eglot-server-programs'.")
(defun ceamx-eglot-server-default-settings (name)
  "Return the custom initialization options for the NAME language server."
  (alist-get name ceamx-eglot-server-configurations-alist nil nil #'string=))

(defun ceamx-eglot-server-contact (name &optional program &rest args)
  "Return a contact specification for the language server NAME.
NAME is a string of the \"<lang>-<program>\" format for naming
language servers in Ceamx.  This format is based on the format
commonly used by `lsp-mode'.

PROGRAM and ARGS are as in `eglot-server-programs', which see.

Unless PROGRAM is provided, the program name used in
`eglot-server-programs' will be the value of NAME."
  (let ((options (ceamx-eglot-server-default-settings name))
        (program (or program (string-trim-left name "[[:alpha:]]+-"))))
    ;; The use of `append' here is significant because it will filter out a nil
    ;; value for `options'.
    (append (ensure-list program)
            args
            (when options (list :initializationOptions options)))))

(provide 'lib-prog)
;;; lib-prog.el ends here
