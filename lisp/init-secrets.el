;;; init-secrets.el --- Support for secretive operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
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

;; GnuPG, pinentry, ~auth-source~, Unix password store, etc.

;;; Sources:

;; <https://github.com/jwiegley/dot-emacs/blob/9d595c427136e2709dee33271db1a658493265bd/init.org#auth-source-pass>

;;; Code:

(require 'epg)
(require 'auth-source)
(require 'auth-source-pass)

(require 'ceamx-paths)
(require 'ceamx-lib)

;;; Configure secrets lookup with ~auth-source~ and the Unix password store

;; Ensure secrets and auth credentials are not stored in plaintext (the default).
;;
;; It's best to list only a single file here to avoid confusion about where
;; secrets might be stored.
(setopt auth-sources (list "~/.authinfo.gpg"))

;; TODO: provide explanation as to why these functions are named like so -- they just magically work..?
(use-feature! auth-source-pass
  :preface
  (defvar auth-source-pass--cache (make-hash-table :test #'equal))

  (defun auth-source-pass--reset-cache ()
    (setq auth-source-pass--cache (make-hash-table :test #'equal)))

  (defun auth-source-pass--read-entry (entry)
    "Return a string with the file content of ENTRY."
    (run-at-time 45 nil #'auth-source-pass--reset-cache)
    (let ((cached (gethash entry auth-source-pass--cache)))
      (or cached
        (puthash
          entry
          (with-temp-buffer
            (insert-file-contents (expand-file-name
                                    (format "%s.gpg" entry)
                                    (getenv "PASSWORD_STORE_DIR")))
            (buffer-substring-no-properties (point-min) (point-max)))
          auth-source-pass--cache))))

  (defun ceamx-auth-source-pass-list-items ()
    "Return a list of all password store items."
    (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
      (mapcar
        (lambda (file)
          (file-name-sans-extension (file-relative-name file store-dir)))
        (directory-files-recursively store-dir "\.gpg$"))))

  :config
  (auth-source-pass-enable))

;;; Use Emacs for pinentry

(use-feature! epg
  :defer 2
  :config
  (setopt epg-pinentry-mode 'loopback))

(provide 'init-secrets)
;;; init-secrets.el ends here
