;;; ceamx-auth.el --- Ceamx auth source integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
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

;;;; Password store (`auth-source-pass')

;;;;; Override `auth-source-pass' cache mechanism

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

;;;;; Functions

(defun ceamx-auth-source-pass-list-items ()
  "Return a list of all password store items."
  (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
    (mapcar
     (lambda (file)
       (file-name-sans-extension (file-relative-name file store-dir)))
     (directory-files-recursively store-dir "\.gpg$"))))

;;;;; Commands

(defun ceamx-auth/lookup (host user &optional port)
  (interactive)
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

(provide 'ceamx-auth)
;;; ceamx-auth.el ends here
