;;; lib-secrets.el --- Helpers for secrets           -*- lexical-binding: t; -*-

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

;;

;;; Code:

(eval-when-compile
  (require 'auth-source)
  (require 'auth-source-pass))

;; via <https://github.com/jwiegley/dot-emacs/blob/9d595c427136e2709dee33271db1a658493265bd/init.org#lookup-a-password-using-auth-source>
(defun ceamx-lookup-password (host user port)
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

(provide 'lib-secrets)
;;; lib-secrets.el ends here
