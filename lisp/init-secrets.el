;;; init-secrets.el --- Support for secretive operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

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

;; Examples may or may not include pinentry, handling file encryption,
;; password-store integration, etc.

;;; Code:

(require 'epg)
;; Ensure secrets and auth credentials are not stored in plaintext (the default).
;;
;; It's best to list only a single file here to avoid confusion about where
;; secrets might be stored.
(setopt auth-sources (list "~/.authinfo.gpg"))


(setopt epg-pinentry-mode 'loopback)

(provide 'init-secrets)
;;; init-secrets.el ends here
