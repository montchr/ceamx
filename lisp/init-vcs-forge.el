;;; init-vcs-forge.el --- Support for git forge integration  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'lib-common)

(use-package ghub
  :demand t)

;; TODO: maybe: <https://github.com/jwiegley/dot-emacs/blob/9d595c427136e2709dee33271db1a658493265bd/init.org#ghub>
;; :config
;; (require 'auth-source-pass)
;; (defvar ceamx-ghub-token-cache nil)
;; (def-advice! +ghub--token-use-cache-a (orig-func host username package &optional nocreate forge)
;;   :around #'ghub--token
;;   "Use a cached GitHub token."
;;   (or ceamx-ghub-token-cache
;;     (setq ceamx-ghub-token-cache
;;       (funcall orig-func host username package nocreate forge))))


(provide 'init-vcs-forge)
;;; init-vcs-forge.el ends here
