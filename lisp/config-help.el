;;; config-help.el --- Help settings                 -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local, help

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

;; Settings for helping.

;;; Code:

(require 'ceamx-paths)

(defvar cmx-cheatsheets-dir
  (file-name-as-directory
   (concat cmx-home-dir "Documents/cheatsheets"))
  "Absolute path to the directory containing user cheatsheets.")

(provide 'config-help)
;;; config-help.el ends here
