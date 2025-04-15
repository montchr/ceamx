;;; yijing-data.el --- Helpers for reading Yijing JSON data  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: data

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

(require 'seq)

(require 'llama)

;;;; Customization

(defcustom yijing-data-directory
  (file-name-as-directory (concat yijing-base-directory "data"))
  "Base directory for the `yijing-data' file storage."
  :group 'yijing
  :type 'directory)

(defcustom yijing-data-hexagrams-json-file
  (file-name-concat yijing-data-directory "hexagrams.json")
  "JSON file containing Yijing hexagram data."
  :group 'yijing
  :type 'file)


(provide 'yijing-data)
;;; yijing-data.el ends here
