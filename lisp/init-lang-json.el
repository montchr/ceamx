;;; init-lang-json.el --- JSON language support      -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local, languages

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

;; TODO: better JSONC support

;;; Code:

;;; Requirements

(require 'elpaca-autoloads)

;;; Navigate arborescent JSON structures with `json-navigator'

;; <https://github.com/DamienCassou/json-navigator>

;; TODO: add bindings

(package! json-navigator)

(provide 'init-lang-json)
;;; init-lang-json.el ends here
