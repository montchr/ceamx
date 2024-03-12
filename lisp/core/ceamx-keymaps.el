;;; ceamx-keymaps.el --- Keymap declarations               -*- lexical-binding: t; -*-


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

(require 'lib-keys)

(defmap! ceamx-code-map)
(defmap! ceamx-file-map)
(defmap! ceamx-launch-map)
(defmap! ceamx-replace-map)
(defmap! ceamx-toggle-map)

(provide 'ceamx-keymaps)
;;; ceamx-keymaps.el ends here
