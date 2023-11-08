;;; config-ui.el --- General UI settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; FIXME: defcustom with preset options

;;; Code:

(defvar cmx-ui-theme-dark 'modus-vivendi "Preferred dark theme.")

(defvar cmx-ui-theme-light 'modus-operandi "Preferred light theme.")

(defvar cmx-modeline-provider 'doom-modeline)

(provide 'config-ui)
;;; config-ui.el ends here