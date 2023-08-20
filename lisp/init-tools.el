;;; init-tools.el --- Tools and utilities            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

;; Tools and, of course, utilities.

;;; Code:

;;; <https://github.com/astoff/devdocs.el>
;; NOTE: Must run `devdocs-install' before use.
;; TODO: Install devdocs automatically.
(use-package devdocs
  :defer 5
  :commands (devdocs-lookup devdocs-install devdocs-delete devdocs-update-all)
  :config
  (devdocs-update-all)
  (keymap-global-set "C-h D" #'devdocs-lookup))

(provide 'init-tools)
;;; init-tools.el ends here

