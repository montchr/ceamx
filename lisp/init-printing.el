;;; init-printing.el --- Support for printing documents  -*- lexical-binding: t; -*-

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

;; FIXME: prints raw PDF data

;;; Code:

(require 'ceamx-lib)

(use-feature! printing
  :defer 10
  :commands (pr-update-menus)
  :config
  ;; EPSON WF-3520
  (setopt printer-name "LABORTTY")
  ;; (setopt lpr-switches '())
  (pr-update-menus))

(provide 'init-printing)
;;; init-printing.el ends here
