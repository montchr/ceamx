;;; ceamx-lang.el --- Ceamx Language support helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: languages, convenience, local

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
;;; Code:

(require 'seq)

;;;; Customization

(defcustom ceamx-lang-typo-mode-excluded-modes nil
  "Modes where `typo-mode' should not be enabled."
  :group 'ceamx-lang
  :type '(symbol))

;;;; Functions



(provide 'ceamx-lang)
;;; ceamx-lang.el ends here
