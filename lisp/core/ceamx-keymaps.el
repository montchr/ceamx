;;; ceamx-keymaps.el --- Keymap declarations               -*- lexical-binding: t; -*-


;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
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

(require 'ceamx-lib)

(defvar-keymap ceamx-activities-map)
(define-prefix-command 'ceamx-activities-map)

(defvar-keymap ceamx-appearance-map)
(define-prefix-command 'ceamx-appearance-map)

(defvar-keymap ceamx-file-map)
(define-prefix-command 'ceamx-file-map)

(defvar-keymap ceamx-insert-map)
(define-prefix-command 'ceamx-insert-map)

(defvar-keymap ceamx-launch-map)
(define-prefix-command 'ceamx-launch-map)

(defvar-keymap ceamx-packages-map)
(define-prefix-command 'ceamx-packages-map)

(defvar-keymap ceamx-pairs-map)
(define-prefix-command 'ceamx-pairs-map)

(defvar-keymap ceamx-replace-map)
(define-prefix-command 'ceamx-replace-map)

(defvar-keymap ceamx-session-map)
(define-prefix-command 'ceamx-session-map)

(defvar-keymap ceamx-toggle-map)
(define-prefix-command 'ceamx-toggle-map)

(provide 'ceamx-keymaps)
;;; ceamx-keymaps.el ends here
