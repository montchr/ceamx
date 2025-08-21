;;; ceamx-keymaps.el --- Keymaps for Ceamx           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: convenience, local, internal

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

;; Keymaps required for Ceamx operations.

;;; Code:

(define-prefix-command 'ceamx-activities-prefix)
(define-prefix-command 'ceamx-appearance-prefix)
(define-prefix-command 'ceamx-bookmark-prefix 'ceamx-bookmark-prefix-map)
(define-prefix-command 'ceamx-buffer-prefix)
(define-prefix-command 'ceamx-capture-prefix 'ceamx-capture-prefix-map)
(define-prefix-command 'ceamx-code-prefix)
(define-prefix-command 'ceamx-completion-prefix 'ceamx-completion-prefix-map)
(define-prefix-command 'ceamx-cryption-prefix 'ceamx-cryption-prefix-map)
(define-prefix-command 'ceamx-file-prefix)
(define-prefix-command 'ceamx-fold-prefix)
(define-prefix-command 'ceamx-help-keybindings-prefix 'ceamx-help-keybindings-prefix-map)
(define-prefix-command 'ceamx-history-prefix)
(define-prefix-command 'ceamx-info-prefix 'ceamx-info-prefix-map)
(define-prefix-command 'ceamx-insert-prefix 'ceamx-insert-prefix-map "[ INSERT ]")
(define-prefix-command 'ceamx-launch-prefix)
(define-prefix-command 'ceamx-note-prefix 'ceamx-note-prefix-map)
(define-prefix-command 'ceamx-journal-prefix 'ceamx-journal-prefix-map)
(define-prefix-command 'ceamx-package-prefix)
(define-prefix-command 'ceamx-replace-prefix)
(define-prefix-command 'ceamx-session-prefix)
(define-prefix-command 'ceamx-snippet-prefix)
(define-prefix-command 'ceamx-structural-editing-prefix 'ceamx-structural-editing-prefix-map)
(define-prefix-command 'ceamx-toggle-prefix)
(define-prefix-command 'ceamx-web-prefix 'ceamx-web-prefix-map)
(define-prefix-command 'ceamx-workspace-prefix)

(provide 'ceamx-keymaps)
;;; ceamx-keymaps.el ends here
