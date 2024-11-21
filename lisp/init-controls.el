;;; init-controls.el --- Controlling various subsystems  -*- lexical-binding: t; -*-

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

;; " Is Control controlled by its need to control? "

;;; Code:

;;; Requirements

(require 'ceamx-keymaps)
(require 'ceamx-lib)

(keymap-global-set "C-c o" '("Launch" . ceamx-launch-map))
(keymap-global-set "C-c t" '("Toggle" . ceamx-toggle-map))

(define-keymap :keymap ceamx-toggle-map
  "l" #'display-line-numbers-mode
  "w" '("side windows" . window-toggle-side-windows))

(provide 'init-controls)
;;; init-controls.el ends here
