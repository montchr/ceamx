;;; config-window.el --- Settings for window management  -*- lexical-binding: t; -*-

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

;; Variables and options for window management configurations.

;;; Code:

(defcustom ceamx-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist.
The buffer will be created if it does not exist."
  :group 'ceamx
  :type '(string))

(provide 'config-window)
;;; config-window.el ends here