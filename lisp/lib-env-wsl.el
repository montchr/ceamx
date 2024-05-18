;;; lib-env-wsl.el --- Helpers for WSL environments  -*- lexical-binding: t; -*-

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

;;; Sources:

;; <https://emacsredux.com/blog/2021/12/19/wsl-specific-emacs-configuration/>
;; <https://emacsredux.com/blog/2021/12/19/using-emacs-on-windows-11-with-wsl2/>

;;; Code:

(require 'config-env)

(defun ceamx-wsl/copy-selected-text (start end)
  "In WSL, copy text region with START and END to the host clipboard."
  (interactive "r")
  (if (use-region-p)
    (let ((text (buffer-substring-no-properties start end)))
      (shell-command (concat "echo '" text "' | clip.exe")))))

(provide 'lib-env-wsl)
;;; lib-env-wsl.el ends here
