;;; lib-lisp.el --- Lisp library functions           -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

(defvar ceamx-lisp-init-hook '()
  "Hook to run in all supported Lisp modes.")

(defun ceamx-lisp-init ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'ceamx-lisp-init-hook))

(defun ceamx-enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

;; TODO: elpaca package path
(defun +emacs-lisp--in-package-buffer-p ()
  (let* ((file-path (buffer-file-name (buffer-base-buffer)))
          (file-base (if file-path (file-name-base file-path))))
    (and (derived-mode-p 'emacs-lisp-mode)
      (or (null file-base)
        (locate-file file-base (custom-theme--load-path) '(".elc" ".el"))))))

(provide 'lib-lisp)
;;; lib-lisp.el ends here
