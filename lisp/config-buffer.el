;;; config-buffer.el --- Variables relating to buffers and modes  -*- lexical-binding: t; -*-

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

;;; Sources:

;; <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>

;;; Code:

(require 'ceamx-paths)

(defvar ceamx-occur-grep-modes-list
  '(occur-mode
     grep-mode
     xref--xref-buffer-mode
     flymake-diagnostics-buffer-mode)
  "List of major-modes used in occur-type buffers.")

(defvar ceamx-repl-modes-list
  '(eshell-mode
    inferior-emacs-lisp-mode            ; ielm
    shell-mode
    eat-mode
    nix-repl-mode)
  "List of major-modes used in REPL buffers.")

(defvar ceamx-repl-buffer-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*Inferior .*\\*$"
    "\\*ielm\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers.")

(defvar ceamx-help-modes-list
  '(helpful-mode
     help-mode
     eldoc-mode)
  "List of major-modes used in documentation buffers.")

(defvar ceamx-help-buffer-names-list
  '("^\\*Apropos"
     "^\\*eldoc\\*")
  "List of buffer names used in help buffers.")

(defvar ceamx-manual-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers.")

(defvar ceamx-message-modes-list
  '(compilation-mode
    edebug-eval-mode)
  "List of major-modes used in message buffers.")

(defvar ceamx-buffer-read-only-dirs-list
  (list ceamx-packages-dir)
  "List of directories whose files should be opened in read-only buffers.")

(provide 'config-buffer)
;;; config-buffer.el ends here
