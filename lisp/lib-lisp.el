;;; lib-lisp.el --- Lisp support functions  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2025  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

;;;; Variables

(defvar ceamx-lisp-modes-list '(emacs-lisp-mode
                                ielm-mode
                                lisp-mode
                                inferior-lisp-mode
                                lisp-interaction-mode)
  "Supported Lisp modes.")

(defvar +emacs-lisp-outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
  "Regexp to use for `outline-regexp' in `emacs-lisp-mode'.
This marks a foldable marker for `outline-minor-mode' in elisp buffers.")

(defvar ceamx-lisp-init-hook '()
  "Hook to run in all supported Lisp modes.")

;;;; Functions

;;;;; Public

(defun ceamx-lisp-init ()
  "Enable features useful in any Lisp mode."
  (when (and (fboundp 'lispy-mode))
    (lispy-mode))

  ;; `outli' overrides some `lispy' features.
  ;; <https://github.com/jdtsmith/outli?tab=readme-ov-file#configuration>
  (when (fboundp 'outli-mode)
    (outli-mode))

  (run-hooks 'ceamx-lisp-init-hook))

(defun ceamx-enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defun +emacs-lisp--in-package-buffer-p ()
  (let* ((file-path (buffer-file-name (buffer-base-buffer)))
         (file-base (if file-path (file-name-base file-path))))
    (and (derived-mode-p 'emacs-lisp-mode)
         (or (null file-base)
             (locate-file file-base (custom-theme--load-path) '(".elc" ".el"))))))

(defun ceamx/indent-last-sexp ()
  "Apply indentation to sexp before point."
  (interactive)
  (save-excursion
    (backward-list)
    (indent-sexp)))

(provide 'lib-lisp)
;;; lib-lisp.el ends here
