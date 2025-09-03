;;; ceamx-lisp.el --- ceamx :: lib :: lisp           -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: lisp

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

;;;; Variables

;;;; Customization

(defgroup ceamx-lisp nil
  "Group for `ceamx-lisp' customization."
  :group 'ceamx)

(defcustom ceamx-lisp-global-modes
  '( emacs-lisp-mode
     ielm-mode
     lisp-mode
     inferior-lisp-mode
     lisp-interaction-mode
     yuck-mode)
  "Lisp modes supported by `ceamx-lisp'."
  :type	'(repeat sexp)
  :group 'ceamx-lisp)

;;;; Functions

;;;###autoload
(defun ceamx-lisp-mode-enable ()
  "Enable features for `ceamx-lisp-mode'."
  (when (and (fboundp 'lispy-mode))
    (lispy-mode 1))

  ;; `outli' overrides some `lispy' features.
  ;; <https://github.com/jdtsmith/outli?tab=readme-ov-file#configuration>
  (when (fboundp 'outli-mode)
    (outli-mode 1))

  (add-hook 'after-save-hook #'check-parens nil t))

;;;###autoload
(defun ceamx-lisp-mode-disable ()
  "Disable features for `ceamx-lisp-mode'."
  (when (and (fboundp 'lispy-mode))
    (lispy-mode -1))

  ;; `outli' overrides some `lispy' features.
  ;; <https://github.com/jdtsmith/outli?tab=readme-ov-file#configuration>
  (when (fboundp 'outli-mode)
    (outli-mode -1))
  
  (remove-hook 'after-save-hook #'check-parens t))

(defun ceamx-lisp-package-buffer-p ()
  "Whether the current buffer is part of a package or theme."
  (let* ((file-path (buffer-file-name (buffer-base-buffer)))
         (file-base (if file-path (file-name-base file-path))))
    (and (derived-mode-p 'emacs-lisp-mode)
         (or (null file-base)
             (locate-file file-base (custom-theme--load-path) '(".elc" ".el"))))))

;;;; Modes

;;;###autoload
(define-globalized-minor-mode ceamx-lisp-global-mode
  ceamx-lisp-mode ceamx-lisp-mode
  :predicate ceamx-lisp-global-modes)

;;;###autoload
(define-minor-mode ceamx-lisp-mode
  "Minor mode for supported Lisp major modes."
  :group 'ceamx-lisp
  (cond
   (ceamx-lisp-mode
    (ceamx-lisp-mode-enable))
   (t
    (ceamx-lisp-mode-disable))))

;;;; Footer 

(provide 'ceamx-lisp)
;;; ceamx-lisp.el ends here
