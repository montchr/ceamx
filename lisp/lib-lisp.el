;;; lib-lisp.el --- Lisp library functions           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; TODO: elpaca package path
(defun +emacs-lisp--in-package-buffer-p ()
  (let* ((file-path (buffer-file-name (buffer-base-buffer)))
         (file-base (if file-path (file-name-base file-path))))
    (and (derived-mode-p 'emacs-lisp-mode)
         (or (null file-base)
             (locate-file file-base (custom-theme--load-path) '(".elc" ".el"))))))

;; TODO: find some other alternative, i'm not sure how to remove the doom
;; FIXME: integrate
;; FIXME: copyright/license <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/lang/emacs-lisp/autoload.el#L280C1-L327C1>
;;;###autoload
;; (define-minor-mode cmx-emacs-lisp-non-package-mode
;;   "Reduce flycheck verbosity where it is appropriate."
;;   (unless (and (bound-and-true-p flycheck-mode)
;;                (not (+emacs-lisp--in-package-buffer-p)))
;;     (setq cmx-emacs-lisp-non-package-mode nil))
;;   (when (derived-mode-p 'emacs-lisp-mode)
;;     (add-hook 'after-save-hook #'cmx-emacs-lisp-non-package-mode nil t))
;;   (if (not cmx-emacs-lisp-non-package-mode)
;;       (when (get 'flycheck-disabled-checkers 'initial-value)
;;         (setq-local flycheck-disabled-checkers (get 'flycheck-disabled-checkers 'initial-value))
;;         (kill-local-variable 'flycheck-emacs-lisp-check-form))
;;     (with-memoization (get 'flycheck-disabled-checkers 'initial-value)
;;       flycheck-disabled-checkers)
;;     (setq-local flycheck-emacs-lisp-check-form
;;                 (prin1-to-string
;;                  `(progn
;;                     (setq doom-modules ',doom-modules
;;                           doom-disabled-packages ',doom-disabled-packages
;;                           byte-compile-warnings ',+emacs-lisp-linter-warnings)
;;                     (condition-case e
;;                         (progn
;;                           (require 'doom)
;;                           (require 'doom-cli)
;;                           (require 'doom-start))
;;                       (error
;;                        (princ
;;                         (format "%s:%d:%d:Error:Failed to load Doom: %s\n"
;;                                 (or ,(ignore-errors
;;                                        (file-name-nondirectory
;;                                         (buffer-file-name (buffer-base-buffer))))
;;                                     (car command-line-args-left))
;;                                 0 0 (error-message-string e)))))
;;                     ,(read (default-toplevel-value 'flycheck-emacs-lisp-check-form))))
;;                 flycheck-disabled-checkers (cons 'emacs-lisp-checkdoc
;;                                                  flycheck-disabled-checkers))))

(provide 'lib-lisp)
;;; lib-lisp.el ends here
