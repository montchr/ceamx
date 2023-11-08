;;; init-lisp.el --- Emacs Lisp language configuration -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;;  Configuration for working with Emacs Lisp.

;;; Code:

(require 'config-lisp)

;;
;;; Shared mode hooks
;;

;; For managing load order, especially concerning visual enhancements.
;; Apply to hooks on individual Lisp modes in their respective files.

(defun cmx-lisp-prog-defaults-h ()
  (after! 'smartparens (smartparens-strict-mode +1))
  ;; (after! 'evil-cleverparens (evil-cleverparens-mode +1))
  (after! 'rainbow-delimiters (rainbow-delimiters-mode +1))
  ;; `highlight-function-calls-mode' should be invoked after other highlighters
  ;; (e.g. `rainbow-delimiters-mode'), according to its readme.
  (after! 'highlight-function-calls (highlight-function-calls-mode +1)))

(setq cmx-lisp-prog-hook 'cmx-lisp-prog-defaults-h)

(defun cmx-interactive-lisp-prog-defaults-h ()
  (after! 'smartparens
    (smartparens-strict-mode +1))
  (after! 'rainbow-delimiters
    (rainbow-delimiters-mode +1))
  (whitespace-mode -1))

(setq cmx-interactive-lisp-prog-hook 'cmx-interactive-lisp-prog-defaults-h)

;; Always use 2-space indentation in Lisps.
(dolist (sym '(add-function advice-add plist-put))
  (put sym 'lisp-indent-function 2))

(provide 'init-lisp)
;;; init-lisp.el ends here
