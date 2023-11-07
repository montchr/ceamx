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

;; Always use 2-space indentation.
(dolist (sym '(add-function advice-add plist-put))
  (put sym 'lisp-indent-function 2))

(use-feature eldoc
  :hook (emacs-lisp-mode)
  :diminish eldoc-mode)

;;; `suggest' :: <https://github.com/Wilfred/suggest.el>
;;  discover elisp functions that do what you want,
;;  brought to you by enumerative program synthesis
(use-package suggest
  :commands (suggest))

;; FIXME: via doom <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/lang/emacs-lisp/config.el#L10C1-L20C50>
;; (defvar +emacs-lisp-linter-warnings
;;   '(not free-vars    ; don't complain about unknown variables
;;         noruntime    ; don't complain about unknown function calls
;;         unresolved)  ; don't complain about undefined functions
;;   "The value for `byte-compile-warnings' in non-packages.

;; This reduces the verbosity of flycheck in Emacs configs and scripts, which are
;; so stateful that the deluge of false positives (from the byte-compiler,
;; package-lint, and checkdoc) can be more overwhelming than helpful.

;; See `+emacs-lisp-non-package-mode' for details.")

;; (after! 'flycheck
;;   ;; UX: Flycheck's two emacs-lisp checkers produce a *lot* of false positives
;;   ;;   in non-packages (like Emacs configs or elisp scripts), so I disable
;;   ;;   `emacs-lisp-checkdoc' and set `byte-compile-warnings' to a subset of the
;;   ;;   original in the flycheck instance (see `+emacs-lisp-linter-warnings').
;;   ;; TODO: do it
;;   ;; (add-hook 'flycheck-mode-hook #'+emacs-lisp-non-package-mode)
;;   )

(after! 'smartparens
  (smartparens-strict-mode +1))

(defvar evil-cleverparens-use-s-and-S nil)
(use-package evil-cleverparens
  :init
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  :config ;; (require 'evil-cleverparens-text-objects)
  )

;; TODO
;; (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)

;; TODO
;; <https://github.com/bbatsov/prelude/blob/b57ff48e0985a6ef0f1ed9b279ec487c55982334/core/prelude-core.el#L147>
;; (defun prelude-wrap-with (s)
;;   "Create a wrapper function for smartparens using S."
;;   `(lambda (&optional arg)
;;      (interactive "P")
;;      (sp-wrap-with-pair ,s)))

(provide 'init-lisp)
;;; init-lisp.el ends here
