;;; init-lang-emacs-lisp.el --- Emacs Lisp development support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local, lisp

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

(require 'config-lisp)

;;
;;; Mode hooks
;;

;;; `emacs-lisp-mode'

(defun cmx-prog--emacs-lisp-init-h ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'cmx-prog-lisp-init-hook)
  (after! 'eldoc (eldoc-mode +1))
  (after! 'rainbow-mode (rainbow-mode +1))
  (setq mode-name "EL"))

(setq cmx-prog-emacs-lisp-hook 'cmx-prog--emacs-lisp-init-h)

(add-hook 'emacs-lisp-mode-hook (cmd! (run-hooks 'cmx-prog-emacs-lisp-hook)))

;;; `ielm'

(defun cmx-prog--ielm-init-h ()
  "Sensible defaults for `ielm'."
  (run-hooks 'cmx-interactive-lisp-prog-init-hook)
  (after! 'eldoc
    (eldoc-mode +1)))

(defvar cmx-prog-ielm-init-hook #'cmx-prog--ielm-init-h)

(add-hook 'ielm-mode-hook (cmd! (run-hooks 'cmx-prog-ielm-init-hook)))

;;
;;; Configuration
;;

;;; `eldoc' (internal)
(use-feature! eldoc
  :blackout)

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

(after! 'lispy
  (setq lispy-outline
        (concat
         ;; `lispy-mode' requires `lispy-outline' start with ^
         (unless (string-prefix-p "^" +emacs-lisp-outline-regexp) "^")
         +emacs-lisp-outline-regexp)))

;;
;;; Keybinds
;;

(keymap-set emacs-lisp-mode-map "C-c C-c" #'eval-defun)

(provide 'init-lang-emacs-lisp)
;;; init-lang-emacs-lisp.el ends here
