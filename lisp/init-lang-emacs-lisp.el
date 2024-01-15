;;; init-lang-emacs-lisp.el --- Emacs Lisp development support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

(require 'lib-common)
(require 'lib-packages)
(require 'lib-lisp)

(declare-function blackout "blackout")


;;
;;; Hooks

(defun ceamx-emacs-lisp-init ()
  "Sensible defaults for `emacs-lisp-mode'."
  (ceamx-lisp-init)
  (eldoc-mode 1)
  ;; TODO: do we really want this for `ielm' and other derived modes as well?
  (blackout "EL"))

(add-hook 'emacs-lisp-mode-hook #'ceamx-emacs-lisp-init)
(add-hook 'ielm-mode-hook #'ceamx-emacs-lisp-init)

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

;;
;;; Keybinds

(keymap-set emacs-lisp-mode-map "C-c C-c" #'eval-last-sexp)

;;
;;; Packages

;;; `suggest' :: <https://github.com/Wilfred/suggest.el>
;;  discover elisp functions that do what you want,
;;  brought to you by enumerative program synthesis
(use-package suggest
  :commands (suggest)
  :init
  (keymap-set emacs-lisp-mode-map "C-c m S" #'suggest))

;;; `macrostep' :: <https://github.com/emacsorphanage/macrostep>
;;  "interactive macro-expander for Emacs"
(use-package macrostep
  :commands (macrostep-expand)

  :init
  (keymap-set emacs-lisp-mode-map "C-c m E" #'macrostep-expand))

(after! 'lispy
  (when (boundp 'lispy-outline)
    (setq lispy-outline
      (concat
        ;; `lispy-mode' requires `lispy-outline' start with ^
        (unless (string-prefix-p "^" +emacs-lisp-outline-regexp) "^")
        +emacs-lisp-outline-regexp))))

(provide 'init-lang-emacs-lisp)
;;; init-lang-emacs-lisp.el ends here
