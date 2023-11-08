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

(defun cmx-emacs-lisp-defaults-h ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'cmx-lisp-prog-hook)
  (after! 'eldoc (eldoc-mode +1))
  (after! 'rainbow-mode (rainbow-mode +1))
  (setq mode-name "EL"))

(setq cmx-emacs-lisp-mode-hook 'cmx-emacs-lisp-defaults-h)

(add-hook 'emacs-lisp-mode-hook (##run-hooks 'cmx-emacs-lisp-mode-hook))

;;; `eldoc' (builtin)
(use-feature eldoc
  :commands (eldoc-mode)
  :diminish eldoc-mode
  :init (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

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

;;; `lispy' :: <https://github.com/abo-abo/lispy>
(use-package lispy
  :config
  (dolist (mode cmx-lisp-mode-list)
    (let ((hook (intern (format "%S-hook" mode))))
      (add-hook hook (cmd! (lispy-mode +1)))))

  ;; Prevent `lispy' from inserting escaped quotes when already inside a string,
  ;; in favor of just moving past the closing quote as I would expect.
  (setopt lispy-close-quotes-at-end-p t)

  ;; TODO: Remove after <https://github.com/abo-abo/lispy/pull/619> (if ever?)
  ;; (keymap-unset lispy-mode-map "`" t) ; <- does not work. why not?
  (keymap-set lispy-mode-map "`"   #'self-insert-command)

  (keymap-set lispy-mode-map "M-v" nil))

;;; `lispyville' :: <https://github.com/noctuid/lispyville>
(use-package lispyville
  :after (evil lispy)
  :defines (lispyville-key-theme)

  :init
  ;; via doom
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-lispy
          additional
          additional-insert))
  (add-hook 'lispy-mode-hook #'lispyville-mode)

  :config
  (lispyville-set-key-theme))

;;
;;; Keybinds
;;

(keymap-set emacs-lisp-mode-map "C-c C-c" #'eval-defun)

(provide 'init-lang-emacs-lisp)
;;; init-lang-emacs-lisp.el ends here
