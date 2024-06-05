;;; init-lang-elisp.el --- Emacs Lisp development support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
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

;;; Code:

;;; Requirements

(require 'config-keys)
(require 'config-lisp)

(require 'ceamx-lib)
(require 'lib-lisp)

(declare-function blackout "blackout")

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

;;; Advices

;; via <https://github.com/doomemacs/doomemacs/blob/98d753e1036f76551ccaa61f5c810782cda3b48a/modules/lang/emacs-lisp/config.el#L124C1-L138C15>
(def-advice! +elisp-flymake-byte-compile-fix-load-path-a (orig-fn &rest args)
  :around #'elisp-flymake-byte-compile
  "Set load path for the `emacs-lisp' byte compilation `flymake' backend."
  (let ((elisp-flymake-byte-compile-load-path
         (append elisp-flymake-byte-compile-load-path load-path)))
    (apply orig-fn args)))

;; via <https://github.com/doomemacs/doomemacs/blob/98d753e1036f76551ccaa61f5c810782cda3b48a/modules/lang/emacs-lisp/config.el#L124C1-L138C15>
(def-advice! +emacs-lisp-append-value-to-eldoc-a (fn sym)
  :around #'elisp-get-var-docstring
  "Display variable value next to documentation in eldoc."
  (when-let (ret (funcall fn sym))
    (if (boundp sym)
      (concat ret " "
        (let* ((truncated " [...]")
                (print-escape-newlines t)
                (str (symbol-value sym))
                (str (prin1-to-string str))
                (limit (- (frame-width) (length ret) (length truncated) 1)))
          (format (format "%%0.%ds%%s" (max limit 0))
            (propertize str 'face 'warning)
            (if (< (length str) limit) "" truncated))))
      ret)))

;;; Keybinds

(keymap-global-set "<remap> <indent-pp-sexp>" #'ceamx/indent-last-sexp)

(define-keymap :keymap emacs-lisp-mode-map
  ceamx-keys-repl-toggle #'ielm

  "C-S-t" #'transpose-sexps)

(with-eval-after-load 'ielm
  (defvar ielm-map)
  (keymap-set ielm-map ceamx-keys-repl-toggle #'quit-window))

;;; Packages

;;;; ~eros~ :: <https://github.com/xiongtx/eros>

;;  Evaluation Result OverlayS for Emacs Lisp

(use-package eros
  :commands (eros-mode eros-eval-last-sexp)
  :init
  (add-hook 'emacs-lisp-mode-hook #'eros-mode)
  (keymap-set emacs-lisp-mode-map "<remap> <eval-last-sexp>" #'eros-eval-last-sexp)

  (use-feature! lispy
    :autoload (lispy-define-key)
    :config
    (def-hook! +lispy-use-eros-eval-h () 'lispy-mode-hook
      "Use `eros-eval-last-sexp' in place of `lispy-eval' bindings."
      ;; FIXME: there is currently no way to hide lispy-eval output.
      ;;        nil results in an error.
      ;;        because of this, output is duplicated in the minibuffer and the
      ;;        eros overlay...
      ;;
      ;; (setopt lispy-eval-display-style nil)
      (lispy-define-key lispy-mode-map "e" #'eros-eval-last-sexp))))

;;;; ~suggest~ :: <https://github.com/Wilfred/suggest.el>

;;  discover elisp functions that do what you want,
;;  brought to you by enumerative program synthesis

(use-package suggest
  :commands (suggest)
  :init
  (keymap-set emacs-lisp-mode-map "C-c S" #'suggest))

;;;; ~macrostep~ :: <https://github.com/emacsorphanage/macrostep>

;;  "interactive macro-expander for Emacs"

(use-package macrostep
  :commands (macrostep-expand)

  :preface
  ;; <https://github.com/joddie/macrostep/issues/11>
  ;; <https://github.com/emacsorphanage/macrostep/issues/8>
  (defun ceamx/macrostep-expand ()
    "Wrapper for `macrostep-expand' providing workaround for errors.
The original function fails in the presence of whitespace after a sexp."
    (interactive)
    (when (and (= ?\n (char-after))
            (= (point) (cdr (bounds-of-thing-at-point 'sexp))))
      (backward-char))
    (macrostep-expand))

  :init
  (keymap-set emacs-lisp-mode-map "C-c x" #'ceamx/macrostep-expand))

;;; Install ~xr~ to convert string regexps to ~rx~ forms

;; <https://github.com/mattiase/xr>

;; TODO: keybindings...

(package! xr)

(provide 'init-lang-elisp)
;;; init-lang-elisp.el ends here
