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

;; TODO: investigate rigpa
;; FIXME: a bunch of warnings on emacs init
;;; `symex' :: <https://github.com/drym-org/symex.el>
;;  structural editing for symbolic expressions in lisp modes
(use-package symex
  :diminish

  ;; NOTE: the readme says settings should be added to `:custom', but i take
  ;; that to mean that they should be set in a customize-friendly way
  :config
  (setopt symex-modal-backend 'evil)

  (symex-initialize)

  (keymap-global-set "s-;" #'symex-mode-interface)

  ;; Rebind ESC when `symex-mode' is active, as it is preferred over Evil normal state.
  ;; via <https://github.com/drym-org/symex.el/issues/24>
  (evil-define-key '(normal insert) symex-mode-map
    (kbd "<escape>") 'symex-mode-interface)

  ;; In case Symex's reversal of the muscle-memory j/k bindings becomes too much.
  ;; See <https://github.com/drym-org/symex.el?tab=readme-ov-file#up-and-down>
  ;;
  ;; (setq symex--user-evil-keyspec
  ;;     '(("j" . symex-go-up)
  ;;       ("k" . symex-go-down)
  ;;       ("C-j" . symex-climb-branch)
  ;;       ("C-k" . symex-descend-branch)
  ;;       ("M-j" . symex-goto-highest)
  ;;       ("M-k" . symex-goto-lowest)))
  )

;; TODO
;; <https://github.com/bbatsov/prelude/blob/b57ff48e0985a6ef0f1ed9b279ec487c55982334/core/prelude-core.el#L147>
;; (defun prelude-wrap-with (s)
;;   "Create a wrapper function for smartparens using S."
;;   `(lambda (&optional arg)
;;      (interactive "P")
;;      (sp-wrap-with-pair ,s)))

(provide 'init-lisp)
;;; init-lisp.el ends here
