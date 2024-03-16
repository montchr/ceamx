;;; init-lisp.el --- Emacs Lisp language configuration -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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

;;  Configuration for working with Lisps of all kinds.

;;; Code:

;;;; Requirements

(require 'derived)
(require 'elpaca-autoloads)

(require 'lib-common)
(require 'lib-keys)
(require 'lib-lisp)

(require 'config-lisp)

;;;; Configure behavior for all Lisp modes with `ceamx-lisp-init-hook'

(add-hook 'ceamx-lisp-init-hook #'ceamx-enable-check-parens-on-save)

(after! 'flycheck
  (declare-function flycheck-mode "flycheck")
  (add-hook 'ceamx-lisp-init-hook #'flycheck-mode))

;; Add hooks to supported Lisp modes.
(dolist (mode ceamx-lisp-modes-list)
  (add-hook (derived-mode-hook-name mode) #'ceamx-lisp-init))

;; Use two-space indentation always.
(dolist (sym '(add-function advice-add plist-put))
  (put sym 'lisp-indent-function 2))

;; Prevent `calculate-lisp-indent' from indenting quoted lists as functions.
;; <https://emacs.stackexchange.com/a/52789/40956>
(advice-add #'calculate-lisp-indent :override #'ceamx-calculate-lisp-indent-a)

;;;; Configuration for `lispy', a structural expression editing experience

;; <https://oremacs.com/lispy/>
;; <https://github.com/abo-abo/lispy>

(package! lispy
  (add-hook 'ceamx-lisp-init-hook #'lispy-mode)

  (after! 'lispy
    ;; Prevent `lispy' from inserting escaped quotes when already inside a string,
    ;; in favor of just moving past the closing quote as I would expect.
    ;;
    ;; FIXME: This actually results in creating the quote pair *after* the
    ;; closing quote. "for example:"" "
    (setopt lispy-close-quotes-at-end-p t)

    (setopt lispy-completion-method 'default)

    (setopt lispy-eval-display-style 'message)

    ;; I have mixed feelings about this one because it can be jarring and easily
    ;; lead to mass-commenting expressions. Default is non-nil.
    (setopt lispy-move-after-commenting t)

    (keys! lispy-mode-map
      "M-j" nil                         ; shadows custom binding

      ;; via <https://github.com/abo-abo/lispy/pull/619>
      "`" #'self-insert-command)

    (after! 'outli
      ;; `outli-mode' overrides `lispy-mode' outline functionality, so it must
      ;; be activated afterwards.
      (add-hook 'ceamx-lisp-init-hook #'outli-mode))

    (after! 'macrostep
      (push 'macrostep lispy-compat))

    (after! 'popper
      (push "\\*lispy-message\\*" popper-reference-buffers))))

(provide 'init-lisp)
;;; init-lisp.el ends here
