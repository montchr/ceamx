;;; ceamx-modaled.el --- Ceamx modal editing         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: local, convenience, extensions

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

;; Modal keyboard editing states with `bray-mode'.

;;; Code:

;;;; Requirements

(require 'bray)

;;;; Customization

(defgroup ceamx-modaled nil
  "Group for `ceamx-modaled' modal editing."
  :group 'ceamx)

;;;; Keymaps

(defvar-keymap ceamx-modaled-state-normal-map
  :full t
  :suppress t

  "i" (lambda () (interactive) (bray-state-stack-push 'insert))

  "h" #'backward-char
  "j" #'next-line
  "k" #'previous-line
  "l" #'forward-char

  ;; sexp traversal
  "{" #'puni-beginning-of-sexp
  "}" #'puni-end-of-sexp
  "(" #'puni-syntactic-backward-punct
  ")" #'puni-syntactic-forward-punct
  "[" #'puni-backward-sexp-or-up-list
  "]" #'puni-forward-sexp-or-up-list

  "SPC" #'puni-expand-region
  "S-SPC" #'puni-contract-region

  ;; sexp manipulation
  "R" #'puni-raise
  "<" #'puni-barf-forward
  ">" #'puni-slurp-forward
  "C-<" #'puni-barf-backward
  "C->" #'puni-slurp-backward)

(defvar-keymap ceamx-modaled-state-insert-map
  :full t
  "<escape>" #'bray-state-stack-pop)

;;;; Functions


;;;; Commands

;;;###autoload
(defun ceamx-modaled/enable-bray-mode-maybe ()
  (interactive)
  (when (and (not (minibufferp))
          (not (derived-mode-p 'special-mode)))
    (bray-mode 1)))

;;;###autoload
(defun ceamx-modaled/insert-state ()
  (interactive)
  (bray-state-stack-push 'insert))

;;;; Modes

;;;###autoload
(define-minor-mode ceamx-modaled-mode
  "Minor mode to enable modal editing with `ceamx-modaled'."
  :group 'ceamx-modaled
  :global t
  :lighter " Modaled"
  (if ceamx-modaled-mode
    (progn
      (add-hook 'after-change-major-mode-hook #'ceamx-modaled/enable-bray-mode-maybe)
      (ceamx-modaled/enable-bray-mode-maybe))
    (remove-hook 'after-change-major-mode-hook #'ceamx-modaled/enable-bray-mode-maybe)
    (bray-mode -1)))

(provide 'ceamx-modaled)
;;; ceamx-modaled.el ends here
