;;; init-selection-orderless.el --- Orderless completion matching style configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>

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

;;  Emacs completion style that matches multiple regexps in any order.
;;
;;  <https://github.com/oantolin/orderless>

;; Refer to the following for sources info:
;; <https://github.com/oantolin/orderless?tab=readme-ov-file#style-dispatchers>
;; <https://github.com/minad/consult/wiki#minads-orderless-configuration>


;;; Code:

(require 'lib-common)

(package! orderless
  (require 'orderless)

  ;; Allow escaping space with backslash.
  (setopt orderless-component-separator #'orderless-escapable-split-on-space))

;;;; Query syntax additions through style dispatchers

;; (defun +orderless-flex-if-twiddle-dispatch (pattern _index _total)
;;   "Return `orderless-flex' if PATTERN ends in a tilde character.
;; PATTERN, stripped of its tilde character, will be dispatched as
;; argument to `orderless-flex'."
;;   (when (string-suffix-p "~" pattern)
;;     `(orderless-flex . ,(substring pattern 0 -1))))

;; (defun +orderless-first-initialism-dispatch (_pattern index _total)
;;   "Return `orderless-initialism' when PATTERN has the initial INDEX value."
;;   (if (= index 0) 'orderless-initialism))

;; (defun +orderless-not-if-bang-dispatch (pattern _index _total)
;;   "Return `orderless-not' when PATTERN begins with an exclamation mark.
;; PATTERN, stripped of its exclamation mark, will be dispatched as
;; argument to `orderless-not'."
;;   (cond
;;    ((equal "!" pattern)
;;     #'ignore)
;;    ((string-prefix-p "!" pattern)
;;     `(orderless-not . ,(substring pattern 1)))))

  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

;;;; Define custom completion syntax styles

(after! orderless
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))

(provide 'init-selection-orderless)
;;; init-selection-orderless.el ends here
