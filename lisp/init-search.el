;;; init-search.el --- Search                        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

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

;; Configuration for search-related utilities like `isearch' and
;; `query-replace'.

;; See also `init-selection-consult', as `consult' often provides a frontend to
;; these utilities.

;;; Sources:

;; <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-isearch.el>

;;; Code:

(require 'ceamx-keymaps)

(require 'lib-common)

;;; `isearch' [builtin]

(use-feature! isearch
  :blackout

  :preface

  (defun ceamx/replace-symbol-at-point ()
    "Run `query-replace-regexp' for the symbol at point."
    (interactive)
    (require 'isearch)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp))

  :config

;;;; Definitions

  (defvar-keymap isearch-repeat-map
    :repeat t
    "s" #'isearch-repeat-forward
    "r" #'isearch-repeat-backward)

;;;; Customizations

  (setopt search-highlight t)
  (setopt isearch-lazy-highlight t)
  (setopt isearch-lazy-count t)
  (setopt lazy-count-prefix-format "[%s/%s] ")
  (setopt lazy-count-suffix-format nil)
  (setopt isearch-allow-scroll 'unlimited)

  ;; Allow extending search string by holding shift and using motion commands.
  (setopt isearch-yank-on-move 'shift)

  ;; TODO: monitor behavior
  ;;       specifically, it looks like that regexp will consider any
  ;;       non-alphanumeric character to be whitespace, which might be a bit much.
  ;; via <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-isearch.el>
  (setopt search-whitespace-regexp ".*?")
  (setopt isearch-lax-whitespace t)
  (setopt isearch-regexp-lax-whitespace nil)

;;;; Keybindings

  (define-keymap :keymap (current-global-map)
    "M-s M-o" #'multi-occur
    "M-s %" #'ceamx/replace-symbol-at-point)

  (define-keymap :keymap isearch-mode-map
    "M-<" #'isearch-beginning-of-buffer
    "M->" #'isearch-end-of-buffer
    "M-/" #'isearch-complete
    "M-w" #'isearch-yank-word-or-char

    "M-s <" #'isearch-beginning-of-buffer
    "M-s >" #'isearch-end-of-buffer

    "C-w" nil
    "M-e" nil)

  (keymap-set minibuffer-local-isearch-map "M-/" #'isearch-complete-edit))

;;; Efficiently replace targets in the buffer or context with `substitute'

;; <https://protesilaos.com/emacs/substitute>

(use-package substitute
  :commands (substitute-target-above-point
              substitute-target-below-point
              substitute-target-in-buffer
              substitute-target-in-defun)
  :defines (substitute-post-replace-functions)

  :init
  ;; Provide messages reporting on matches changed in the context.
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation)

  (define-keymap :keymap ceamx-replace-map
    "b" #'substitute-target-in-buffer
    "d" #'substitute-target-in-defun
    "r" #'substitute-target-above-point
    "s" #'substitute-target-below-point)

  :config
  (setopt substitute-hightlight t))

;;; Configure `re-builder', the builtin regular expression builder

;; <https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder>
;; <https://francismurillo.github.io/2017-03-30-Exploring-Emacs-rx-Macro/>

;; Unfortunately, `re-builder' itself is poorly-documented.

(use-feature! re-builder
  :commands (re-builder)
  :config
  ;; "string" => recommended: \\(foo\\\|bar\\)
  ;; "rx"     => recommended; advanced sexp regexp engine
  ;; "read"   => default, avoid: backslash hell
  (setopt reb-re-syntax 'string))

;;; Keybindings

(keymap-set search-map "r" '("replace..." . ceamx-replace-map))

(provide 'init-search)
;;; init-search.el ends here
