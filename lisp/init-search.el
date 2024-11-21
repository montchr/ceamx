;;; init-search.el --- Searching and replacing features  -*- lexical-binding: t;  -*-

;; Copyright (c) 2023-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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
;;; Code:

(require 'ceamx-keymaps)

(require 'ceamx-lib)
(require 'lib-search)
(setopt find-library-include-other-files nil)
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

(after! isearch
  (blackout 'isearch)

  (defvar-keymap isearch-repeat-map
    :repeat t
    "s" #'isearch-repeat-forward
    "r" #'isearch-repeat-backward)

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
(package! substitute
  (define-keymap :keymap ceamx-replace-map
    "b" #'substitute-target-in-buffer
    "d" #'substitute-target-in-defun
    "r" #'substitute-target-above-point
    "s" #'substitute-target-below-point)

  (setopt substitute-hightlight t))

(after! substitute
  ;; Provide messages reporting on matches changed in the context.
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation))
(package! wgrep
  (setopt wgrep-auto-save-buffer t)
  (setopt wgrep-change-readonly-file t))
;; "string" => recommended: \\(foo\\\|bar\\)
;; "rx"     => recommended; advanced sexp regexp engine
;; "read"   => default, avoid: backslash hell
(setopt reb-re-syntax 'string)
(keymap-set search-map "r" '("replace..." . ceamx-replace-map))

(keymap-set minibuffer-local-map "C-c C-e" #'+vertico/embark-export-write)

(after! dired
  (keymap-set dired-mode-map "C-c C-e" #'wgrep-change-to-wgrep-mode))

(after! grep
  (keymap-set grep-mode-map "W" #'wgrep-change-to-wgrep-mode))

;; FIXME: wrong num args
;; the intention is to close the wgrep popup after abort/finish
;; (after! popper
;;   (advice-add #'wgrep-abort-changes :after #'popper-toggle)
;;   (advice-add #'wgrep-finish-edit :after #'popper-toggle))

(provide 'init-search)
;;; init-search.el ends here
