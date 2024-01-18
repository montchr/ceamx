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

(require 'derived)

(require 'lib-common)
(require 'lib-lisp)

(require 'config-lisp)

;;
;;; Hooks
;;
;; For managing load order, especially concerning visual enhancements.
;;
;; Apply to hooks on individual Lisp modes in their respective files so they can
;; be disabled cleanly.

(add-hook 'ceamx-lisp-init-hook #'ceamx-enable-check-parens-on-save)

(after!! 'aggressive-indent
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; Add hooks to supported Lisp modes.
(dolist (mode ceamx-lisp-modes-list)
  (add-hook (derived-mode-hook-name mode) #'ceamx-lisp-init))

;; Always use 2-space indentation in Lisps.
;; TODO: i don't understand this copypasta
(dolist (sym '(add-function advice-add plist-put))
  (put sym 'lisp-indent-function 2))

;;
;;; Packages

;;; `lispy' :: <https://github.com/abo-abo/lispy>
(use-package lispy
  :commands (lispy-mode)

  :init
  (add-hook 'ceamx-lisp-init-hook #'lispy-mode)

  :config
  ;; Prevent `lispy' from inserting escaped quotes when already inside a string,
  ;; in favor of just moving past the closing quote as I would expect.
  (setopt lispy-close-quotes-at-end-p t)

  ;; TODO: Remove after <https://github.com/abo-abo/lispy/pull/619> (if ever?)
  ;; TODO: `keymap-unset' does not work here (with either nil or t) -- why not?
  ;; (keymap-unset lispy-mode-map "`" t)
  (keymap-set lispy-mode-map "`"  #'self-insert-command)

  (keymap-set lispy-mode-map "M-v" nil))

;; `lispyville' :: <https://github.com/noctuid/lispyville>
(use-package lispyville
  :disabled t
  :after (evil lispy)
  :defines (lispyville-key-theme)

  :init
  ;; Enable `lispyville' when `lispy' is enabled.
  (add-hook 'lispy-mode-hook #'lispyville-mode)

  ;; NOTE: `setopt' throws warning on mismatched type
  (setq lispyville-key-theme
    '((operators normal)
       c-w
       (prettify insert)
       (atom-movement t)
       slurp/barf-lispy
       additional
       additional-insert))


  :config
  (lispyville-set-key-theme)

  (add-hook! 'evil-escape-inhibit-functions
    (defun +lispy-inhibit-evil-escape-fn ()
      (and lispy-mode (evil-insert-state-p)))))


(provide 'init-lisp)
;;; init-lisp.el ends here
