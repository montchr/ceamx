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

(use-package lispy
  :config
  (dolist (mode cmx-lisp-mode-list)
    (let ((hook (intern (format "%S-hook" mode))))
      (add-hook hook (cmd! (lispy-mode 1)))))

  ;; Prevent `lispy' from inserting escaped quotes when already inside a string,
  ;; in favor of just moving past the closing quote as I would expect.
  (setopt lispy-close-quotes-at-end-p t)

  ;; TODO: Remove after <https://github.com/abo-abo/lispy/pull/619> (if ever?)
  ;; (keymap-unset lispy-mode-map "`" t) ; <- does not work. why not?
  (keymap-set lispy-mode-map "`"   #'self-insert-command)

  (keymap-set lispy-mode-map "M-v" nil))

;;
;;; `lispyville' :: <https://github.com/noctuid/lispyville>
;;

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

(provide 'init-lisp)
;;; init-lisp.el ends here
