;;; init-lang-elisp.el --- Emacs Lisp language configuration -*- lexical-binding: t -*-

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

(defvar cmx-lisp-mode-list '(emacs-lisp-mode lisp-mode)
  "Supported Lisps.")

(use-feature eldoc
  :hook (emacs-lisp-mode)
  :diminish eldoc-mode)

;;
;;; `lispy' :: <https://github.com/abo-abo/lispy>
;;
;;  <http://oremacs.com/lispy/>

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
  (keymap-set lispy-mode-map "`" #'self-insert-command))

;;
;;; `lispyville' :: <https://github.com/noctuid/lispyville>
;;

(use-package lispyville
  :after lispy
  :defines lispyville-key-theme

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


(provide 'init-lang-elisp)
;;; init-lang-elisp.el ends here
