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

;;  Configuration for working with Lisps of all kinds.

;;; Code:

(require 'config-lisp)

;;
;;; Mode hooks
;;

;; For managing load order, especially concerning visual enhancements.
;; Apply to hooks on individual Lisp modes in their respective files.

(defun cmx-prog--lisp-init-h ()
  "Initialize defaults for Lisp programming modes."

  ;; FIXME: move to appropriate place
  ;; FIXME: `pp-buffer' is broken (at least for elisp)
  ;;        <https://mail.gnu.org/archive/html/emacs-diffs/2023-07/
  (setopt pp-max-width fill-column)
  (setopt pp-use-max-width t)

  (after! 'smartparens
    (smartparens-strict-mode 1))

  (after! 'rainbow-delimiters
    (rainbow-delimiters-mode 1))

  ;; `highlight-function-calls-mode' should be invoked after other highlighters
  ;; (e.g. `rainbow-delimiters-mode'), according to its readme.
  (after! 'highlight-function-calls
    (highlight-function-calls-mode 1))

  (after! 'lispy
    (lispy-mode 1))

  (after! 'lispyville
    (lispyville-mode 1)))

(setq cmx-prog-lisp-init-hook 'cmx-prog--lisp-init-h)

(defun cmx-prog--interactive-lisp-init-h ()
  "Initialize defaults for Lisp shells and other interactive modes."
  (after! 'smartparens
    (smartparens-strict-mode +1))
  (after! 'rainbow-delimiters
    (rainbow-delimiters-mode +1))
  (whitespace-mode -1))

(setq cmx-prog-interactive-lisp-init-hook 'cmx-prog--interactive-lisp-init-h)

;; Always use 2-space indentation in Lisps.
(dolist (sym '(add-function advice-add plist-put))
  (put sym 'lisp-indent-function 2))

;;; `lispy' :: <https://github.com/abo-abo/lispy>
(use-package lispy
  :init
  (defun cmx-init-lispy-in-eval-expression-h ()
    "Enable `lispy-mode' in the minibuffer for `eval-expression'."
    (lispy-mode +1)
    ;; When `lispy-key-theme' has `parinfer', the TAB key doesn't do
    ;; completion, neither (kbd "<tab>"/"TAB"/"C-i")/[tab]/"\C-i" works in
    ;; terminal as tested so remapping is used as a workaround
    (local-set-key (vector 'remap (lookup-key lispy-mode-map (kbd "TAB"))) #'completion-at-point))
  (add-hook 'eval-expression-minibuffer-setup-hook #'cmx-init-lispy-in-eval-expression-h)

  :config
  (dolist (mode cmx-lisp-mode-list)
    (let ((hook (intern (format "%S-hook" mode))))
      ;; FIXME: just add `lispy-mode' directly jeez
      (add-hook hook (cmd! (lispy-mode +1)))))

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
  :after (evil lispy)
  :defines (lispyville-key-theme)

  :init
  ;; NOTE: `setopt' throws warning on mismatched type
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
  (lispyville-set-key-theme)

  (add-hook! 'evil-escape-inhibit-functions
    (defun +lispy-inhibit-evil-escape-fn ()
       (and lispy-mode (evil-insert-state-p)))))


(provide 'init-lisp)
;;; init-lisp.el ends here
