;;; init-prog.el --- Programming mode support        -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
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

;; When programming happens, here we are.

;;; Code:

;;; Requirements

(require 'ceamx-lib)
(require 'lib-lisp)

;;; Hooks

(defun ceamx-prog-mode-init-h ()
  "Enable features and defaults useful in any `prog-mode'-derived major modes.
This function is especially useful to ensure functions are called
in a specific order. For this reason, condition checks on
`boundp'/`fboundp' are preferable to using `after!' or
`with-eval-after-load', as the latter may result in a
non-deterministic execution order.

Intended for use as a hook callback on `prog-mode-hook'."

  ;; `highlight-function-calls-mode' should be enabled after other highlighters
  ;; (e.g. `rainbow-delimiters-mode'), according to its readme.
  (when (fboundp 'highlight-function-calls-mode)
    (highlight-function-calls-mode 1)))

(add-hook 'prog-mode-hook #'ceamx-prog-mode-init-h)

;; Always find references of symbol at point.
(setopt xref-prompt-for-identifier nil)

;; Packages


;;; hl-todo :: <https://github.com/tarsius/hl-todo>
;;  Highlight TODO and other codetags in comments and strings
;;  <https://peps.python.org/pep-0350/#specification>
(use-package hl-todo
  :commands (hl-todo-mode)
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))

;;; ~dumb-jump~ :: <https://github.com/jacktasia/dumb-jump>

;;  "zero-configuration" jump-to-definition package with support for many langs

(package! dumb-jump
  ;; Add to end of list as a fallback for when there are no smart options.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100))

(after! (hydra)
  ;; via <https://github.com/jacktasia/dumb-jump?tab=readme-ov-file#hydra-for-effieciency>
  (defhydra ceamx-prog-dumb-jump-dispatch (:color blue :columns 3)
    "Jump (dumbly)"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

;;; ~rainbow-delimiters~ :: <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'ceamx-lisp-init-hook #'rainbow-delimiters-mode))

;;; ~repl-toggle~ :: <https://git.sr.ht/~tomterl/repl-toggle>
;;  Switch between `prog-mode' buffers and their corresponding REPLs.
;; FIXME: "cannot load"
;; (use-package repl-toggle
;;   :defines (rtog/mode-repl-alist)
;;   :config
;;   ;; TODO: `setopt'?
;;   (setq rtog/mode-repl-alist '( (php-mode . psysh)
;;                                 (emacs-lisp-mode . ielm)
;;                                 (nix-mode . nix-repl))))

(provide 'init-prog)
;;; init-prog.el ends here
