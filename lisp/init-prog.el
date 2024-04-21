;;; init-prog.el --- Programming mode support        -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

;; When programming happens, here we are.

;;; Code:

;;; Requirements

(require 'ceamx-keymaps)

(require 'lib-common)
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

;;
;;; Defaults


;;
;;; Packages

;;; hl-todo :: <https://github.com/tarsius/hl-todo>
;;  Highlight TODO and other codetags in comments and strings
;;  <https://peps.python.org/pep-0350/#specification>
(use-package hl-todo
  :commands (hl-todo-mode)
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))

;;; ~dumb-jump~ :: <https://github.com/jacktasia/dumb-jump>
;;  "zero-configuration" jump-to-definition package with support for many langs
(use-package dumb-jump
  :autoload dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  ;; FIXME: make conditional if ripgrep available
  (setopt dumb-jump-force-searcher 'rg))

(after! (hydra dumb-jump)
  ;; TODO: needs binding
  ;; via <https://github.com/jacktasia/dumb-jump?tab=readme-ov-file#hydra-for-effieciency>
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
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

;;; Keybindings

(keymap-global-set "M-C" ceamx-code-map)

(define-keymap :keymap ceamx-code-map
  "d" #'xref-find-definitions)

(provide 'init-prog)
;;; init-prog.el ends here
