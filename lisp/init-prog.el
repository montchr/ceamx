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

;;
;;; `dumb-jump' :: <https://github.com/jacktasia/dumb-jump>
;;
;;  "zero-configuration" jump-to-definition package with support for many langs

(use-package dumb-jump
  :autoload dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setopt dumb-jump-force-searcher 'rg))

(after! [hydra dumb-jump]
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


;;; `repl-toggle' :: <https://git.sr.ht/~tomterl/repl-toggle>
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
