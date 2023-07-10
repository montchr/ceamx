;;; init-lang-elisp.el --- Emacs Lisp language configuration -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

(use-feature eldoc
  :hook (emacs-lisp-mode)
  :diminish eldoc-mode)


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; Elsa :: Emacs Lisp Static Analyser
;;  <https://github.com/emacs-elsa/Elsa>

;; TODO
;; (use-package elsa
;;   :defer t)


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; lispy :: "short and sweet LISP editing"
;;  <https://github.com/abo-abo/lispy>

(use-package lispy
  :diminish
  :defines (lispy-mode-map)
  :hook ((emacs-lisp-mode . lispy-mode))
  :bind ( :map lispy-mode-map
          ("C-a" . beginning-of-line)))

;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; package-lint :: A linting library for elisp package metadata
;;  <https://github.com/purcell/package-lint>

(use-package package-lint
  :commands (package-lint-current-buffer))

;; Flycheck integration
;; <https://github.com/purcell/flycheck-package>
(use-package flycheck-package
  :after (flycheck package-lint)
  :config
  (flycheck-package-setup)
  :hook (emacs-lisp-mode))

;; TODO: <https://github.com/jerrypnz/major-mode-hydra.el/tree/72bdce649245df276a3f49fb57f890c10fbf0a31>
;; (major-mode-hydra-define emacs-lisp-mode nil
;;                          ("Eval"
;;                           (("b" eval-buffer "buffer")
;;                            ("e" eval-defun "defun")
;;                            ("r" eval-region "region"))
;;                           "REPL"
;;                           (("I" ielm "ielm"))
;;                           "Test"
;;                           (("t" ert "prompt")
;;                            ("T" (ert t) "all")
;;                            ("F" (ert :failed) "failed"))
;;                           "Doc"
;;                           (("d" describe-foo-at-point "thing-at-pt")
;;                            ("f" describe-function "function")
;;                            ("v" describe-variable "variable")
;;                            ("i" info-lookup-symbol "info lookup"))))

(provide 'init-lang-elisp)
;;; init-lang-elisp.el ends here
