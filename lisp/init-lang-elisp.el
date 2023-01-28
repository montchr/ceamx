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
;; (elpaca-use-package elsa
;;   :defer t)


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; lispy :: "short and sweet LISP editing"
;;  <https://github.com/abo-abo/lispy>

(elpaca-use-package lispy
  :diminish
  :defines (lispy-mode-map)
  :hook ((emacs-lisp-mode . lispy-mode))
  :bind ( :map lispy-mode-map
          ("C-a" . beginning-of-line)))

;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; package-lint :: A linting library for elisp package metadata
;;  <https://github.com/purcell/package-lint>

(elpaca-use-package package-lint
  :commands (package-lint-current-buffer))

;;
;;; flycheck-package
;;  <https://github.com/purcell/flycheck-package>
(elpaca-use-package flycheck-package
  :after (flycheck package-lint)
  :config
  (flycheck-package-setup)
  :hook (emacs-lisp-mode))

(provide 'init-lang-elisp)
;;; init-lang-elisp.el ends here