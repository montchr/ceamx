;;; init-clippy.el --- Help -*- lexical-binding: t; -*-

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

;; "It looks like you're writing an Emacs. Would you like help?"

;;; Code:

(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)))


(use-package flycheck
  ;; TODO: enable -- but not in `emacs-lisp-mode' due to endless issues with
  ;; undefined variables and functions etc.
  :disabled)



;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; GNU Hyperbole
;;

;; FIXME: causes startup errors!
;; (use-package hyperbole
;;   :config
;;   (hyperbole-mode 1))

(provide 'init-clippy)
;;; init-clippy.el ends here
