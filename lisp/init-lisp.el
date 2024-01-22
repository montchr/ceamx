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

;; Add hooks to supported Lisp modes.
(dolist (mode ceamx-lisp-modes-list)
  (add-hook (derived-mode-hook-name mode) #'ceamx-lisp-init))

;; Always use 2-space indentation in Lisps.
;; TODO: i don't understand this copypasta
(dolist (sym '(add-function advice-add plist-put))
  (put sym 'lisp-indent-function 2))

;;
;;; Packages

(provide 'init-lisp)
;;; init-lisp.el ends here
