;;; init-external.el --- Third-party function and macro libraries -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/dotfield
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

;;  Defines package recipies and feature configuration for external function and
;;  macro libraries.

;;; Code:

;;
;;; --- llama :: compact syntax for short lambda ---
;;  <https://git.sr.ht/~tarsius/llama>
;;
;;  > The ## macro, whose signature is (## FN &rest args), expands to a lambda
;;  > expressions, which wraps around its arguments.
;;
;;  Example:
;;
;;  (##foo % (bar %3) %*)
;;  => (lambda (% _%2 %3 &rest %*)
;;       (foo % (bar %3) %*))
(elpaca-use-package (llama :host sourcehut :repo "tarsius/llama")
  :commands (##))

(provide 'lib-external)
;;; lib-external.el ends here
