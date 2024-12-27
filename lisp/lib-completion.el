;;; lib-completion.el --- Completion helpers  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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
;;; Code:

;;;;; Pre-defined filters for ~consult-info~ searches

(require 'ceamx-lib)

(defvar devdocs-data-dir)

(declare-function consult-info "consult")

;; via <https://github.com/minad/consult?tab=readme-ov-file#help>
(defun ceamx/emacs-info ()
  "Search through Emacs info pages."
  (interactive)
  (consult-info "emacs" "efaq" "elisp" "cl"))

(defun ceamx/org-info ()
  "Search through the Org info page."
  (interactive)
  (consult-info "org"))

(defun ceamx/completion-info ()
  "Search through completion info pages."
  (interactive)
  (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                "corfu" "cape" "tempel"))

(defun ceamx/consult-info-dwim (&optional buffer)
  "Search Info manuals appropriate to BUFFER's major-mode."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((mode major-mode)
           (fn (pcase mode
                 ((pred (lambda (x) (memq x '(emacs-lisp-mode))))
                  #'ceamx/emacs-info)
                 ((pred (lambda (x) (memq x '(org-mode org-agenda-mode))))
                  #'ceamx/org-info)
                 (_ #'consult-info))))
      (command-execute fn))))
(defvar +dabbrev-friend-buffer-size-max (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned for dynamic abbreviations.")

(defun +dabbrev-friend-buffer-p (buf)
  "Whether to consider BUF a `dabbrev' friend buffer."
  (< (buffer-size buf) +dabbrev-friend-buffer-size-max))

(provide 'lib-completion)
;;; lib-completion.el ends here
