;;; lib-help.el --- Helpers for help and documentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: local, help

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

;; We all need help sometimes.

;;; Code:

(require 'ceamx-lib)

(defvar devdocs-data-dir)

(declare-function consult-info "consult")

;;; Functions

;;;; Pre-defined filters for ~consult-info~ searches

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

;;;; ~devdocs~ support

(defun +devdocs--doc-directory-exists-p (slug)
  "Whether the directory for the doc SLUG exists."
  (file-directory-p (expand-file-name slug devdocs-data-dir)))

(defun +devdocs--doc-installed-p (slug)
  "Whether the document named SLUG is installed.
Installation can be defined as whether there exists a metadata
file inside a directory named SLUG within `devdocs-data-dir'."
  (defvar devdocs-data-dir)
  (let ((file (expand-file-name (concat slug "/metadata") devdocs-data-dir)))
    (file-exists-p file)))

(defun +devdocs-maybe-install (doc)
  "Install the `devdocs' documentation set for DOC if not already installed.
DOC is as in `devdocs-install'."
  (declare-function devdocs-install "devdocs")
  (unless (+devdocs--doc-installed-p doc)
    (devdocs-install doc)))

(defun +devdocs-maybe-install-docs (docs)
  "Install each `devdocs' documentation set in DOCS if not already installed.
DOCS is a quoted list of `devdocs' documentation identifiers as
accepted by `+devdocs-maybe-install'."
  (dolist (doc docs)
    (+devdocs-maybe-install doc)))

;; FIXME: return t if exists, whatever if new, otherwise throw
(defun ceamx/devdocs-maybe-install (doc)
  "Install the `devdocs' documentation set for DOC if not already installed.
DOC is as in `devdocs-install'."
  ;; TODO: prompt for selecting from available docs (see `devdocs-install')
  (interactive "s")
  (+devdocs-maybe-install doc))

(provide 'lib-help)
;;; lib-help.el ends here
