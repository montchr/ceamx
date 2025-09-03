;;; ceamx-devdocs.el --- Ceamx Devdocs extensions    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: extensions, docs, help

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
;;; Code:

;;;; Requirements

(autoload #'devdocs-install "devdocs")

(defvar devdocs-data-dir)

;;;; Functions

(defun +devdocs--doc-directory-exists-p (slug)
  "Whether the directory for the doc SLUG exists."
  (file-directory-p (expand-file-name slug devdocs-data-dir)))

(defun +devdocs--doc-installed-p (slug)
  "Whether the document named SLUG is installed.
Installation can be defined as whether there exists a metadata
file inside a directory named SLUG within `devdocs-data-dir'."
  (let ((file (expand-file-name (concat slug "/metadata") devdocs-data-dir)))
    (file-exists-p file)))

(defun +devdocs-maybe-install (doc)
  "Install the `devdocs' documentation set for DOC if not already installed.
DOC is as in `devdocs-install'."
  (unless (+devdocs--doc-installed-p doc)
    (devdocs-install doc)))

(defun +devdocs-maybe-install-docs (docs)
  "Install each `devdocs' documentation set in DOCS if not already installed.
DOCS is a quoted list of `devdocs' documentation identifiers as
accepted by `+devdocs-maybe-install'."
  (dolist (doc docs)
    (+devdocs-maybe-install doc)))

;;;; Commands

;; FIXME: return t if exists, whatever if new, otherwise throw
(defun ceamx/devdocs-maybe-install (doc)
  "Install the `devdocs' documentation set for DOC if not already installed.
DOC is as in `devdocs-install'."
  ;; TODO: prompt for selecting from available docs (see `devdocs-install')
  (interactive "s")
  (+devdocs-maybe-install doc))

(provide 'ceamx-devdocs)
;;; ceamx-devdocs.el ends here
