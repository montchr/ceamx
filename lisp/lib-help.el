;;; lib-help.el --- Helpers for help and documentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

(require 'lib-common)

;;
;;; Functions

;;;; `devdocs' support

;; FIXME: return t/nil
(defun ceamx-devdocs-doc-installed-p (doc)
  "Whether `devdocs' documentation set DOC is installed."
  ;; TODO: return value or t? check elisp docs
  (when (fboundp 'devdocs--doc-title)
    ;; FIXME: prevent errors, which *will* happen when doc is not present
    ;; <https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html>
    (devdocs--doc-title doc)))

;; FIXME: return t if exists, whatever if new, otherwise throw
(defun ceamx/devdocs-ensure-doc (doc)
  "Install the `devdocs' documentation set for DOC if not already installed.
DOC is a `devdocs' documentation identifier of the form accepted
by `devdocs-install'."
  ;; TODO: prompt for selecting from available docs (see `devdocs-install')
  (interactive "s")
  (devdocs-install doc)
  ;; FIXME: `ceamx-devdocs-doc-installed-p'
  (noop!
    (unless (ceamx-devdocs-doc-installed-p doc)
      (devdocs-install doc))
    ;; TODO: is there another way without calling twice?
    (ceamx-devdocs-doc-installed-p doc)))

(defun ceamx/devdocs-ensure-docs (docs)
  "Install each `devdocs' documentation set in DOCS if not already installed.
DOCS is a quoted list of `devdocs' documentation identifiers as
accepted by `ceamx/devdocs-ensure-doc'."
  ;; FIXME: what is the proper code for a list? "x" didn't work
  (interactive)
  (dolist (doc docs)
    (ceamx/devdocs-ensure-doc doc)))

(provide 'lib-help)
;;; lib-help.el ends here
