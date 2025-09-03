;;; ceamx-lsp-just.el --- LSP-Mode client for Just   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: languages, tools, data, help, extensions

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

;;

;;; Code:

;;;; Requirements

(require 'lsp-mode)

(defgroup ceamx-lsp-just nil
  "Settings for the Just language server `lsp-mode' client."
  :group 'lsp-mode
  :link '(url-link "https://github.com/terror/just-lsp")
  :package-version '(lsp-mode . "8.0.0"))

;;;; Customization

(defcustom ceamx-lsp-just-server-command "just-lsp"
  "The binary (or full path to binary) which executes the server."
  :type 'string
  :group 'ceamx-lsp-just
  :package-version '(lsp-mode . "8.0.0"))

(defcustom ceamx-lsp-just-server-command-args '("--parser=remark-parse" "--stdio")
  "Command-line arguments for the Just LSP server."
  :type '(repeat string)
  :group 'ceamx-lsp-just
  :package-version '(lsp-mode . "8.0.0"))

;;;; Registration

(lsp-register-client
  (make-lsp-client
    :new-connection (lsp-stdio-connection
                      (lambda ()
                        (executable-find ceamx-lsp-just-server-command)))
    :major-modes '(just-mode just-ts-mode)
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                          (lsp-configuration-section "just"))))
    :server-id 'just))

(lsp-consistency-check ceamx-lsp-just)

(provide 'ceamx-lsp-just)
;;; ceamx-lsp-just.el ends here
