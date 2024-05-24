;;; config-prog.el --- User options for programming modes  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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

(defcustom ceamx-lsp-client 'eglot
  "The preferred LSP client."
  :group 'ceamx
  :type '(choice :tag "LSP client" :value eglot
          (const :tag "Eglot [builtin]" eglot)
          (const :tag "LSP-Mode" lsp-mode)))

(defvar ceamx-lsp-mode-cache-dir (file-name-as-directory (concat ceamx-var-dir "lsp")))
(setopt ceamx-lsp-client 'eglot)
(defconst ceamx-lang-php-extension-regexp "\\.\\(php\\|phtml\\)\\'"
  "Pattern matching files with PHP syntax.")

(defcustom ceamx-lang-php-major-mode-provider 'php-ts-mode
  "Which of several major-modes to provide PHP language support."
  :group 'ceamx
  :type '(choice :tag "PHP language support provider" :value php-mode
          (const :tag "`php-ts-mode': Tree-Sitter-based" php-ts-mode)
          (const :tag "`phps-mode': claims of intelligence" phps-mode)
          (const :tag "`php-mode': regular-degular" php-mode)))

(provide 'config-prog)
;;; config-prog.el ends here

(defvar xref-ignored-files '()
  "List of files to be ignored by `xref'.")
