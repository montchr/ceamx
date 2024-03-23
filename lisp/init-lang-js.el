;;; init-lang-js.el --- JavaScript/TypeScript language support improvements  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local, languages

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

;; Emacs supports JavaScript and TypeScript out of the box, however it needs some help.

;; TODO: try <https://github.com/llemaitre19/jtsx> but it's not in melpa or nixpkgs yet

;;; Code:

(require 'lib-common)

(defun ceamx-init-javascript-modes ()
  (setopt js-indent-level 2)

  (after! lsp-mode
    (lsp-deferred)
    (lsp-lens-mode)
    (dolist (hook '(lsp-format-buffer
                     lsp-organize-imports))
      (add-hook 'before-save-hook hook nil t))))

;; TODO: must happen before `treesit-auto' so it can override
;; (add-to-list 'auto-mode-alist '("\\.js\\'"     . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.pac\\'"    . js2-mode))
;; (add-to-list 'interpreter-mode-alist '("node"  . js2-mode))

(use-feature! typescript-ts-mode
  :init
  (add-hook 'typescript-ts-base-mode #'ceamx-init-javascript-modes))

(provide 'init-lang-js)
;;; init-lang-js.el ends here
