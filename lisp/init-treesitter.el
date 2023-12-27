;;; init-treesitter.el --- Tree-Sitter support          -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: languages, local

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

;;;; Requirements:

;; Emacs 29+ built with tree-sitter support.
;; If using Nix, this is handled by default, done.
;;
;; Linux: `pkgs.emacs29' or `emacs-overlay.packages.emacs-unstable-pgtk'
;; Darwin: `pkgs.emacs29-macport'

;; Add these to `programs.emacs.extraPackages':
;;
;;  - `epkgs.treesit-auto'
;;  - `epkgs.treesit-grammars.with-all-grammars'

;;;; Mode Association:

;; NOTE: This feature is intended to be loaded *after* all other language
;;       packages have been installed so that `treesit-auto' it can override `auto-mode-alist'.

;;  By default, Emacs plays it safe with tree-sitter language support so as not
;;  to override legacy mode file extension associations. This makes sense as a
;;  default, but it's a pain to have to override `auto-mode-alist' for every
;;  language individually.
;;
;;  `treesit-auto' is pretty smart about how it handles these behaviors; its
;;  readme provides more in-depth details.

;;  In short, `global-treesit-auto-mode' will:
;;
;;  - Automatically switch to <name>-ts-mode when the grammar for <name> is installed
;;  - Stick with <name>-mode if the grammar isn’t installed
;;  - Automatically install a grammar before opening a compatible file
;;  - Modify auto-mode-alist for tree-sitter modes

;;  See also <https://github.com/purcell/emacs.d/blob/master/lisp/init-treesitter.el>
;;  for a more manual approach.

;;; Code:

(use-feature! treesit
  :demand t)

;;; `treesit-auto' <https://github.com/renzmann/treesit-auto>
(use-package treesit-auto
  :after (treesit)
  :commands (global-treesit-auto-mode)
  ;; FIXME: Package has bad autoloads
  ;; <https://github.com/renzmann/treesit-auto/issues/44>
  :autoload (treesit-auto-add-to-auto-mode-alist)

  :config
  ;; Grammars should be installed via Nixpkgs.
  (setopt treesit-auto-install nil)

  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))


(provide 'init-treesitter)
;;; init-treesitter.el ends here
