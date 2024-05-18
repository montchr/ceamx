;;; init-treesitter.el --- Tree-Sitter support          -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
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
;;       packages have been installed so that ~treesit-auto~ it can override ~auto-mode-alist~.

;;  By default, Emacs plays it safe with tree-sitter language support so as not
;;  to override legacy mode file extension associations. This makes sense as a
;;  default, but it's a pain to have to override ~auto-mode-alist~ for every
;;  language individually.
;;
;;  ~treesit-auto~ is pretty smart about how it handles these behaviors; its
;;  readme provides more in-depth details.

;;  In short, ~global-treesit-auto-mode~ will:
;;
;;  - Automatically switch to <name>-ts-mode when the grammar for <name> is installed
;;  - Stick with <name>-mode if the grammar isn’t installed
;;  - Automatically install a grammar before opening a compatible file
;;  - Modify auto-mode-alist for tree-sitter modes

;;  See also <https://github.com/purcell/emacs.d/blob/master/lisp/init-treesitter.el>
;;  for a more manual approach.

;;; Code:

(require 'treesit)

(require 'ceamx-lib)

;;; Automatically use available ~treesit~ modes via ~treesit-auto~

;; <https://github.com/renzmann/treesit-auto>

;; NOTE: This package does *not* automatically manage mode-hook translation.
;; Those should be managed manually on a case-by-case basis. For example,
;; ~nix-ts-mode-hook~ does not currently inherit the value of ~nix-mode-hook~.
;; Some Tree-Sitter modes, however, still derive from their non-Tree-Sitter
;; predecessor, and so will also run that mode's hooks in addition to its own.

(package! treesit-auto
  (require 'treesit-auto)

  ;; Grammars should be installed via Nixpkgs.
  (setopt treesit-auto-install nil)

  (treesit-auto-add-to-auto-mode-alist 'all)

  (global-treesit-auto-mode))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
