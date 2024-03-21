;;; init-lang-misc.el --- Miscellaneous language support -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

(require 'lib-common)

;; Language support for Apache Web Server configuration files


;; [[file:../config.org::*Language support for Apache Web Server configuration files][Language support for Apache Web Server configuration files:1]]
(package! apache-mode)
;; Language support for Apache Web Server configuration files:1 ends here

;; Language support for the Just task runner configuration files


;; [[file:../config.org::*Language support for the Just task runner configuration files][Language support for the Just task runner configuration files:1]]
(package! just-mode)
;; Language support for the Just task runner configuration files:1 ends here

;; Syntax highlighting for robots.txt files


;; [[file:../config.org::*Syntax highlighting for robots.txt files][Syntax highlighting for robots.txt files:1]]
(package! robots-txt-mode)
;; Syntax highlighting for robots.txt files:1 ends here

;; Language support for =vimrc= syntax


;; [[file:../config.org::*Language support for =vimrc= syntax][Language support for =vimrc= syntax:1]]
(package! vimrc-mode
  (add-to-list 'auto-mode-alist '("\\.(idea)?vim\\(rc\\)?\\'" . vimrc-mode)))
;; Language support for =vimrc= syntax:1 ends here

;; Language support for =kmonad= configuration

;; [[https://github.com/kmonad/kbd-mode][GitHub - kmonad/kbd-mode: Emacs mode for syntax highlighting kmonad's .kbd files.]]
;; [[https://github.com/kmonad/kmonad][GitHub - kmonad/kmonad: An advanced keyboard manager]]


;; [[file:../config.org::*Language support for =kmonad= configuration][Language support for =kmonad= configuration:1]]
(package! (kbd-mode :host github :repo "kmonad/kbd-mode"))
;; Language support for =kmonad= configuration:1 ends here

(provide 'init-lang-misc)
;;; init-lang-misc.el ends here
