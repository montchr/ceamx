;;; init-lang-misc.el --- Miscellaneous language support -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;  Configuration for miscellaneous languages that don't warrant
;;  their own initialization files.

;;; Code:

(use-package apache-mode
  :defer t)

(use-package just-mode
  :defer t)

(use-package robots-txt-mode
  :defer t)

(use-package vimrc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.(idea)?vim\\(rc\\)?\\'" . vimrc-mode)))


(provide 'init-lang-misc)
;;; init-lang-misc.el ends here
