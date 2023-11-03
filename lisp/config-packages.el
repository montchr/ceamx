;;; config-packages.el --- Package variables and recipes  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

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

;; <https://codeberg.org/akib/emacs-popon>
(defvar popon-elpaca-order
  '(popon :host codeberg
          :repo "akib/emacs-popon"))

;; <https://codeberg.org/akib/emacs-corfu-terminal>
(defvar corfu-terminal-elpaca-order
  '(corfu-terminal :host codeberg
                   :repo "akib/emacs-corfu-terminal"))

;; <https://codeberg.org/akib/emacs-corfu-doc-terminal>
(defvar corfu-doc-terminal-elpaca-order
  '(corfu-doc-terminal :host codeberg
                       :repo "akib/emacs-corfu-doc-terminal"))

(defvar flymake-popon-elpaca-order
  '(flymake-popon :host codeberg
                  :repo "akib/emacs-flymake-popon"))

(provide 'config-packages)
;;; config-packages.el ends here
