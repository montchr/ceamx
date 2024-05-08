;;; init-selection-marginalia.el --- Marginalia configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>

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

;;  Enable rich completion annotations in the minibuffer.
;;
;;  <https://github.com/minad/marginalia>

;;; Code:

(require 'ceamx-lib)

(use-package marginalia
  :defines (marginalia-annotators)
  :init
  ;; <https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:37ACBBF7-989F-4A57-9454-06B79B8EB4F0>
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light
                                nil))

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  :config
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle)

  (after! projectile
    (add-to-list 'marginalia-command-categories '(projectile-find-file . file))))

(provide 'init-selection-marginalia)
;;; init-selection-marginalia.el ends here
