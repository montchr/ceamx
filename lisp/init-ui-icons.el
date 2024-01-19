;;; init-ui-icons.el --- Configuration for user interface icons  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords:

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

;; Some packages look nicer with icons. The icons will be configured here.

;; Icon fonts should be installed via Nix.

;;; Code:

(use-package all-the-icons
  :if (display-graphic-p))

(use-package nerd-icons
  :demand t
  :init
  (setopt nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package svg-lib
  :demand t)

(use-package kind-icon
  :after (svg-lib corfu)
  :commands (kind-icon-reset-cache)
  :functions (kind-icon-margin-formatter)

  :init
  (defvar corfu-margin-formatters)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
  (add-hook 'after-enable-theme-hook #'kind-icon-reset-cache)

  :config
  (setopt kind-icon-use-icons (display-graphic-p))
  (setopt kind-icon-default-face 'corfu-default))


(provide 'init-ui-icons)
;;; init-ui-icons.el ends here
