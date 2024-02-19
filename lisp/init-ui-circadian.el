;;; init-ui-circadian.el --- Support for sunrise/sunset adjustments  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

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

;;

;;; Code:

(require 'lib-common)

(require 'config-ui)

(use-feature! solar
  :config
  (setopt calendar-latitude 39.968)
  (setopt calendar-longitude -75.133))

(use-package circadian
  :ensure t
  :demand t
  :after solar
  :commands (circadian-setup)

  :init
  (setopt circadian-themes `((:sunrise . ,ceamx-ui-theme-light)
                             (:sunset . ,ceamx-ui-theme-dark)))
  (circadian-setup))

(provide 'init-ui-circadian)
;;; init-ui-circadian.el ends here