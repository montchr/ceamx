;;; init-ui-ef-themes.el --- Support for the Ef theme family  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

;; <https://protesilaos.com/emacs/ef-themes>

;;; Code:

;;; Requirements

(require 'config-ui)

;;; Configuration

(use-package ef-themes
  :ensure t
  :demand t
  :commands (ef-themes-select)

  :config
  (setopt ceamx-theme-default-dark 'ef-trio-dark)
  (setopt ceamx-theme-default-light 'ef-frost)

  (setopt ef-themes-to-toggle (list ceamx-theme-default-dark ceamx-theme-default-light))

  (setopt ef-themes-headings nil)
  ;; (setopt ef-themes-headings         ; read the manual's entry or the doc string
  ;;   '((0 variable-pitch light 1.9)
  ;;      (1 variable-pitch light 1.8)
  ;;      (2 variable-pitch regular 1.7)
  ;;      (3 variable-pitch regular 1.6)
  ;;      (4 variable-pitch regular 1.5)
  ;;      (5 variable-pitch 1.4)           ; absence of weight means `bold'
  ;;      (6 variable-pitch 1.3)
  ;;      (7 variable-pitch 1.2)
  ;;      (t variable-pitch 1.1)))
  ;; They are nil by default...
  ;; (setopt ef-themes-mixed-fonts t
  ;;         ef-themes-variable-pitch-ui t)

  ;; Disable all other themes to avoid awkward blending:
  ;; TODO: how does this help when using theme-buffet-mode
  (mapc #'disable-theme custom-enabled-themes)

  ;; The themes we provide are recorded in the `ef-themes-dark-themes',
  ;; `ef-themes-light-themes'.
  ;; Load the theme of choice:
  ;; (load-theme 'ef-elea-dark :no-confirm)
  ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
  ;; (ef-themes-select 'ef-winter)
  ;; OR use some other method of loading a theme in `init-ui-circadian'
  )

(provide 'init-ui-ef-themes)
;;; init-ui-ef-themes.el ends here
