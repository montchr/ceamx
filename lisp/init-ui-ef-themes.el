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

(defvar ceamx-font-headings-style-alist)


(use-package ef-themes
  :ensure t
  :demand t
  :commands (ef-themes-select)

  :config
  (setopt ceamx-theme-default-dark 'ef-night)
  (setopt ceamx-theme-default-light 'ef-frost)

  (setopt ef-themes-to-toggle (list ceamx-theme-default-dark ceamx-theme-default-light))

  (setopt ef-themes-headings
          '((0 . (variable-pitch 1.4))
            (1 . (variable-pitch 1.3))
            (2 . (variable-pitch semibold 1.25))
            (3 . (variable-pitch medium 1.2))
            (4 . (variable-pitch regular 1.15))
            (5 . (variable-pitch semilight 1.05))
            (agenda-date . (semilight 1.5))
            (agenda-structure . (variable-pitch light 1.4))
            (t . (variable-pitch 0.9))))

  (setopt ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui nil)

  ;; Disable all other themes to avoid awkward blending
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
