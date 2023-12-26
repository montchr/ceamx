;;; init-ui-modus-themes.el ---Modus Themes initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; <https://protesilaos.com/modus-themes/>

;;; Code:

(use-package modus-themes
  :demand t
  :config
  (setopt modus-themes-italic-constructs t)
  (setopt modus-themes-bold-constructs t)
  ;; (setopt modus-themes-common-palette-overrides
  ;;       '((fringe unspecified)))

  ;; (defun cmx-modus-themes--custom-faces ()
  ;;   (modus-themes-with-colors
  ;;     (custom-set-faces
  ;;      ;; Add "padding" to modeline.
  ;;      `(mode-line ((,c :box ( :line-width 10
  ;;                              :color ,bg-mode-line-active))))
  ;;      `(mode-line-inactive ((,c :box ( :line-width 10
  ;;                                       :color ,bg-mode-line-inactive)))))))
  ;; (add-hook 'modus-themes-after-load-theme-hook #'cmx-modus-themes--custom-faces)

  (load-theme 'modus-vivendi :no-confirm))

(provide 'init-ui-modus-themes)
;;; init-ui-modus-themes.el ends here
