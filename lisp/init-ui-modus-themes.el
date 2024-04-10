;;; init-ui-modus-themes.el ---Modus Themes initialization  -*- lexical-binding: t; -*-

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

;; <https://protesilaos.com/modus-themes/>

;;; Code:

(require 'lib-common)

(package! modus-themes
  (require 'modus-themes)

  (setopt modus-themes-italic-constructs t)
  (setopt modus-themes-bold-constructs nil)
  (setopt modus-themes-mixed-fonts t)
  (setopt modus-themes-variable-pitch-ui nil)
  (setopt modus-themes-disable-other-themes t)
  (setopt modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

  (setopt modus-themes-common-palette-overrides
          '(
            ;; Make the fringe invisible.
            (fringe unspecified)
            ;; Make line numbers less intense and add a shade of cyan
            ;; for the current line number.
            (fg-line-number-inactive "gray50")
            (fg-line-number-active cyan-cooler)
            (bg-line-number-inactive unspecified)
            (bg-line-number-active unspecifed)))

  (def-hook! +modus-themes-custom-faces-h ()
    'modus-themes-after-load-theme-hook
    "Configurate custom faces for the `modus-themes'."
    (modus-themes-with-colors
      (custom-set-faces
       ;; Add "padding" to the mode lines.
       `(mode-line ((,c :box (:line-width 10
                              :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width 10
                                       :color ,bg-mode-line-inactive)))))))

  ;; Do not extend `region' background past the end of the line.
  ;; <https://protesilaos.com/emacs/modus-themes#h:a5140c9c-18b2-45db-8021-38d0b5074116>
  (custom-set-faces
   '(region ((t :extend nil))))

  ;; NOTE: Loaded in `init-ui-circadian'.
  ;; (load-theme 'modus-vivendi-tinted :no-confirm)
  )

(provide 'init-ui-modus-themes)
;;; init-ui-modus-themes.el ends here
