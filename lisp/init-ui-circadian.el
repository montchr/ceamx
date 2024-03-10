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

(require 'cal-dst)

(require 'config-ui)
(require 'lib-common)

;;; Sunrise/sunset interval via `solar' and `circadian'

(use-feature! solar
  :when (eq 'solar ceamx-theme-circadian-interval)

  :config
  (setopt calendar-latitude 39.968)
  (setopt calendar-longitude -75.133))

(use-package circadian
  :when (eq 'solar ceamx-theme-circadian-interval)
  :ensure t
  :demand t
  :after solar

  :commands (circadian-setup)

  :init
  (setopt circadian-themes `((:sunrise . ,ceamx-theme-default-light)
                             (:sunset . ,ceamx-theme-default-dark)))
  (circadian-setup))

;;;; Phase-of-day interval via `theme-buffet'

;; <https://git.sr.ht/~bboal/theme-buffet>

;; > The theme-buffet package arranges to automatically change themes during
;; > specific times of the day or at fixed intervals. The collection of themes
;; > is customisable, with the default options covering the built-in Emacs
;; > themes as well as Prot's modus-themes and ef-themes.

(use-package theme-buffet
  :ensure t
  :demand t
  :when (eq 'buffet ceamx-theme-circadian-interval)

  :commands (theme-buffet-modus-ef)
  :defines (theme-buffet-menu)

  :init

  ;; Take Daylight Savings Time offset into account for time period boundaries.
  ;; I am not sure why the additional `1+' is necessary, but this is copied from
  ;; the author's recommendation.
  ;; via <https://git.sr.ht/~bboal/theme-buffet/tree/06f1be349e9c3d124520b18742911307de9abda3/item/theme-buffet.el#L68-70>
  (setopt theme-buffet-time-offset (1+ (/ (cadr (calendar-current-time-zone)) 60)))

  (setopt theme-buffet-menu 'end-user)

  (setopt theme-buffet-end-user
          '(:night (;; ef-autumn
                     ef-duo-dark
                     ef-trio-dark
                     ef-night
                     ef-winter
                     ef-dark)
            :twilight (ef-bio
                       ef-cherie
                       modus-vivendi)
            :morning (ef-elea-light
                      ef-maris-light
                      ef-spring)
            :day (ef-frost
                  ef-light
                  ef-trio-light
                  modus-operandi)
            :afternoon (ef-cyprus
                        ef-arbutus
                        ef-day
                        ef-duo-light
                        ef-kassio
                        ef-melissa-light
                        ef-summer
                        modus-operandi-tinted)
            :evening (ef-elea-dark
                      ef-maris-dark
                      ef-melissa-dark
                      ef-symbiosis
                      ef-trio-dark
                      modus-vivendi-tinted)))

  :config

  (theme-buffet-end-user)

  ;; Activate some theme in the current period.
  (theme-buffet-a-la-carte))

(provide 'init-ui-circadian)
;;; init-ui-circadian.el ends here
