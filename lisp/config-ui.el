;;; config-ui.el --- General UI settings             -*- lexical-binding: t; -*-

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

;;; Code:

(defconst ceamx-gnome-ui-namespace "org.gnome.desktop.interface")

(defconst ceamx-theme-buffet-dark-periods
  '(:night :twilight :evening))

(defconst ceamx-theme-buffet-light-periods
  '(:morning :day :afternoon))

(defcustom ceamx-theme-default-light 'modus-operandi-tinted
  "The default light theme."
  :group 'ceamx
  :type 'symbol)

(defcustom ceamx-theme-default-dark 'modus-vivendi-tinted
  "The default dark theme."
  :group 'ceamx
  :type 'symbol)

(defcustom ceamx-modeline-provider 'doom
  "Modeline provider to load.
Valid values are the symbols `doom', `nano', and `telephone'
which reference the `doom-modeline', `nano-modeline', and
`telephone-line' modules respectively.

A nil value will not load any modeline customizations (use Emacs
with its default modeline)."
  :group 'ceamx
  :type '(choice :tag "Modeline to load" :value doom
           (const :tag "The `doom-modeline' module" doom)
           (const :tag "The `nano-modeline' module" nano)
           (const :tag "The `telephone-line' module" telephone)
           (const :tag "Do not load a modeline module" nil)))

(defcustom ceamx-theme-family 'modus
  "Set of themes to load.
Valid values are the symbols `ef', `modus', and `standard', which
reference the `ef-themes', `modus-themes', and `standard-themes',
respectively.

A nil value does not load any of the above (use Emacs without a
theme)."
  :group 'ceamx
  :type '(choice :tag "Set of themes to load" :value modus
                 (const :tag "The `ef-themes' module" ef)
                 (const :tag "The `modus-themes' module" modus)
                 (const :tag "The `standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom ceamx-theme-circadian-interval 'buffet
  "The circadian theme switching interval.
Value may be either `period' or `solar', corresponding
respectively to period-based switching with `theme-buffet' or
sunrise/sunset toggling from the combination of the `solar'
library and the `circadian' package."
  :group 'ceamx
  :type '(choice :tag "Circadian theme switching interval" :value solar
          (const :tag "Time periods via `theme-buffet'" :value buffet)
          (const :tag "Sunrise or sunset via `solar' and `circadian'" :value solar)))

(defcustom ceamx-font-height-multiplier 1.0
  "Multiplier for display font size.
Intended for use as a per-system (or, ideally, per-display)
accommodation for varying pixel densities."
  :group 'ceamx
  :type '(float))

(provide 'config-ui)
;;; config-ui.el ends here
