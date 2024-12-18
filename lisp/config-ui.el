;;; config-ui.el --- User options for Emacs appearance  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

;; Configure the preferred theme family :config:


(defcustom ceamx-ui-theme-family 'modus
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

;; Configure the preferred light and dark themes :config:


(defcustom ceamx-ui-theme-light 'modus-operandi-tinted
  "The default light theme."
  :group 'ceamx
  :type 'symbol)

(defcustom ceamx-ui-theme-dark 'modus-vivendi
  "The default dark theme."
  :group 'ceamx
  :type 'symbol)

;; TODO Define groupings of themes into dark/light or other qualities :config:

;; Needs expansion for more than just the Modus Themes.


(defvar ceamx-ui-dark-themes-list nil)

(defvar ceamx-ui-light-themes-list nil)

(after! modus-themes
  (appendq! ceamx-ui-dark-themes-list
            ;; `modus-vivendi' variants
            (seq-filter
             (lambda (sym) (string-prefix-p "modus-vivendi" (symbol-name sym)))
             modus-themes-items))

  (appendq! ceamx-ui-light-themes-list
            ;; `modus-operandi' variants
            (seq-filter
             (lambda (sym) (string-prefix-p "modus-operandi" (symbol-name sym)))
             modus-themes-items)))

;; Theme Phasing Schedule


(defcustom ceamx-ui-theme-circadian-interval 'solar
  "The circadian theme switching interval.
Value may be `period', `solar', or nil, corresponding
respectively to period-based switching with `theme-buffet' or
sunrise/sunset toggling from the combination of the `solar'
library and the `circadian' package.

A nil value means to disable automatic theme switching.
Theme-switching commands `ceamx/light' and `ceamx/dark' will
unconditionally use `ceamx-ui-theme-default-light' and
`ceamx-ui-theme-default-dark', respectively."
  :group 'ceamx
  :type '(choice :tag "Circadian theme switching interval" :value nil
          (const :tag "Time periods via `theme-buffet'" :value buffet)
          (const :tag "Sunrise or sunset via `solar' and `circadian'" :value solar)))

;; Define darkness and lightness and apply these classifications to circadian structures :config:


(defconst ceamx-ui-theme-buffet-dark-periods
  '(:night :twilight :evening))

(defconst ceamx-ui-theme-buffet-light-periods
  '(:morning :day :afternoon))

;; TODO Move to environment config


(defconst ceamx-ui-gsettings-ui-namespace "org.gnome.desktop.interface")

;; Define a custom setting to adjust font height multiplier :config:


(defcustom ceamx-font-height-multiplier 1.0
  "Multiplier for display font size.
Intended for use as a per-system (or, ideally, per-display)
accommodation for varying pixel densities."
  :group 'ceamx
  :type '(float))

;; Modeline :modeline:


(defcustom ceamx-modeline-provider nil
  "Modeline provider to load.
Valid values are the symbols `doom', `nano', and `telephone'
which reference the `doom-modeline', `nano-modeline', and
`telephone-line' modules respectively.

A nil value will not load any modeline customizations (use Emacs
with its default modeline)."
  :group 'ceamx
  :type '(choice :tag "Modeline to load" :value nil
          (const :tag "The `doom-modeline' module" doom)
          (const :tag "The `nano-modeline' module" nano)
          (const :tag "The `telephone-line' module" telephone)
          (const :tag "Do not load a modeline module" nil)))

;; Measurement constants


(defconst ceamx-inch-as-mm 25.4
  "One inch in millimeters.")

(defconst ceamx-pt-as-mm 0.353
  "One typographic point in millimeters.")

;; Function to get geometry attributes for the default display monitor


(defun ceamx-default-monitor-geometry ()
  "Return geometry for the first monitor in `display-monitor-attributes-list'."
  (let* ((first-monitor (car (display-monitor-attributes-list))))
    (alist-get 'geometry first-monitor)))

;; Function to get the pixel pitch for a frame


;; via <https://www.reddit.com/r/emacs/comments/7hzxb8/comment/dqywyqc/>
(defun ceamx-pixel-pitch (&optional frame)
  "Return the pixel pitch for FRAME in millimeters.
When FRAME is nil, the current frame will be used as default.

Pixel pitch is the distance from the center of a pixel to the
center of its adjacent pixel."
  (let ((monitor-attrs (frame-monitor-attributes frame)))
    (/ (float (nth 1 (assoc 'mm-size monitor-attrs)))
       (nth 3 (assoc 'geometry monitor-attrs)))))

;; Function to calculate font height scaling


(defun ceamx-font-height (number &optional multiplier)
  "Return a numeric font height based on NUMBER multiplied by MULTIPLIER.
NUMBER should be a whole number. MULTIPLIER should be a float.

If MULTIPLIER is nil, the value of `ceamx-font-height-multiplier'
will be used as default."
  (truncate (* number (or multiplier ceamx-font-height-multiplier))))

(provide 'config-ui)
;;; config-ui.el ends here
