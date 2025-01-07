;;; lib-ui.el --- Appearance helper functions  -*- lexical-binding: t;  -*-

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

;; Function to load a random theme from the specified periods :lib:


(declare-function theme-buffet--load-random "theme-buffet")

(defun +theme-buffet--load-random-from-periods (periods)
  "Load a random theme from the specified `theme-buffet' PERIODS.
PERIODS can be a single keyword or list of keywords. Each keyword
must be a valid `theme-buffet' period as defined in
`theme-buffet--keywords'."
  (let ((period (if (listp periods) (seq-random-elt periods) periods)))
    (theme-buffet--load-random period)))

;; Desktop environment integration :lib:graphical:
;; :PROPERTIES:
;; :header-args: :tangle lisp/lib-ui.el
;; :END:


(require 'ceamx-lib)

;; Integration with the GNOME/GTK/GSettings color scheme


(defun ceamx-ui/gsettings-set-theme (theme)
  "Set the GNOME/GTK theme to THEME."
  ;; FIXME: prompt with completion
  (interactive "s")
  (let* ((value (pcase theme
                  ((rx (optional "prefer-") "dark")
                   "prefer-dark")
                  ((rx (optional "prefer-") "light")
                   "prefer-light")
                  (_ "prefer-dark")))
         (cmd (format "gsettings set %s color-scheme %s"
                      ceamx-ui-gsettings-ui-namespace
                      value)))
    (shell-command cmd)))

(defun ceamx-ui/gsettings-dark-theme ()
  "Enable the dark GNOME/GTK theme."
  (interactive)
  (ceamx-ui/gsettings-set-theme "dark"))

(defun ceamx-ui/gsettings-light-theme ()
  "Enable the light GNOME/GTK theme."
  (interactive)
  (ceamx-ui/gsettings-set-theme "light"))

;; Integration with Kitty terminal theme :tty:


;; FIXME: error open /dev/tty: no such address or device
(defun ceamx-ui-kitty-set-theme (polarity)
  "Set the Kitty terminal emulator colors to POLARITY.
POLARITY is a string matching either \"light\" or \"dark\"."
  (shell-command
   (format "kitty @set-colors -a -c $KITTY_CONFIG_DIRECTORY/theme-%s.conf"
           polarity)))

;; ~ceamx-ui-load-theme~: function to cleanly load a theme

;; Similar to the theme-family-specific ~modus-themes-load-theme~.


(defun ceamx-ui-load-theme (theme)
  "Load THEME after resetting any previously-loaded themes."
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (load-theme theme :no-confirm))

;; Commands to load a preferred light or dark Emacs theme


(defun ceamx-ui/load-dark-theme ()
  "Load a random dark theme."
  (interactive)
  (pcase ceamx-ui-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-ui-theme-buffet-dark-periods))
    (_
     (load-theme ceamx-ui-theme-dark :no-confirm))))

(defun ceamx-ui/load-light-theme ()
  "Load a random light theme."
  (interactive)
  (pcase ceamx-ui-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-ui-theme-buffet-light-periods))
    (_
     (load-theme ceamx-ui-theme-light :no-confirm))))

;; Commands to globally set a preferred light or dark theme


(defun ceamx-ui/light ()
  "Activate a light theme globally."
  (interactive)
  (ceamx-ui/gsettings-light-theme)
  ;;(ceamx-ui-kitty-set-theme "light")
  (ceamx-ui/load-light-theme))

(defun ceamx-ui/dark ()
  "Activate a dark theme globally."
  (interactive)
  (ceamx-ui/gsettings-dark-theme)
  ;;(ceamx-ui-kitty-set-theme "dark")
  (ceamx-ui/load-dark-theme))

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

(provide 'lib-ui)
;;; lib-ui.el ends here
