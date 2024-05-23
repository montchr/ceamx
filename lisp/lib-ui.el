;;; lib-ui.el --- Appearance helper functions  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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

;; via prot-emacs
(defun ceamx-theme-re-enable-in-frame (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`ceamx-theme-no-bright-flash'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))
(declare-function theme-buffet--load-random "theme-buffet")

(defun +theme-buffet--load-random-from-periods (periods)
  "Load a random theme from the specified `theme-buffet' PERIODS.
PERIODS can be a single keyword or list of keywords. Each keyword
must be a valid `theme-buffet' period as defined in
`theme-buffet--keywords'."
  (let ((period (if (listp periods) (seq-random-elt periods) periods)))
    (theme-buffet--load-random period)))
(require 'config-ui)
(require 'ceamx-lib)
(defun ceamx-gsettings-theme ()
  "Get the currently-active GNOME/GTK color scheme."
  (shell-command (format "gsettings get %s color-scheme"
                         ceamx-gsettings-ui-namespace)))

(defun ceamx-gsettings-theme-dark-p ()
  "Whether GNOME/GTK are using a theme with a dark color scheme."
  (string-match-p "dark" (ceamx-gsettings-theme)))

(defun ceamx/gsettings-set-theme (theme)
  "Set the GNOME/GTK theme to THEME."
  ;; FIXME: prompt with completion
  (interactive "s")
  (let* ((namespace ceamx-gsettings-ui-namespace)
         (value (pcase theme
                  ((rx (optional "prefer-") "dark")
                   "prefer-dark")
                  ((rx (optional "prefer-") "light")
                   "prefer-light")
                  (_ "prefer-dark")))
         (cmd (format "gsettings set %s color-scheme %s" namespace value)))
    (shell-command cmd)))

(defun ceamx/gsettings-dark-theme ()
  "Enable the dark GNOME/GTK theme."
  (interactive)
  (ceamx/gsettings-set-theme "dark"))

(defun ceamx/gsettings-light-theme ()
  "Enable the light GNOME/GTK theme."
  (interactive)
  (ceamx/gsettings-set-theme "light"))
(defun ceamx/load-dark-theme ()
  "Load a random dark theme."
  (interactive)
  (pcase ceamx-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-theme-buffet-dark-periods))
    (_
     (load-theme ceamx-theme-default-dark :no-confirm))))

(defun ceamx/load-light-theme ()
  "Load a random light theme."
  (interactive)
  (pcase ceamx-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-theme-buffet-light-periods))
    (_
     (load-theme ceamx-theme-default-light :no-confirm))))
(defun ceamx/light ()
  "Activate a light theme globally."
  (interactive)
  (ceamx/gsettings-light-theme)
  (ceamx/load-light-theme))

(defun ceamx/dark ()
  "Activate a dark theme globally."
  (interactive)
  (ceamx/gsettings-dark-theme)
  (ceamx/load-dark-theme))

(provide 'lib-ui)
;;; lib-ui.el ends here
