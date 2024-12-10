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

;; Ensure themes are applied in new frames to prevent flashing

;; - Source :: <https://protesilaos.com/emacs/dotemacs#h:7d3a283e-1595-4692-8124-e0d683cb15b2>


;; [[file:../config.org::*Ensure themes are applied in new frames to prevent flashing][Ensure themes are applied in new frames to prevent flashing:1]]
;; via prot-emacs
(defun ceamx-ui-re-enable-theme-in-frame (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`ceamx-ui-theme-no-bright-flash'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))
;; Ensure themes are applied in new frames to prevent flashing:1 ends here

;; Function to load a random theme from the specified periods :lib:


;; [[file:../config.org::*Function to load a random theme from the specified periods][Function to load a random theme from the specified periods:1]]
(declare-function theme-buffet--load-random "theme-buffet")

(defun +theme-buffet--load-random-from-periods (periods)
  "Load a random theme from the specified `theme-buffet' PERIODS.
PERIODS can be a single keyword or list of keywords. Each keyword
must be a valid `theme-buffet' period as defined in
`theme-buffet--keywords'."
  (let ((period (if (listp periods) (seq-random-elt periods) periods)))
    (theme-buffet--load-random period)))
;; Function to load a random theme from the specified periods:1 ends here

;; Desktop environment integration :lib:graphical:
;; :PROPERTIES:
;; :header-args: :tangle lisp/lib-ui.el
;; :END:


;; [[file:../config.org::*Desktop environment integration][Desktop environment integration:1]]
(require 'config-ui)
(require 'ceamx-lib)
;; Desktop environment integration:1 ends here

;; Commands


;; [[file:../config.org::*Commands][Commands:1]]
(defun ceamx-ui-gsettings-theme ()
  "Get the currently-active GNOME/GTK color scheme."
  (shell-command-to-string (format "gsettings get %s color-scheme"
                         ceamx-ui-gsettings-ui-namespace)))

(defun ceamx-ui-gsettings-dark-theme-p ()
  "Whether GNOME/GTK are using a theme with a dark color scheme."
  (string-match-p "dark" (ceamx-ui-gsettings-theme)))

(defun ceamx-ui/gsettings-set-theme (theme)
  "Set the GNOME/GTK theme to THEME."
  ;; FIXME: prompt with completion
  (interactive "s")
  (let* ((namespace ceamx-ui-gsettings-ui-namespace)
         (value (pcase theme
                  ((rx (optional "prefer-") "dark")
                   "prefer-dark")
                  ((rx (optional "prefer-") "light")
                   "prefer-light")
                  (_ "prefer-dark")))
         (cmd (format "gsettings set %s color-scheme %s" namespace value)))
    (shell-command cmd)))

(defun ceamx-ui/gsettings-dark-theme ()
  "Enable the dark GNOME/GTK theme."
  (interactive)
  (ceamx-ui/gsettings-set-theme "dark"))

(defun ceamx-ui/gsettings-light-theme ()
  "Enable the light GNOME/GTK theme."
  (interactive)
  (ceamx-ui/gsettings-set-theme "light"))
;; Commands:1 ends here

;; Integration with Kitty terminal theme :tty:


;; [[file:../config.org::*Integration with Kitty terminal theme][Integration with Kitty terminal theme:1]]
;; FIXME: error open /dev/tty: no such address or device
(defun ceamx-ui-kitty-set-theme (polarity)
  "Set the Kitty terminal emulator colors to POLARITY.
POLARITY is a string matching either \"light\" or \"dark\"."
  (shell-command
   (format "kitty @set-colors -a -c $KITTY_CONFIG_DIRECTORY/theme-%s.conf"
           polarity)))
;; Integration with Kitty terminal theme:1 ends here

;; Generalized commands for desktop environment integration

;; Taking all supported environments into account:

;; + [[*Integration with the GNOME/GTK/GSettings color scheme]]


;; [[file:../config.org::*Generalized commands for desktop environment integration][Generalized commands for desktop environment integration:1]]
(defun ceamx-ui-desktop-dark-theme-p ()
  "Predicate whether a desktop environment is displaying a dark appearance."
  (or (ceamx-ui-gsettings-dark-theme-p)))
;; Generalized commands for desktop environment integration:1 ends here

;; ~ceamx-ui-load-theme~: function to cleanly load a theme

;; Similar to the theme-family-specific ~modus-themes-load-theme~.


;; [[file:../config.org::*~ceamx-ui-load-theme~: function to cleanly load a theme][~ceamx-ui-load-theme~: function to cleanly load a theme:1]]
(defun ceamx-ui-load-theme (theme)
  "Load THEME after resetting any previously-loaded themes."
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (load-theme theme :no-confirm))
;; ~ceamx-ui-load-theme~: function to cleanly load a theme:1 ends here

;; Commands to load a preferred light or dark Emacs theme


;; [[file:../config.org::*Commands to load a preferred light or dark Emacs theme][Commands to load a preferred light or dark Emacs theme:1]]
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
;; Commands to load a preferred light or dark Emacs theme:1 ends here

;; Commands to globally set a preferred light or dark theme


;; [[file:../config.org::*Commands to globally set a preferred light or dark theme][Commands to globally set a preferred light or dark theme:1]]
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
;; Commands to globally set a preferred light or dark theme:1 ends here

(provide 'lib-ui)
;;; lib-ui.el ends here
