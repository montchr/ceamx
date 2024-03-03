;;; lib-ui-theme.el --- Theme library functions and macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery
;; Copyright (C) 2024  Protesilaos Stavrou

;; Author: Chris Montgomery <chris@cdom.io>
;;         Protesilaos Stavrou <info@protesilaos.com>
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

;; Theme library functions and macros.

;;; Code:

(require 'config-ui)

(require 'lib-common)

(declare-function theme-buffet--load-random "theme-buffet")

;;; Helpers

(defun +theme-buffet--load-random-from-periods (periods)
  "Load a random theme from the specified `theme-buffet' PERIODS.
PERIODS can be a single keyword or list of keywords. Each keyword
must be a valid `theme-buffet' period as defined in
`theme-buffet--keywords'."
  (let ((period (if (listp periods) (seq-random-elt periods) periods)))
    (theme-buffet--load-random period)))

;;; Functions

;; via prot-emacs
(defun ceamx-theme-re-enable-in-frame (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`ceamx-theme-no-bright-flash'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

(defun ceamx-gnome-theme ()
  "Get the currently-active GNOME/GTK color scheme."
  (shell-command (format "gsettings get %s color-scheme"
                         ceamx-gnome-ui-namespace)))

(defun ceamx-gnome-theme-dark-p ()
  "Whether GNOME/GTK are using a theme with a dark color scheme."
  (string-match-p "dark" (ceamx-gnome-theme)))

;;; Commands

;;;; Desktop environment commands

(defun ceamx/gnome-set-theme (theme)
  "Set the GNOME/GTK theme to THEME."
  ;; FIXME: prompt with completion
  (interactive "s")
  (let* ((namespace ceamx-gnome-ui-namespace)
         (value (pcase theme
                  ((rx (optional "prefer-") "dark")
                    "prefer-dark")
                  ((rx (optional "prefer-") "light")
                    "prefer-light")
                  (_ "prefer-dark")))
         (cmd (format "gsettings set %s color-scheme %s" namespace value)))
    (shell-command cmd)))

(defun ceamx/gnome-dark-theme ()
  "Enable the dark GNOME/GTK theme."
  (interactive)
  (ceamx/gnome-set-theme "dark"))

(defun ceamx/gnome-light-theme ()
  "Enable the light GNOME/GTK theme."
  (interactive)
  (ceamx/gnome-set-theme "light"))

;;;; Emacs-specific commands

(defun ceamx/load-dark-theme ()
  "Load a random dark theme."
  (interactive)
  (+theme-buffet--load-random-from-periods
    ceamx-theme-buffet-dark-periods))

(defun ceamx/load-light-theme ()
  "Load a random light theme."
  (interactive)
  (+theme-buffet--load-random-from-periods
    ceamx-theme-buffet-light-periods))

;;;; Global commands

(defun ceamx/light ()
  "Activate a light theme globally."
  (interactive)
  (ceamx/gnome-light-theme)
  (ceamx/load-light-theme))

(defun ceamx/dark ()
  "Activate a dark theme globally."
  (interactive)
  (ceamx/gnome-dark-theme)
  (ceamx/load-dark-theme))

(provide 'lib-ui-theme)
;;; lib-ui-theme.el ends here
