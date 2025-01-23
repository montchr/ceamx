;;; ceamx-ui.el --- Ceamx: User interface & appearance library  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2025  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>

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

;;;; Requirements

(eval-when-compile
  (require 'cl-lib)
  (require 'ceamx-lib))

;;;; Variables

;; TODO: add standard-themes
(defvar ceamx-ui-dark-themes-list
  (remove 'nil (append (when (locate-library "modus-themes")
                         (seq-filter
                          (##string-prefix-p "modus-vivendi" (symbol-name %))
                          modus-themes-items))
                       (when (locate-library "ef-themes")
                         ef-themes-dark-themes))))

;; TODO: add standard-themes
(defvar ceamx-ui-light-themes-list
  (remove 'nil (append (when (locate-library "modus-themes")
                         (seq-filter
                          (##string-prefix-p "modus-operandi" (symbol-name %))
                          modus-themes-items))
                       (when (locate-library "ef-themes")
                         ef-themes-light-themes))))

  ;;;; Customization

(defgroup ceamx-ui ()
  "Ceamx user interface"
  :group 'faces)

(defcustom ceamx-ui-theme-family 'modus
  "Set of themes to load.
  Valid values are the symbols `ef', `modus', and `standard', which
  reference the `ef-themes', `modus-themes', and `standard-themes',
  respectively.

  A nil value does not load any of the above (use Emacs without a
  theme)."
  :group 'ceamx-ui
  :type '(choice :tag "Set of themes to load" :value modus
                 (const :tag "The `ef-themes' module" ef)
                 (const :tag "The `modus-themes' module" modus)
                 (const :tag "The `standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom ceamx-ui-preferred-dark-theme nil
  "Alist of preferred dark themes per supported `ceamx-ui-theme-family'.
  The car of the alist cells should either be nil or match one of the
  `ceamx-ui-theme-family' options.

  The cdr must be a symbol for a theme provided by one of the supported
  theme options.  The theme should be a theme with a dark background.

  When car is nil, the theme specified in the cdr will be used as fallback
  when `ceamx-ui-theme-family' is nil."
  :type '(alist :key-type symbol
                :value-type symbol)
  :group 'ceamx-ui)

(defcustom ceamx-ui-preferred-light-theme nil
  "Alist of preferred light themes per supported `ceamx-ui-theme-family'.
  The car of the alist cells should either be nil or match one of the
  `ceamx-ui-theme-family' options.

  The cdr must be a symbol for a theme provided by one of the supported
  theme options.  The theme should be a theme with a light background.

  When car is nil, the theme specified in the cdr will be used as fallback
  when `ceamx-ui-theme-family' is nil."
  :type '(alist :key-type symbol
                :value-type symbol)
  :group 'ceamx-ui)

(defcustom ceamx-ui-theme-dark
  (or (alist-get ceamx-ui-theme-family ceamx-ui-preferred-dark-theme)
      'modus-vivendi)
  "The default dark theme."
  :group 'ceamx-ui
  :type 'symbol)

(defcustom ceamx-ui-theme-light
  (or (alist-get ceamx-ui-theme-family ceamx-ui-preferred-light-theme)
      'modus-operandi)
  "The default light theme."
  :group 'ceamx-ui
  :type 'symbol)

(defcustom ceamx-ui-theme-circadian-interval nil
  "The circadian theme switching interval.
  Value may be `period', `solar', or nil, corresponding
  respectively to period-based switching with `theme-buffet' or
  sunrise/sunset toggling from the combination of the `solar'
  library and the `circadian' package.

  A nil value means to disable automatic theme switching.
  Theme-switching commands `ceamx/light' and `ceamx/dark' will
  unconditionally use `ceamx-ui-theme-default-light' and
  `ceamx-ui-theme-default-dark', respectively."
  :group 'ceamx-ui
  :type '(choice :tag "Circadian theme switching interval" :value nil
                 (const :tag "Time periods via `theme-buffet'" :value buffet)
                 (const :tag "Sunrise or sunset via `solar' and `circadian'" :value solar)))

  ;;;; Functions

(defun ceamx-ui-define-preferred-themes (theme-family dark-theme light-theme)
  "Define the preferred DARK-THEME and LIGHT-THEME for THEME-FAMILY."
  (cl-pushnew `(,theme-family . ,dark-theme) ceamx-ui-preferred-dark-theme)
  (cl-pushnew `(,theme-family . ,light-theme) ceamx-ui-preferred-light-theme))

(defun ceamx-ui-theme-family-preferred-themes (theme-family)
  "Return a list of preferred dark and light themes for THEME-FAMILY."
  (list (or (alist-get theme-family ceamx-ui-preferred-dark-theme)
            (alist-get nil ceamx-ui-preferred-dark-theme))
        (or (alist-get theme-family ceamx-ui-preferred-light-theme)
            (alist-get nil ceamx-ui-preferred-dark-theme))))

(defun ceamx-ui-load-theme (theme)
  "Load THEME after resetting any previously-loaded themes.
  See also `modus-themes-load-theme'."
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (load-theme theme :no-confirm))

  ;;;; Commands

  ;;;###autoload
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

  ;;;###autoload
(defun ceamx-ui/gsettings-dark-theme ()
  "Enable the dark GNOME/GTK theme."
  (interactive)
  (ceamx-ui/gsettings-set-theme "dark"))

  ;;;###autoload
(defun ceamx-ui/gsettings-light-theme ()
  "Enable the light GNOME/GTK theme."
  (interactive)
  (ceamx-ui/gsettings-set-theme "light"))

  ;;;###autoload
(defun ceamx-ui/load-dark-theme ()
  "Load the preferred dark theme."
  (interactive)
  (load-theme ceamx-ui-theme-dark :no-confirm))

  ;;;###autoload
(defun ceamx-ui/load-light-theme ()
  "Load the preferred light theme."
  (interactive)
  (load-theme ceamx-ui-theme-light :no-confirm))

  ;;;###autoload
(defun ceamx-ui/load-random-dark-theme ()
  "Load a random dark theme."
  (interactive)
  (pcase ceamx-ui-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-ui-theme-buffet-dark-periods))
    (_
     (let ((theme (seq-random-elt ceamx-ui-dark-themes-list)))
       (load-theme theme :no-confirm)))))

  ;;;###autoload
(defun ceamx-ui/load-random-light-theme ()
  "Load a random light theme."
  (interactive)
  (pcase ceamx-ui-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-ui-theme-buffet-light-periods))
    (_
     (let ((theme (seq-random-elt ceamx-ui-light-themes-list)))
       (load-theme theme :no-confirm)))))

  ;;;###autoload
(defun ceamx-ui/light ()
  "Activate a light theme globally."
  (interactive)
  (ceamx-ui/gsettings-light-theme)
  (ceamx-ui/load-light-theme))

  ;;;###autoload
(defun ceamx-ui/dark ()
  "Activate a dark theme globally."
  (interactive)
  (ceamx-ui/gsettings-dark-theme)
  (ceamx-ui/load-dark-theme))

  ;;;; Footer

(provide 'ceamx-ui)
  ;;; ceamx-ui.el ends here
