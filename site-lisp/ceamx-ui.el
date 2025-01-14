;;; ceamx-ui.el --- Ceamx: User interface & appearance library  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

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
  (require 'ceamx-lib))

;;;; Variables

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

;;;; Customization

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

(defcustom ceamx-ui-theme-light 'modus-operandi
  "The default light theme."
  :group 'ceamx
  :type 'symbol)

(defcustom ceamx-ui-theme-dark 'modus-vivendi
  "The default dark theme."
  :group 'ceamx
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
  :group 'ceamx
  :type '(choice :tag "Circadian theme switching interval" :value nil
          (const :tag "Time periods via `theme-buffet'" :value buffet)
          (const :tag "Sunrise or sunset via `solar' and `circadian'" :value solar)))

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

;;;; Functions

;;;;; Public

(defun ceamx-ui-load-theme (theme)
  "Load THEME after resetting any previously-loaded themes.
See also `modus-themes-load-theme'."
  (mapc #'disable-theme (remq theme custom-enabled-themes))
  (load-theme theme :no-confirm))

;;;;; Private

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
  "Load a random dark theme."
  (interactive)
  (pcase ceamx-ui-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-ui-theme-buffet-dark-periods))
    (_
     (load-theme ceamx-ui-theme-dark :no-confirm))))

;;;###autoload
(defun ceamx-ui/load-light-theme ()
  "Load a random light theme."
  (interactive)
  (pcase ceamx-ui-theme-circadian-interval
    ('buffet
     (+theme-buffet--load-random-from-periods
      ceamx-ui-theme-buffet-light-periods))
    (_
     (load-theme ceamx-ui-theme-light :no-confirm))))

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
