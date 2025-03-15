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

(defcustom ceamx-ui-preferred-dark-themes nil
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

(defcustom ceamx-ui-preferred-light-themes nil
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

(defcustom ceamx-ui-theme-dark nil
  "The default dark theme."
  :group 'ceamx-ui
  :type 'symbol)

(defcustom ceamx-ui-theme-light nil
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

(defun ceamx-ui--filter-symbols-by-prefix (prefix symbols)
  "Filter a list of SYMBOLS whose name begins with PREFIX."
  (seq-filter (##string-prefix-p prefix (symbol-name %)) symbols))

(defun ceamx-ui-dark-themes-list ()
  "Return a list of known dark themes."
  (append
    (when (locate-library "modus-themes")
      (ceamx-ui--filter-symbols-by-prefix
        "modus-vivendi" modus-themes-items))
    (when (locate-library "ef-themes")
      ef-themes-dark-themes)
    (when (locate-library "standard-themes")
      (ceamx-ui--filter-symbols-by-prefix
        "standard-light" standard-themes-items))))

(defun ceamx-ui-light-themes-list ()
  "Return a list of known light themes."
  (append
    (when (locate-library "modus-themes")
      (ceamx-ui--filter-symbols-by-prefix
        "modus-operandi" modus-themes-items))
    (when (locate-library "ef-themes")
      ef-themes-light-themes)
    (when (locate-library "standard-themes")
      (ceamx-ui--filter-symbols-by-prefix
        "standard-dark" standard-themes-items))))

(defun ceamx-ui-theme-dark ()
  "Dark theme to toggle.
The theme specified in the customizable variable `ceamx-ui-theme-dark'
will take priority over themes inferred based on `ceamx-ui-theme-family'
and `ceamx-ui-preferred-dark-themes'."
  (or ceamx-ui-theme-dark
    (alist-get ceamx-ui-theme-family ceamx-ui-preferred-dark-themes)
    'modus-vivendi))

(defun ceamx-ui-theme-light ()
  "Light theme to toggle.
The theme specified in the customizable variable `ceamx-ui-theme-light'
will take priority over themes inferred based on `ceamx-ui-theme-family'
and `ceamx-ui-preferred-light-themes'."
  (or ceamx-ui-theme-light
      (alist-get ceamx-ui-theme-family ceamx-ui-preferred-light-themes)
      'modus-operandi))

(defun ceamx-ui-define-preferred-themes (theme-family dark-theme light-theme)
  "Define the preferred DARK-THEME and LIGHT-THEME for THEME-FAMILY."
  (cl-pushnew `(,theme-family . ,dark-theme) ceamx-ui-preferred-dark-themes)
  (cl-pushnew `(,theme-family . ,light-theme) ceamx-ui-preferred-light-themes))

(defun ceamx-ui-theme-family-preferred-themes (theme-family)
  "Return a list of preferred dark and light themes for THEME-FAMILY."
  (list (or (alist-get theme-family ceamx-ui-preferred-dark-themes)
            (alist-get nil ceamx-ui-preferred-dark-themes))
        (or (alist-get theme-family ceamx-ui-preferred-light-themes)
            (alist-get nil ceamx-ui-preferred-dark-themes))))

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
(defun ceamx-ui/load-theme (theme)
  "Load THEME exclusively, disabling all other themes."
  (interactive)
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes))
  (load-theme theme :no-confirm))

;;;###autoload
(defun ceamx-ui/load-dark-theme ()
  "Load the preferred dark theme."
  (interactive)
  (ceamx-ui/load-theme (ceamx-ui-theme-dark)))

  ;;;###autoload
(defun ceamx-ui/load-light-theme ()
  "Load the preferred light theme."
  (interactive)
  (ceamx-ui/load-theme (ceamx-ui-theme-light)))

(defun ceamx-ui/load-random-theme (polarity)
  (interactive)
  (let* ((themes (pcase polarity
                   ('light (ceamx-ui-light-themes-list))
                   ('dark (ceamx-ui-dark-themes-list))
                   (_ (append
                        (ceamx-ui-light-themes-list)
                        (ceamx-ui-dark-themes-list)))))
          (theme (seq-random-elt themes)))
    (ceamx-ui/load-theme theme)
    (message "Ceamx loaded random %S theme ‘%S’" polarity theme)))

;;;###autoload
(defun ceamx-ui/load-random-dark-theme ()
  "Load a random dark theme."
  (interactive)
  (ceamx-ui/load-random-theme 'dark))

  ;;;###autoload
(defun ceamx-ui/load-random-light-theme ()
  "Load a random light theme."
  (interactive)
  (ceamx-ui/load-random-theme 'light))

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
