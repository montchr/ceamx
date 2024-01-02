;;; config-ui.el --- General UI settings             -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; FIXME: defcustom with preset options

;;; Code:

(defcustom cmx-modeline-provider 'doom
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

(defcustom cmx-theme-family 'ef
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

;; TODO: implement
;;       see `modus-themes-items' for available options source for that theme family

;; (defcustom cmx-theme-dark nil
;;   "Preferred dark theme.
;; A nil value means functionality relying on this will be unavailable."
;;   :group 'ceamx
;;   :type '(symbol))

;; (defcustom cmx-theme-light nil
;;   "Preferred light theme.
;; A nil value means functionality relying on this will be unavailable."
;;   :group 'ceamx
;;   :type '(symbol))

;; FIXME: doesn't seem to have desired effect?
(defvar cmx-ui-theme-family-package
  (alist-get cmx-theme-family '((ef . ef-themes)
                                 (modus . modus-themes)
                                 (nano . nano-theme))))

;; TODO: use these
(defvar cmx-ui-light-theme-alist '((modus . modus-operandi)
                                    (ef . ef-elea-light)))
(defvar cmx-ui-dark-theme-alist '((modus . modus-vivendi)
                                  (ef . ef-elea-dark)))
;; (defun cmx-ui-theme-preferred ())
;; FIXME: defcustom
(defvar cmx-ui-theme-dark 'modus-vivendi "Preferred dark theme.")
(defvar cmx-ui-theme-light 'modus-operandi "Preferred light theme.")


(provide 'config-ui)
;;; config-ui.el ends here
