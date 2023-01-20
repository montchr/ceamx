;;; init-theme.el --- Theme Initalization -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/dotfield
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;  Load the theme and font configurations.

;;; Code:

;;; --- modus-themes ---

(elpaca-use-package modus-themes
  :ensure t
  :demand t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  (load-theme 'modus-vivendi-tinted :no-confirm))


;;; -- fontaine ---

(elpaca-use-package fontaine
  :ensure t
  :demand t
  :config
  (setq fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" +path-var-dir))
  (setq fontaine-presets
         '((small :default-height 106
            :default-family "Iosevka Term")
           (regular :default-height 124)
           (medium :default-height 135)
           (large :default-height 160)
           (xlarge :default-height 170
                   :bold-weight bold)
           (t
            :default-family "Iosevka"
            :default-weight regular
            :default-height 124
            :fixed-pitch-family nil
            :fixed-pitch-family nil
            :fixed-pitch-height 1.0
            :fixed-pitch-serif-family nil
            :fixed-pitch-serif-weight nil
            :variable-pitch-family "IBM Plex Sans"
            :variable-pitch-weight nil
            :variable-pitch-height 1.0
            :bold-family nil
            :bold-weight semibold
            :italic-family nil
            :italic-slant italic
            :line-spacing nil)))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(provide 'init-theme)
;;; init-theme.el ends here

