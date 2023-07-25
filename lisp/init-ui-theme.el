;;; init-ui-theme.el --- Theme Initalization -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;;  Load the theme configurations.

;;; Code:

;; Don't prompt to confirm theme safety.
;; This also has the benefit of avoiding problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(modus-vivendi))

;; Ensure that themes will be applied even if they have not been customized
(defun cmx-reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
(add-hook 'elpaca-after-init-hook 'cmx-reapply-themes)

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-operandi-tinted))
  (cmx-reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-vivendi))
  (cmx-reapply-themes))

(use-package modus-themes
  :demand t
  :config
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-common-palette-overrides
        '((fringe unspecified)))

  ;; via <https://protesilaos.com/emacs/modus-themes#h:d0a3157b-9c04-46e8-8742-5fb2a7ae8798>
  (defun cmx/multine-comments-h ()
    (setq-local c-doc-face-name 'font-lock-comment-face))
  (add-hook 'php-mode-hook #'cmx/multine-comments-h)

  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-feature solar
  :config
  (setq calendar-latitude 39.96)
  (setq calendar-longitude -75.13))

(use-package circadian
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-vivendi-tinted)))
  (circadian-setup))

;; FIXME: does not belong in this file, is more of a generalised interface library
(use-package magit-section :defer t)

(provide 'init-ui-theme)
;;; init-ui-theme.el ends here

