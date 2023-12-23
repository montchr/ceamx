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

(require 'config-ui)
(require 'lib-ui-theme)

;; Don't prompt to confirm theme safety.
;; This also has the benefit of avoiding problems with
;; first-time startup on Emacs > 26.3.
(setopt custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
;; `modus-vivendi' is bundled with Emacs, albeit in an older version.
(setq-default custom-enabled-themes '(modus-vivendi))

;; Ensure that themes will be applied even if they have not been customized
(add-hook 'after-init-hook #'cmx-reapply-themes)

;; Set up `after-enable-theme-hook'.
;; via <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
(defvar cmx-after-enable-theme-hook nil)
(defun cmx-after-enable-theme-hook (&rest _args)
  "Hook to run after enabling theme."
  (run-hooks 'cmx-after-enable-theme-hook))
(advice-add 'enable-theme :after #'cmx-after-enable-theme-hook)

;; TODO: why? also move to `init-ui-font'
(setopt font-lock-maximum-decoration t)

(when +sys-mac-p
  ;; `undecorated-round' is macOS-specific.
  (add-to-list 'default-frame-alist '(undecorated-round . t)))

;;; <https://git.sr.ht/~protesilaos/spacious-padding>
(use-package spacious-padding
  :commands (spacious-padding-mode)
  :init (spacious-padding-mode)
  :config
  (setopt spacious-padding-widths '( :internal-border-width 24
                                     :right-divider-width 24
                                     :scroll-bar-width 8)))

(use-feature! solar
  :config
  (setopt calendar-latitude 39.96)
  (setopt calendar-longitude -75.13))

(use-package circadian
  :after solar
  :config
  ;; FIXME: based on theme selected by `cmx-ui-theme-dark'
  (setopt circadian-themes '((:sunrise . modus-operandi)
                             (:sunset  . modus-vivendi)))
  (circadian-setup))

(provide 'init-ui-theme)
;;; init-ui-theme.el ends here
