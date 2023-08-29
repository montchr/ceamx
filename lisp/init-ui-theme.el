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

(require 'config-ui-theme)
(require 'lib-ui-theme)

;; Don't prompt to confirm theme safety.
;; This also has the benefit of avoiding problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; Ensure that themes will be applied even if they have not been customized
(add-hook 'elpaca-after-init-hook 'cmx-reapply-themes)

;; Set up `after-enable-theme-hook'.
(advice-add 'enable-theme :after #'cmx--after-enable-theme-hook)

;; Render multiline comments using `font-lock-comment-face'.
(add-hook 'php-mode-hook #'cmx--multine-comment-face-hook)

(setq font-lock-maximum-decoration t)
(setq default-frame-alist
      (append (list
               '(min-height . 1)  '(height . 45)
               '(min-width  . 1)  '(width  . 81)
               '(vertical-scroll-bars . nil)
               ;; '(internal-border-width . 24
               '(left-fringe . 0)
               '(right-fringe . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(when +sys-mac-p
  ;; emacs-plus@29+ only
  (add-to-list 'default-frame-alist '(undecorated-round . t)))

;;; <https://git.sr.ht/~protesilaos/spacious-padding>
(use-package spacious-padding
  :commands (spacious-padding-mode)
  :init (spacious-padding-mode)
  :config
  (setq spacious-padding-widths '( :internal-border-width 32
                                   :right-divider-width 24
                                   :scroll-bar-width 8)))

(use-feature solar
  :config
  (setq calendar-latitude 39.96)
  (setq calendar-longitude -75.13))

(use-package circadian
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

;; FIXME: does not belong in this file, is more of a generalised interface library
(use-package magit-section :defer t)

(provide 'init-ui-theme)
;;; init-ui-theme.el ends here
