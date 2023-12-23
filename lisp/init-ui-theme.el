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

;; Ensure that themes will be applied in new frames to counteract default
;; effects from early-init frame flash workaround.
;; TODO: also some other link i can't find now
;; <https://protesilaos.com/emacs/dotemacs#h:7d3a283e-1595-4692-8124-e0d683cb15b2>
(add-hook 'after-make-frame-functions #'cmx-theme-re-enable-in-frame)

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

;;; `spacious-padding' :: <https://protesilaos.com/emacs/spacious-padding>
;;  TODO: track upstream git repo by tag since this is in rapid development
(use-package spacious-padding
   :defer 1
  :commands (spacious-padding-mode)
  :defines (spacious-padding-widths)

:init
  ;; These are the defaults, but I keep it here for visiibility.
  (setopt spacious-padding-widths
        '(
           :internal-border-width 15
           ;; FIXME: `:internal-border-width' in combination with non-zero
           ;; `:header-line-width' breaks tab height.
           ;;
           ;; Since I don't use the header line (does anyone?), hiding it this
           ;; way is fine with me. However, this seems like a `spacious-padding' bug.
           :header-line-width 0         ; default: 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  :config
  ;; (setopt tab-bar-border nil)

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible.
  ;; TODO: v0.3.0 standardizes this a bit
  ;; (setq spacious-padding-subtle-mode-line
  ;;       `(:mode-line-active default     ; NOTE: assumes `modus-themes'
  ;;                           :mode-line-inactive vertical-border))

  (spacious-padding-mode 1))

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
