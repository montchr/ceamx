;;; init-after-ui.el --- Late UI configuration       -*- lexical-binding: t; -*-

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

;; Configuration to load after all other user interface customizations.

;; For features depending on the state of all other `init-ui-*' modules.

;;; Code:

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
           :header-line-width 4
           ;; :header-line-width 0
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

(provide 'init-after-ui)
;;; init-after-ui.el ends here