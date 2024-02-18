;;; init-ui-font.el --- Font configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords:

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

;;; Code:

(require 'config-env)

(require 'lib-common)
(require 'lib-ui-font)

(use-package fontaine
  :demand t
  :config
  (setopt fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" ceamx-var-dir))
  ;; TODO: separate presets per font i.e. Berkeley Mono + Iosevka
  ;; FIXME: macOS forces sub-pixel rendering which can cause distortion at various sizes? or
  ;;        at least i think that's what's causing the inconsistencies...
  ;;
  ;;        ligatures at some sizes sometimes broken -- but not everywhere...?
  ;;        test case (double-colons, should not have a visual space between)
  ;;        => ::
  ;;
  ;;        - update: this might also be happening on NixOS running on
  ;;        old MacBookPro hardware (hodgepodge system), so could be a general
  ;;        ppi issue -- but i've had the preset set to `small' for a while and
  ;;        haven't noticed any issues since then

  ;; original values from earlier settings:
  ;;        (xsmall :default-height 100)
  ;; (small :default-height 110)
  ;; (regular :default-height 140)
  ;; (regular-alt :default-height 130)
  ;; (medium :default-height 160)
  ;; (medium-alt :default-height 150)
  ;; (large :default-height 180)
  ;; (large-alt :default-height 170
  ;;   :line-spacing 0.1)
  ;; (xlarge :default-height 240
  ;;   :line-spacing nil)

  (setopt fontaine-presets
    `((regular
        :default-height ,(ceamx-font-height 100))
       (medium
         :default-height ,(ceamx-font-height 120))
       (large
         :default-weight semilight
         :default-height ,(ceamx-font-height 140)
         :bold-weight extrabold)

       (t
         :default-family "Iosevka Comfy"
         :default-family "Monospace"
         :default-weight regular
         :default-height ,(ceamx-font-height 100)
         :fixed-pitch-family nil
         :fixed-pitch-weight nil
         :fixed-pitch-height 1.0
         :fixed-pitch-serif-family "Iosevka Comfy Motion"
         :fixed-pitch-serif-weight nil
         :fixed-pitch-serif-height 1.0
         :variable-pitch-family "Inter"
         :variable-pitch-weight nil
         :variable-pitch-height 1.0
         :bold-family nil
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)))

  ;; Persist latest preset across sessions.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(elpaca-wait)

;;;; `ligature.el' :: <https://github.com/mickeynp/ligature.el>

;; A better implementation of ligature support than `prettify-symbols-mode'.
;; <https://old.reddit.com/r/emacs/comments/keji66/what_is_bad_about_prettifysymbolsmode/>

(use-package ligature
  :demand t
  :config
  ;; Enable all Iosevka ligatures in programming modes
  ;; <https://github.com/mickeynp/ligature.el/wiki#iosevka>
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                        "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                        "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                        ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))

  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(provide 'init-ui-font)
;;; init-ui-font.el ends here
