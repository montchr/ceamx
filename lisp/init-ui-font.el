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


;;; Code:

;;;; Requirements

(require 'elpaca-autoloads)

(require 'ceamx-paths)
(require 'config-env)

(require 'lib-common)
(require 'lib-ui-font)

;;;; General

(setq x-underline-at-descent-line nil)

(setq-default text-scale-remap-header-line t)

;;;; Use the `fontaine' package for configuring and managing font presets

;; <https://protesilaos.com/emacs/fontaine>

(elpaca fontaine
  (require 'fontaine)

  (setopt fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" ceamx-var-dir))

  ;; For some reason I do not yet understand, according to some hearsay, font
  ;; sizes best scale in multiples of 3-point increments. So, each height value
  ;; is a multiple of 3.
  (setopt fontaine-presets
    `( (small
         :default-height ,(ceamx-font-height 90))
       (regular)
       (medium
         :default-height ,(ceamx-font-height 120))
       (large
         :default-height ,(ceamx-font-height 144))
       (xlarge
         :default-height ,(ceamx-font-height 156))
       (big-mclarge-huge
         :default-weight semilight
         :default-height ,(ceamx-font-height 180)
         :bold-weight extrabold)
       (t
         :default-family "Iosevka Comfy"
         :default-weight nil
         :default-slant normal
         :default-height ,(ceamx-font-height 105)

         :fixed-pitch-family "Iosevka Comfy"
         :fixed-pitch-weight nil
         :fixed-pitch-slant nil
         :fixed-pitch-height 1.0

         :fixed-pitch-serif-family nil
         :fixed-pitch-serif-weight nil
         :fixed-pitch-serif-slant nil
         :fixed-pitch-serif-height 1.0

         :variable-pitch-family "Iosevka Comfy Motion"
         :variable-pitch-weight nil
         :variable-pitch-slant nil
         :variable-pitch-height 1.0

         :header-line-family nil
         :header-line-height 1.0
         :header-line-slant nil
         :header-line-weight nil

         :line-number-family nil
         :line-number-height 1.0
         :line-number-slant nil
         :line-number-weight nil

         :mode-line-active-family nil
         :mode-line-active-weight nil
         :mode-line-active-slant nil
         :mode-line-active-height 1.0

         :mode-line-inactive-family nil
         :mode-line-inactive-weight nil
         :mode-line-inactive-slant nil
         :mode-line-inactive-height 1.0

         :tab-bar-family nil
         :tab-bar-weight nil
         :tab-bar-slant nil
         :tab-bar-height 1.0

         :tab-line-family nil
         :tab-line-weight nil
         :tab-line-slant nil
         :tab-line-height 1.0

         :bold-family nil
         :bold-weight semibold
         :bold-slant nil
         :bold-height 1.0

         :italic-family nil
         :italic-weight nil
         :italic-slant italic
         :italic-height 1.0

         :line-spacing nil)))

  ;; Persist latest preset across sessions.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(elpaca-wait)

;;;; Enable improved ligature support with the `ligature.el' package

;; <https://github.com/mickeynp/ligature.el>

;; A better implementation of ligature support than the builtin `prettify-symbols-mode'.
;; <https://old.reddit.com/r/emacs/comments/keji66/what_is_bad_about_prettifysymbolsmode/>

(elpaca ligature
  (require 'ligature)

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
