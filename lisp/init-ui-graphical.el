;;; init-ui-graphical.el --- Appearance customizations for graphical environments  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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
;;; Code:

;; Typography :font:
;; :PROPERTIES:
;; :header-args: :tangle lisp/init-ui-graphical.el
;; :END:


(require 'ceamx-paths)
(require 'config-env)

(require 'ceamx-lib)
(require 'lib-ui)

;; Text rendering and scaling


(setq x-underline-at-descent-line nil)

(setq-default text-scale-remap-header-line t)

;; ~fontaine~: pre-configure font presets :package:

;; <https://protesilaos.com/emacs/fontaine>

;; TIP: You can test out alterations quickly with, for example:
;;      (internal-set-lisp-face-attribute 'default :weight 'semilight)


(package! fontaine
  (require 'fontaine)

  (setopt fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" ceamx-var-dir))

  ;; For some reason I do not yet understand, according to some hearsay, font
  ;; sizes best scale in multiples of 3-point increments. So, each height value
  ;; is a multiple of 3.
  (setopt fontaine-presets
          `( (tiny
              :bold-weight medium
              :default-height ,(pcase (system-name)
                                (_ 78))
              :default-weight ,(pcase (system-name)
                                (_ 'semilight)))
             (small
              :bold-weight medium
              :default-height ,(pcase (system-name)
                                (_ 90))
              :default-weight ,(pcase (system-name)
                                (_ 'regular)))
             (regular)
             (medium
              :default-height ,(pcase (system-name)
                                ("boschic" 124)
                                ("tuuvok"
                                 120
                                 ;; 115

                                 )
                                (_ 120)))
             (large
              :default-height ,(pcase (system-name)
                                ;; ("tuuvok" 140)
                                (_ 144))
              :default-weight semilight
              :bold-weight semibold)
             (xlarge
              :default-height ,(pcase (system-name)
                                (_ 156))
              :bold-weight bold)
             (big-mclarge-huge
              :default-weight semilight
              :default-height ,(pcase (system-name)
                                (_ 180))
              :bold-weight extrabold)
             (t
              :default-family "Iosevka Comfy"
              :default-weight regular
              :default-slant normal
              :default-height ,(pcase (system-name)
                                ("tuuvok" 102)
                                (_ 105))

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
              :bold-weight medium
              ;; :bold-weight semibold
              :bold-slant nil
              :bold-height 1.0

              :italic-family nil
              :italic-weight nil
              :italic-slant italic
              :italic-height 1.0

              :line-spacing 1)))

  ;; Persist latest preset across sessions.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

;; Elpaca-Wait № 5: after baseline font settings :wait:


(elpaca-wait)

;; ~ligature.el~: improved ligature support :package:

;; <https://github.com/mickeynp/ligature.el>

;; A better implementation of ligature support than the builtin ~prettify-symbols-mode~.
;; <https://old.reddit.com/r/emacs/comments/keji66/what_is_bad_about_prettifysymbolsmode/>


(package! ligature
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

(provide 'init-ui-graphical)
;;; init-ui-graphical.el ends here
