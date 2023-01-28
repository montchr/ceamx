;;; init-theme.el --- Theme Initalization -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
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

(elpaca-use-package circadian
  :after solar
  :config
  (setq circadian-themes '((:sunrise . modus-operandi-tinted)
                           (:sunset  . modus-vivendi-tinted)))
  (circadian-setup))

(elpaca-use-package dimmer
  :after (modus-themes)

  ;; :custom
  ;; FIXME: include corfu etc
  ;; (dimmer-exclusion-regexp-list '("^\\*[h|H]elm.*\\*"
  ;;                                 "^\\*Minibuf-.*\\*"
  ;;                                 "^\\*Echo.*"
  ;;                                 "^.\\*which-key\\*$")
  ;;                               )

  :config
  ;;; See notes re: Modus Themes compatibility:
  ;;; <https://protesilaos.com/emacs/modus-themes#h:8eb4b758-d318-4480-9ead-357a571beb93>
  (setq dimmer-fraction 0.3)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)

  (setq dimmer-watch-frame-focus-events nil)

  (dimmer-mode 1)

  :hook '((which-key-mode . dimmer-configure-which-key)
          (magit-mode . dimmer-configure-magit)))



;;; -- fontaine ---

(elpaca-use-package fontaine :demand t
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

(elpaca-use-package (ligature :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable all Iosevka ligatures in programming modes
  ;; <https://github.com/mickeynp/ligature.el/wiki#iosevka>
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enable all JetBrains Mono ligatures in programming modes
  ;; (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
  ;;                                     "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
  ;;                                     "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
  ;;                                     "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
  ;;                                     "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
  ;;                                     "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
  ;;                                     ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
  ;;                                     "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
  ;;                                     "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
  ;;                                     "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
  ;;                                     "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; FIXME: configure symbols -- e.g. `lambda' prettification is problematic
;; (use-feature prettify-symbols
;;   :hook prog-mode)

(provide 'init-theme)
;;; init-theme.el ends here

