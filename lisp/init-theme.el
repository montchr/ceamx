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

;; Toggle between light and dark theme.

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

;;; --- modus-themes ---

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

;;; -- fontaine ---

(use-package fontaine
  :demand t
  :config
  (setq fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" +path-var-dir))
  ;; TODO: separate presets per font i.e. Berkeley Mono + Iosevka
  ;; FIXME: macOS forces sub-pixel rendering which can cause distortion at various sizes? or
  ;; at least i think that's what's causing the inconsistencies...
  (setq fontaine-presets
        '((small :default-height 110)
          (regular :default-height 130)
          (medium :default-height 150)
          (large :default-height 170
                 :line-spacing 0.1)
          (xlarge :default-height 240
                  :line-spacing nil)
          (t
           ;; TODO: set values from nix config (or, less ideally, by env vars)
           :default-family "JetBrains Mono"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil
           :fixed-pitch-family nil
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :variable-pitch-family "Inter"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing 0.1)))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

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



;;; -- ligature.el ---

(use-package ligature
  :elpaca (ligature :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable all JetBrains Mono ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                       "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                       "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                       "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                       "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                       "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                       ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                       "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                       "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                       "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                       "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))

  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; FIXME: configure symbols -- e.g. `lambda' prettification is problematic
;; (use-feature prettify-symbols
;;   :hook prog-mode)

(use-feature emacs
  :config
  ;; Add frame borders and window dividers
  ;; via <https://github.com/minad/org-modern#configuration>
  (modify-all-frames-parameters
   '((right-divider-width . 10)
     (internal-border-width . 10)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background)))


;; FIXME: move to a dedicated 'ui' module
(use-package magit-section :defer t)

(provide 'init-theme)
;;; init-theme.el ends here

