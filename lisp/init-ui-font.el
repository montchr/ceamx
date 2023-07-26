;;; init-ui-font.el --- Font configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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


(provide 'init-ui-font)
;;; init-ui-font.el ends here