;;; init-ui-nano-theme.el --- N A N O Theme           -*- lexical-binding: t; -*-

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

;; <https://github.com/rougier/nano-theme>

;;; Code:


(use-package nano-theme
  ;; FIXME: :elpaca (nano :fetcher github :repo "rougier/nano-theme" :branch "master")

  :init
  (load-theme 'nano t)

  :config
  ;;; Configuration sourced from `nano-mode'.
  (setopt font-lock-maximum-decoration t)
  (setopt default-frame-alist
        (append (list
                 '(min-height . 1)  '(height . 45)
                 '(min-width  . 1)  '(width  . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 24)
                 '(left-fringe . 0)
                 '(right-fringe . 0)
                 '(undecorated-round . t) ;; emacs-plus@29 only
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))

  ;; Line spacing (in pixels)
  ;; (setopt line-spacing 0)

  ;; Vertical window divider
  (setq window-divider-default-right-width 24)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)

  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?… 'nano-faded))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?- 'nano-faded))

  (custom-theme-set-faces 'nano
                          ;;'(nano-mono ((t (:weight light :height 140 :family "Iosevka Xtal"))))
                          '(nano-mono ((t (:weight light :height 140 :family "Iosevka Comfy"))))
                          '(nano-sans ((t (:weight light :height 140 :family "Inter")))))


  (let ((font (font-spec :name "Symbols Nerd Font Mono")))
    (if (find-font font)
        (set-fontset-font t '(#xe000 . #xffdd) font)
      (message "Symbols Nerd Font Mono has not been found on your system"))))

;; FIXME: (elpaca-wait)

(provide 'init-ui-nano-theme)
;;; init-ui-nano-theme.el ends here
