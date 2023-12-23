;;; init-window.el --- Window management -*- lexical-binding: t -*-

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

;;; Code:

(require 'lib-common)
(require 'lib-window)

;; TODO: add notes
(defadvice keyboard-escape-quit
    (around keyboard-escape-quit-dont-close-windows activate)
  (let ((buffer-quit-function (lambda () ())))
    ad-do-it))

;; Disable buffer line wrapping by default.
(set-default 'truncate-lines t)

;; Hide buffer until there's output.
;; Prevents an extra window appearing during init.
(setopt async-shell-command-display-buffer nil)

(setopt display-buffer-base-action
        '((display-buffer-reuse-mode-window display-buffer-pop-up-window)
          (reusable-frames . t)))
(setopt even-window-sizes nil)

(use-feature! winner
  :config (winner-mode))

;;; `olivetti-mode'
(use-package olivetti :defer t
  :hook (org-mode . olivetti-mode))

;;; popper -- <https://github.com/karthink/popper>
;;  "minor-mode to summon and dismiss buffers easily."
(use-package popper
  :blackout
  :commands (popper-mode
             popper-echo-mode
             popper-group-by-projectile)
  :init
  (setopt popper-reference-buffers
        '("\\*Messages\\*"
           "Output\\*$"
           "\\*vc\\*"
           "\\*Warnings\\*"
           "\\*elpaca-log\\*"
           "\\*Embark Export"
           "\\*lispy-message\\*"        ; `lispy-eval' output
           compilation-mode
           help-mode
           helpful-mode
           +popper-current-buffer-popup-p
           ))

  (popper-mode +1)
  (popper-echo-mode +1)                 ; For echo area hints

  :config
  (define-keymap :keymap (current-global-map)
    "C-`"   #'popper-toggle
    ;; NOTE: Conflicts with default GNOME keybinding.
    "M-`"   #'popper-echo-mode
    "C-M-`" #'popper-toggle-type)

  (def-hook! cmx-escape-popper-popup-h (&rest _) cmx-escape-hook
    "Close any focused `popper' popup when calling `cmx/escape'."
    (when (bound-and-true-p popper-popup-status)
      (popper-toggle)))

  ;; NOTE: A non-nil value for this setting will conflict with `shackle.el' (if
  ;; installed) and, potentially, `display-buffer-alist' customizations.
  ;; <https://github.com/karthink/popper?tab=readme-ov-file#popup-placement-controlled-using-display-buffer-alist-or-shackleel>
  (after! 'shackle
    (setopt popper-display-control nil))

  ;; Disable mode-line in popups.
  ;;
  ;; TIP: Use `buffer-name' to get a buffer's name for customization.
  (setopt popper-mode-line nil)

  (after! [projectile]
    (setopt popper-group-function #'popper-group-by-projectile)))

;;; `ace-window' :: <https://github.com/abo-abo/ace-window>
;; TODO: keybindings
(use-package ace-window
  :after avy)

(provide 'init-window)
;;; init-window.el ends here
