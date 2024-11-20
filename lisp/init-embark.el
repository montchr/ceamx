;;; init-embark.el --- Configuration for Embark  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>

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

(require 'ceamx-lib)

(package! embark
  (require 'embark)

  (setopt embark-indicators '(;; embark--vertico-indicator
                              ;; embark-mixed-indicator
                              embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))
  (setopt embark-mixed-indicator-delay 2.0)

  ;; You can reverse the configured behavior at any time by calling `embark-act'
  ;; with a "C-u" prefix argument.
  ;; For finer control, e.g.: `((kill-buffer . t) (t . nil))'
  ;; This only affects the behavior of `embark-act' inside the minibuffer.
  (setopt embark-quit-after-action nil))

(after! vertico
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(setopt prefix-help-command #'embark-prefix-help-command)

(package! embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;; NOTE: This key may be the default in GNOME Desktop for emoji input.  However,
;; I have not encountered a conflict on GNOME, so I must be doing something
;; conveniently correct in my GNOME configurations.  FWIW, I have enabled the
;; Emacs-style keybindings there.
(keymap-global-set "C-." #'embark-act)

;; NOTE: While this overrides the default binding for `xref-find-definitions',
;; the result of calling `embark-dwim' on a symbol still ends up calling
;; `xref-find-definitions' as the default "dwim" action.
(keymap-global-set "M-." #'embark-dwim)

(keymap-global-set "C-h b" #'embark-bindings) ; orig: `describe-bindings'
;; TODO: make sure this doesn't override something, and if it does, note it
;; (keymap-global-set "C-h B" #'describe-bindings)

(after! embark
  (defvar-keymap ceamx-embark-tab-actions
    :doc "Keymap for Embark actions for `tab-bar' tabs (when mentioned by name)."
    :parent embark-general-map

    "s" #'tab-bar-select-tab-by-name
    "d" #'tab-bar-close-tab-by-name
    "R" #'tab-bar-rename-tab-by-name)

  (add-to-list 'embark-keymap-alist '(tab . ceamx-embark-tab-actions))

  (push #'embark--confirm
        (alist-get 'tab-bar-close-tab-by-name
                   embark-pre-action-hooks)))

(with-eval-after-load 'embark
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(provide 'init-embark)
;;; init-embark.el ends here
