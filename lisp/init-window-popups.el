;;; init-window-popups.el --- Popup window management  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

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

;; Configuration primarily for `popper'.

;;; Code:

(require 'config-buffer)
(require 'lib-common)

;;; popper :: <https://github.com/karthink/popper>

(use-package popper
  :blackout
  :commands ( popper-mode
              popper-cycle
              popper-cycle-backwards
              popper-echo-mode
              popper-group-by-projectile
              popper-toggle
              popper-toggle-type)
  :functions (popper-select-popup-at-bottom)

  :preface

  (defun +popper-select-below-fn (buffer &optional _alist)
    (funcall (if (> (frame-width) 170)
               ;; #'display-buffer-in-direction
               #'popper-select-popup-at-bottom
               #'display-buffer-at-bottom)
      buffer
      `((window-height . ,popper-window-height)
         (direction . below)
         (body-function . ,#'select-window))))

  :init
  (setopt popper-reference-buffers
    (append
      ceamx-help-modes-list
      ceamx-manual-modes-list
      ceamx-repl-modes-list
      ceamx-repl-names-list
      ceamx-occur-grep-modes-list
      '(+popper-current-buffer-popup-p)
      '(Custom-mode
         compilation-mode
         messages-buffer-mode)
      '("Output\\*$"
         "\\*Async-native-compile-log\\*"
         "\\*Compile-Log\\*"
         "\\*compilation\\*"     ; not necessarily covered by `compilation-mode'
         "\\*vc\\*"
         "\\*Warnings\\*"
         "\\*Embark Export"
         "^\\*Backtrace\\*"
         ;; TODO: not entirely sure if this pattern is correct
         "Profiler-Report"             ; report generated by `profiler' workflow
         "^\\*Apropos"
         "^Calc:"
         "^\\*eldoc\\*"
         "^\\*ielm\\*"
         "\\*Shell Command Output\\*"
         ("\\*Async Shell Command\\*" . hide)
         ("\\*Detached Shell Command\\*" . hide)
         "\\*Completions\\*"
         "[Oo]utput\\*")))

  ;; Load as early as possible to catch popups during startup.
  (add-hook 'ceamx-after-init-hook #'popper-mode)
  (add-hook 'ceamx-after-init-hook #'popper-echo-mode)

  :config
  (define-keymap :keymap (current-global-map)
    "C-`"   #'popper-toggle
    "C-~"   #'popper-cycle
    "C-M-`" #'popper-toggle-type)
  ;; "M-`"   #'popper-echo-mode

  (defvar-keymap popper-repeat-map
    :repeat t
    "`" #'popper-cycle
    "~" #'popper-cycle-backwards)

  ;; Prevent conflict with custom `display-buffer' rules.
  ;; <https://github.com/karthink/popper?tab=readme-ov-file#popup-placement-controlled-using-display-buffer-alist-or-shackleel>
  ;; (setopt popper-display-control nil)

  (setopt popper-display-function #'+popper-select-below-fn)

  (after! [projectile]
    (setopt popper-group-function #'popper-group-by-projectile)))

(provide 'init-window-popups)
;;; init-window-popups.el ends here
