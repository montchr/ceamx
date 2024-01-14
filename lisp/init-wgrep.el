;;; init-wgrep.el --- Configuration for writable minibuffer grep  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>

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

;; "Writable grep buffer and apply the changes to files"
;;
;; <https://github.com/mhayashi1120/Emacs-wgrep>

;;; Code:

(require 'lib-common)
(require 'lib-wgrep)

(use-package wgrep
  :defines ( wgrep-auto-save-buffer
             wgrep-change-readonly-file)
  :commands (wgrep-change-to-wgrep-mode)

  :init
  (setopt wgrep-auto-save-buffer t)
  (setopt wgrep-change-readonly-file t)

  (keymap-set minibuffer-local-map "C-c C-e" #'+vertico/embark-export-write)

  (after! 'dired
    (keymap-set dired-mode-map "C-c C-e" #'wgrep-change-to-wgrep-mode))
  (after! 'grep
    (keymap-set grep-mode-map "W" #'wgrep-change-to-wgrep-mode))

  :config
  ;; FIXME: `wgrep-mode-map' does not exist -- maybe we want `grep-mode-map'?
  ;; (after! 'evil
  ;;   (evil-define-key '(normal motion) 'wgrep-mode-map "q" #'wgrep-exit))

  ;; FIXME: wrong num args
  ;; the intention is to close the wgrep popup after abort/finish
  ;; (after! 'popper
  ;;   (advice-add #'wgrep-abort-changes :after #'popper-toggle)
  ;;   (advice-add #'wgrep-finish-edit :after #'popper-toggle))
  )

(provide 'init-wgrep)
;;; init-wgrep.el ends here
