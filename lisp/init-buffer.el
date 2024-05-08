;;; init-buffer.el --- Buffers configuration         -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

;;; Code:

(require 'ceamx-lib)
(require 'lib-buffer)

(setq-default indicate-empty-lines nil)
(setq-default fill-column 80)

;; Available cycle positions for `recenter-top-bottom'.
(setopt recenter-positions '(top middle bottom))

;; Disable buffer line wrapping by default.
(setq-default truncate-lines t)

(setopt scroll-error-top-bottom t)

;; Prevent unwanted horizontal scrolling upon navigation.
(setopt scroll-preserve-screen-position t)

(setopt scroll-conservatively 1)

;; Add a margin when scrolling vertically (or don't).
(setq-default scroll-margin 4)

(define-keymap :keymap (current-global-map)
  ;; The default bindings feel backwards to me.
  "C-x <" #'scroll-right
  "C-x >" #'scroll-left

  "<wheel-left>" #'scroll-left
  "<wheel-right>" #'scroll-right)

;; Ensure the non-file-visiting buffers are also auto-reverted as needed. For
;; example, this will cause Dired to refresh a file list when the directory
;; contents have changed.
(setopt global-auto-revert-non-file-buffers t)

;; Automatically revert a buffer if its file has changed on disk.
(add-hook 'ceamx-after-init-hook #'global-auto-revert-mode)

(setopt ibuffer-movement-cycle t)

;; FIXME: auto-select window
(keymap-global-set "C-x C-b" #'ibuffer-list-buffers)

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

(autoload 'goto-address-prog-mode "goto-addr")

(add-hook 'prog-mode-hook #'goto-address-prog-mode)

(with-eval-after-load 'uniquify
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")

  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)

  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))

(package! link-hint
  (global-keys!
    "M-g u" #'link-hint-open-link
    "M-g U" #'link-hint-copy-link))

(package! expand-region
  (keymap-global-set "C-=" #'er/expand-region))

(package! rainbow-mode)

(package! lentic
  (global-lentic-mode))

(with-eval-after-load 'lentic
  (add-to-list 'safe-local-variable-values '(lentic-init . lentic-orgel-org-init)))

(global-keys!
  "C-c [" #'previous-buffer
  "C-c ]" #'next-buffer
  "C-c `" #'mode-line-other-buffer)

(provide 'init-buffer)
;;; init-buffer.el ends here
