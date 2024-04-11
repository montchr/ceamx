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

;; Requirements


;; [[file:../config.org::*Requirements][Requirements:1]]
(require 'lib-common)
(require 'lib-buffer)
;; Requirements:1 ends here

;; General


;; [[file:../config.org::*General][General:1]]
(setq-default indicate-empty-lines nil)
(setq-default fill-column 80)

;; Available cycle positions for `recenter-top-bottom'.
(setopt recenter-positions '(top middle bottom))

;; Disable buffer line wrapping by default.
(setq-default truncate-lines t)
;; General:1 ends here

;; Scrolling


;; [[file:../config.org::*Scrolling][Scrolling:1]]
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
;; Scrolling:1 ends here

;; Auto-revert buffers


;; [[file:../config.org::*Auto-revert buffers][Auto-revert buffers:1]]
;; Ensure the non-file-visiting buffers are also auto-reverted as needed. For
;; example, this will cause Dired to refresh a file list when the directory
;; contents have changed.
(setopt global-auto-revert-non-file-buffers t)

;; Automatically revert a buffer if its file has changed on disk.
(add-hook 'ceamx-after-init-hook #'global-auto-revert-mode)
;; Auto-revert buffers:1 ends here

;; Buffer selection


;; [[file:../config.org::*Buffer selection][Buffer selection:1]]
(setopt ibuffer-movement-cycle t)

;; FIXME: auto-select window
(keymap-global-set "C-x C-b" #'ibuffer-list-buffers)
;; Buffer selection:1 ends here

;; ~hl-line~: Enable highlighting of the current line


;; [[file:../config.org::*~hl-line~: Enable highlighting of the current line][~hl-line~: Enable highlighting of the current line:1]]
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'package-menu-mode-hook #'hl-line-mode)
;; ~hl-line~: Enable highlighting of the current line:1 ends here

;; ~goto-address~: Linkify URLs and email addresses in buffers [builtin]


;; [[file:../config.org::*~goto-address~: Linkify URLs and email addresses in buffers \[builtin\]][~goto-address~: Linkify URLs and email addresses in buffers [builtin]:1]]
(autoload 'goto-address-prog-mode "goto-addr")

(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; ~goto-address~: Linkify URLs and email addresses in buffers [builtin]:1 ends here

;; ~uniquify~: Disambiguate identically-named buffers [builtin]


;; [[file:../config.org::*~uniquify~: Disambiguate identically-named buffers \[builtin\]][~uniquify~: Disambiguate identically-named buffers [builtin]:1]]
(with-eval-after-load 'uniquify
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")

  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)

  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))
;; ~uniquify~: Disambiguate identically-named buffers [builtin]:1 ends here

;; ~link-hint~: Activate links in buffer with ~avy~

;; <https://github.com/noctuid/link-hint.el>


;; [[file:../config.org::*~link-hint~: Activate links in buffer with ~avy~][~link-hint~: Activate links in buffer with ~avy~:1]]
(package! link-hint
  (global-keys!
    "M-g u" #'link-hint-open-link
    "M-g U" #'link-hint-copy-link))
;; ~link-hint~: Activate links in buffer with ~avy~:1 ends here

;; ~expand-region~: Expand your regions

;; <https://github.com/magnars/expand-region.el>


;; [[file:../config.org::*~expand-region~: Expand your regions][~expand-region~: Expand your regions:1]]
(package! expand-region
  (keymap-global-set "C-=" #'er/expand-region))
;; ~expand-region~: Expand your regions:1 ends here

;; ~rainbow-mode~: Colorize color names and hexcodes in buffers

;; <https://elpa.gnu.org/packages/rainbow-mode.html>


;; [[file:../config.org::*~rainbow-mode~: Colorize color names and hexcodes in buffers][~rainbow-mode~: Colorize color names and hexcodes in buffers:1]]
(package! rainbow-mode)
;; ~rainbow-mode~: Colorize color names and hexcodes in buffers:1 ends here

;; ~lentic~: Create decoupled views of the same content


;; [[file:../config.org::*~lentic~: Create decoupled views of the same content][~lentic~: Create decoupled views of the same content:1]]
(package! lentic
  (global-lentic-mode))

(with-eval-after-load 'lentic
  (add-to-list 'safe-local-variable-values '(lentic-init . lentic-orgel-org-init)))
;; ~lentic~: Create decoupled views of the same content:1 ends here

;; Global Keybindings


;; [[file:../config.org::*Global Keybindings][Global Keybindings:1]]
(global-keys!
  "C-c [" #'previous-buffer
  "C-c ]" #'next-buffer
  "C-c `" #'mode-line-other-buffer)
;; Global Keybindings:1 ends here

(provide 'init-buffer)
;;; init-buffer.el ends here
