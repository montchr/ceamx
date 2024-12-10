;;; init-buffer.el --- Buffer customizations  -*- lexical-binding: t;  -*-

;; Copyright (c) 2023-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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

(require 'ceamx-keymaps)
(require 'ceamx-lib)
(require 'lib-buffer)

;; Bind ~ceamx-buffer-prefix~ to "C-c b" :keybinds:


;; [[file:../config.org::*Bind ~ceamx-buffer-prefix~ to "C-c b"][Bind ~ceamx-buffer-prefix~ to "C-c b":1]]
(keymap-global-set "C-c b" '("Buffer" . ceamx-buffer-prefix))
;; Bind ~ceamx-buffer-prefix~ to "C-c b":1 ends here

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

(setopt scroll-conservatively 10000)

;; Add a margin when scrolling vertically (or don't).
;; (setq-default scroll-margin 4)

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

(setopt auto-revert-interval 2)         ; default: 5

;; Automatically revert a buffer if its file has changed on disk.
(add-hook 'ceamx-after-init-hook #'global-auto-revert-mode)
;; Auto-revert buffers:1 ends here

;; Diredishly operate on buffers with ~ibuffer~ [builtin]


;; [[file:../config.org::*Diredishly operate on buffers with ~ibuffer~ \[builtin\]][Diredishly operate on buffers with ~ibuffer~ [builtin]:1]]
(setopt ibuffer-movement-cycle t)
;; Diredishly operate on buffers with ~ibuffer~ [builtin]:1 ends here

;; ~mowie~: library to define repeatable "smart" point-moving commands :package:


;; [[file:../config.org::*~mowie~: library to define repeatable "smart" point-moving commands][~mowie~: library to define repeatable "smart" point-moving commands:1]]
(package! (mowie :host codeberg :repo "mekeor/mowie"))
;; ~mowie~: library to define repeatable "smart" point-moving commands:1 ends here

;; ~mwim~: Move-Where-I-Mean line positions :package:

;; + src :: <https://github.com/alezost/mwim.el/blob/master/README.org#usage>


;; [[file:../config.org::*~mwim~: Move-Where-I-Mean line positions][~mwim~: Move-Where-I-Mean line positions:1]]
(package! mwim
  (keymap-global-set "C-a" #'mwim-beginning)
  (keymap-global-set "C-e" #'mwim-end))
;; ~mwim~: Move-Where-I-Mean line positions:1 ends here

;; Linkify URLs and email addresses with ~goto-address~ [builtin]


;; [[file:../config.org::*Linkify URLs and email addresses with ~goto-address~ \[builtin\]][Linkify URLs and email addresses with ~goto-address~ [builtin]:1]]
(autoload 'goto-address-prog-mode "goto-addr")

(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; Linkify URLs and email addresses with ~goto-address~ [builtin]:1 ends here

;; Disambiguate buffer names with ~uniquify~ [builtin]


;; [[file:../config.org::*Disambiguate buffer names with ~uniquify~ \[builtin\]][Disambiguate buffer names with ~uniquify~ [builtin]:1]]
(with-eval-after-load 'uniquify
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")

  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)

  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))
;; Disambiguate buffer names with ~uniquify~ [builtin]:1 ends here

;; ~link-hint~: Activate links in buffer with ~avy~ :package:

;; <https://github.com/noctuid/link-hint.el>


;; [[file:../config.org::*~link-hint~: Activate links in buffer with ~avy~][~link-hint~: Activate links in buffer with ~avy~:1]]
(package! link-hint
  (global-keys!
    "M-g u" #'link-hint-open-link
    "M-g U" #'link-hint-copy-link))
;; ~link-hint~: Activate links in buffer with ~avy~:1 ends here

;; ~expand-region~: Expand your regions :package:

;; <https://github.com/magnars/expand-region.el>


;; [[file:../config.org::*~expand-region~: Expand your regions][~expand-region~: Expand your regions:1]]
(package! expand-region
  (keymap-global-set "C-=" #'er/expand-region))
;; ~expand-region~: Expand your regions:1 ends here

;; ~lentic~: Create decoupled views of the same content :package:


;; [[file:../config.org::*~lentic~: Create decoupled views of the same content][~lentic~: Create decoupled views of the same content:1]]
(package! lentic
  (global-lentic-mode))

(with-eval-after-load 'lentic
  (add-to-list 'safe-local-variable-values '(lentic-init . lentic-orgel-org-init)))
;; ~lentic~: Create decoupled views of the same content:1 ends here

;; Keybindings :keybinds:


;; [[file:../config.org::*Keybindings][Keybindings:1]]
(define-keymap :keymap (current-global-map)
  "C-c [" #'previous-buffer
  "C-c ]" #'next-buffer
  "C-c `" #'mode-line-other-buffer

  "C-x k" #'ceamx/kill-this-buffer      ; orig: `kill-buffer'
  "C-x K" #'kill-buffer
  "C-x C-b" #'ibuffer)

(define-keymap :keymap ceamx-buffer-prefix-map
  "b" '("switch (local)..." . consult-buffer)
  "k" #'ceamx/kill-this-buffer)
;; Keybindings:1 ends here

(provide 'init-buffer)
;;; init-buffer.el ends here
