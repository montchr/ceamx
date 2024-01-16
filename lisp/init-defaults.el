;;; init-defaults.el --- Better defaults -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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

;;  Better is better, maybe.

;; TODO: move settings to appropriate feature files for discoverability

;;; Code:

(require 'ceamx-paths)
(require 'config-env)

;;; `no-littering' :: <https://github.com/emacscollective/no-littering>
;;  Help keeping ~/.emacs.d clean
(use-package no-littering
  :demand t
  :init
  (setq no-littering-etc-directory cmx-etc-dir)
  (setq no-littering-var-directory cmx-var-dir))

;; TODO: re-evaluate these settings
;; FIXME: how many of these actually need to use `setq-default'?
(setq-default
  ad-redefinition-action 'accept        ; Silence warnings for redefinition
  auto-save-list-file-prefix nil        ; Prevent tracking for auto-saves
  create-lockfiles nil                  ; Locks are more nuisance than blessing
  custom-unlispify-menu-entries nil     ; Prefer kebab-case for titles
  custom-unlispify-tag-names nil        ; Prefer kebab-case for symbols
  delete-by-moving-to-trash t           ; Delete files to trash
  help-window-select t                  ; Focus new help windows when opened
  mouse-yank-at-point t                 ; Yank at point rather than pointer
  ;; TODO: make sure this is correct
  read-process-output-max (* 1024 1024) ; Increase read size for data chunks
  recenter-positions '(5 bottom)        ; Set re-centering positions
  scroll-conservatively 101             ; Avoid recentering when scrolling far
  scroll-margin 1                       ; Add a margin when scrolling vertically
  ;; FIXME: probably does not work with `meow', which uses `meow-use-clipboard'
  select-enable-clipboard t             ; Merge system's and Emacs' clipboard
  sentence-end-double-space nil         ; Use a single space after dots
  show-help-function t                  ; Enable help text everywhere
  uniquify-buffer-name-style 'forward   ; Uniquify buffer names
  use-short-answers t                   ; Replace yes/no prompts with y/n
  window-combination-resize t           ; Resize windows proportionally
  x-stretch-cursor t)                    ; Stretch cursor to the glyph width

(blink-cursor-mode 1)                   ; Prefer a blinking cursor
(delete-selection-mode 1)               ; Replace region when inserting text
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point

;; TODO: why are these disabled by default?
(put 'downcase-region 'disabled nil)    ; Enable `downcase-region'
(put 'scroll-left 'disabled nil)        ; Enable `scroll-left'
(put 'upcase-region 'disabled nil)      ; Enable `upcase-region'

;; Improve initial scratch buffer load time
;; FIXME: "variable without a source file"
(setq default-major-mode #'text-mode)
(setopt initial-major-mode #'text-mode)

(setopt fill-column 80)

;; Increase number of messages saved in log.
;; TODO: this is a lot! when will this ever be necessary?
(setq message-log-max 10000)

;; Default to UTF-8 for all of the things.
(set-language-environment "UTF-8")
;; `set-language-environment' also presumptively sets `default-input-method'.
;; TODO: remove? this seems to be the default value
(setopt default-input-method nil)

;; Prevent unnecessary rendering/line scanning in non-focused windows.
;; Seeing a cursor in a window other than the active window is pretty confusing.
(setq-default cursor-in-non-selected-windows nil)
;; Improve visual contrast between focused/non-focused windows.
(setopt highlight-nonselected-windows nil)

;; Disable extraneous OS window chrome.
(when (window-system)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  ;; Display help text in echo area, not in a popup.
  ;; TODO: why?
  (tooltip-mode -1)
  (scroll-bar-mode -1))

;; Highlight matching parentheses.
(show-paren-mode t)

;; Unbind `suspend-frame'.
;; TODO: provide more context
(global-unset-key (kbd "C-x C-z"))

;; Disable bidirectional text scanning, because I don't need it.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Remove unnecessary OS-specific command-line options while running
;; Emacs in a different OS.
;; TODO: any way to avoid "variable without a source file"?
(unless +sys-mac-p
  (setq command-line-ns-option-alist nil))
(unless +sys-linux-p
  (setq command-line-x-option-alist nil))

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setopt auto-mode-case-fold nil)

;; Prevent Emacs from pinging domain names unexpectedly.
;;
;; NOTE: Emacs 29.1+ will no longer ping domain names (see the NEWS file), but
;; since it's so important from a privacy standpoint, it should still be set
;; explicitly in case, for some reason, this config is loaded by an earlier
;; version of Emacs.
(setopt ffap-machine-p-known 'reject)

;; Throttle UI refreshing slightly.
(setopt idle-update-delay 1.0)  ; default is 0.5

;; PGTK-only: Improve childframe responsiveness (e.g. `lsp-ui').
;; See emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Ensure secrets and auth credentials are not stored in plaintext (the default).
;; Requires GnuPG configuration.
(setopt auth-sources (list (file-name-concat cmx-var-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

;; "More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling."
;; TODO: move to `init-ui'?
(setopt fast-but-imprecise-scrolling t)

;; Don’t compact font caches during garbage collection.
;; NOTE: Set pre-emptively, anticipating lag from `doom-modeline',
;;       as recommended in its readme.
;;       <https://github.com/seagle0128/doom-modeline#faq>
(setq inhibit-compacting-font-caches t)

;; FIXME: Ideally, this action should only affect edited lines.
;; TODO: move to `init-editor'
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setopt use-file-dialog nil)
(setopt use-dialog-box nil)
(setopt indicate-empty-lines nil)

(setq-default cursor-type '(hbar .  2))
(setq-default cursor-in-non-selected-windows nil)

(provide 'init-defaults)
;;; init-defaults.el ends here
