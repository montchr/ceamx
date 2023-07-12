;;; init-defaults.el --- Better defaults -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;; Code:

;; > Help keeping ~/.emacs.d clean
;; <https://github.com/emacscollective/no-littering>
(use-package no-littering
  :demand t
  :preface
  (setq no-littering-etc-directory +path-etc-dir)
  (setq no-littering-var-directory +path-var-dir)

  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(setq-default
 ad-redefinition-action 'accept         ; Silence warnings for redefinition
 auto-save-list-file-prefix nil         ; Prevent tracking for auto-saves
 create-lockfiles nil                   ; Locks are more nuisance than blessing
 custom-unlispify-menu-entries nil      ; Prefer kebab-case for titles
 custom-unlispify-tag-names nil         ; Prefer kebab-case for symbols
 delete-by-moving-to-trash t            ; Delete files to trash
 fill-column 80                         ; Set width for automatic line breaks
 help-window-select t                   ; Focus new help windows when opened
 initial-major-mode #'fundamental-mode  ; Improve initial scratch buffer load time
 mouse-yank-at-point t                  ; Yank at point rather than pointer
 native-comp-async-report-warnings-errors 'silent ; Skip error buffers
 read-process-output-max (* 1024 1024)  ; Increase read size for data chunks
 recenter-positions '(5 bottom)         ; Set re-centering positions
 scroll-conservatively 101              ; Avoid recentering when scrolling far
 scroll-margin 1                        ; Add a margin when scrolling vertically
 select-enable-clipboard t              ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil          ; Use a single space after dots
 show-help-function t                   ; Enable help text everywhere
 uniquify-buffer-name-style 'forward    ; Uniquify buffer names
 use-short-answers t                    ; Replace yes/no prompts with y/n
 window-combination-resize t            ; Resize windows proportionally
 x-stretch-cursor t)                    ; Stretch cursor to the glyph width

(blink-cursor-mode 1)                   ; Prefer a blinking cursor
(delete-selection-mode 1)               ; Replace region when inserting text
(global-subword-mode 1)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point

(put 'downcase-region 'disabled nil)    ; Enable `downcase-region'
(put 'scroll-left 'disabled nil)        ; Enable `scroll-left'
(put 'upcase-region 'disabled nil)      ; Enable `upcase-region'

;; Increase number of messages saved in log.
(setq message-log-max 10000)

;; Default to UTF-8 for all of the things.
(set-language-environment "UTF-8")
;; `set-language-environment' also presumptively sets `default-input-method'.
(setq default-input-method nil)

;; Prevent unnecessary rendering/line scanning in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Disable extraneous OS window chrome.
(when (window-system)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

(show-paren-mode t)

;; Unbind `suspend-frame'
(global-unset-key (kbd "C-x C-z"))

;; Global indentation defaults
(setq-default
 indent-tabs-mode nil
 tab-always-indent 'complete ; Indent, then try completions
 tab-width 2)
(put 'add-function 'lisp-indent-function 2)
(put 'advice-add 'lisp-indent-function 2)
(put 'plist-put 'lisp-indent-function 2)

;; Disable bidirectional text scanning.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Remove unnecessary OS-specific command-line options while running
;; Emacs in a different OS.
(unless +sys-mac-p    (setq command-line-ns-option-alist nil))
(unless +sys-linux-p  (setq command-line-x-option-alist nil))

;; No littering the file system with backup files.
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setq auto-mode-case-fold nil)

;; "Don't ping things that look like domain names."
(setq ffap-machine-p-known 'reject)

;; Throttle UI refreshing slightly.
(setq idle-update-delay 1.0)  ; default is 0.5

;; PGTK-only: Improve childframe responsiveness (e.g. `lsp-ui').
;; See emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Ensure secrets and auth credentials are not stored in plaintext (the default).
;; Requires GnuPG configuration.
(setq auth-sources (list (file-name-concat +path-var-dir "authinfo.gpg")
                         "~/.authinfo.gpg"))

;; "More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling."
(setq fast-but-imprecise-scrolling t)

;; Restore Emacs session buffers, their file names, major modes, buffer
;; positions, window+frame configuration, "some important global variables".
(desktop-save-mode 1)

(provide 'init-defaults)
;;; init-defaults.el ends here
