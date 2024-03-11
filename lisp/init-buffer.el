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

;; Buffers of all shapes and sizes.

;; TODO: <https://github.com/abo-abo/avy/wiki/custom-commands>

;;; Code:

;;;; Requirements

(require 'elpaca-autoloads)

(require 'lib-common)
(require 'lib-keys)
(require 'lib-buffer)

;;;; General

(setq-default indicate-empty-lines nil)
(setq-default fill-column 80)

(use-feature! emacs
  :config

  ;; Available cycle positions for `recenter-top-bottom'.
  (setopt recenter-positions '(middle top bottom))

  ;; Disable buffer line wrapping by default.
  (set-default 'truncate-lines t)

;;;;; Scrolling

  (setopt scroll-error-top-bottom t)

  (global-keys!
    ;; The default bindings feel backwards to me.
    "C-x <" #'scroll-right
    "C-x >" #'scroll-left

    "<wheel-left>" #'scroll-left
    "<wheel-right>" #'scroll-right)

;;;;; Auto-revert buffers

  ;; Ensure the non-file-visiting buffers are also auto-reverted as needed. For
  ;; example, this will cause Dired to refresh a file list when the directory
  ;; contents have changed.
  (setopt global-auto-revert-non-file-buffers t)

  ;; (setopt auto-revert-interval 0.01)

  ;; Automatically revert a buffer if its file has changed on disk.
  (global-auto-revert-mode t)

;;;;; Buffer selection

  (keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)

  (setopt ibuffer-movement-cycle t))

;;;; Enable highlighting of the current line with `hl-line' [builtin]

(use-feature! hl-line
  :commands (hl-line-mode)
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'package-menu-mode-hook #'hl-line-mode))

;;;; Linkify URLs and email addresses in buffers with `goto-address' [builtin]

(use-feature! goto-addr
  :config
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;;;; Disambiguate identically-named buffers with `uniquify' [builtin]

(use-feature! uniquify
  :config
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")
  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)
  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))

;;;; Activate links in buffer with an `avy'-like UI via the `link-hint' package

;; <https://github.com/noctuid/link-hint.el>

(elpaca link-hint
  (global-keys!
    "M-g u" #'link-hint-open-link
    "M-g U" #'link-hint-copy-link))

;;;; `expand-region' :: <https://github.com/magnars/expand-region.el>

(elpaca expand-region
  (keymap-global-set "C-=" #'er/expand-region))

;;;; Enable simple comment-based outline features in many modes with `outli'

;; <https://github.com/jdtsmith/outli>

;; NOTE: In `emacs-lisp-mode' buffers, `outli-mode' should be enabled *after*
;; `lispy-mode'. See the package configuration for `lispy'.

(elpaca (outli :host github :repo "jdtsmith/outli")
  (def-hook! +outli-mode-maybe-enable-h ()
    '(prog-mode-hook text-mode-hook)
    "Enable `outli-mode' conditionally, excluding some modes."
    (let ((exclude-modes '(emacs-lisp-mode))
           (excludep (lambda (excluded-mode)
                       (eq major-mode excluded-mode))))
      (unless (seq-some excludep exclude-modes)
        (outli-mode))))

  (after! 'outli
    ;; FIXME: function definition is void -- from readme:
    ;; (advice-add 'load-theme :after #'outli-reset-all-faces)

    (define-keymap :keymap outli-mode-map
      "C-c C-n" #'outline-next-heading
      "C-c C-p" #'outline-previous-heading
      "C-c M-h" #'outline-promote
      "C-c M-l" #'outline-demote)))

;;; Colorize color names and hexcodes in buffers via `rainbow-mode'

;; <https://elpa.gnu.org/packages/rainbow-mode.html>

(elpaca rainbow-mode)

;;; Global Keybindings

(global-keys!
  "C-c [" #'previous-buffer
  "C-c ]" #'next-buffer
  "C-c `" #'mode-line-other-buffer)

(provide 'init-buffer)
;;; init-buffer.el ends here
