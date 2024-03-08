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

(require 'lib-common)
(require 'lib-keys)
(require 'lib-buffer)

;;; General

(setq-default indicate-empty-lines nil)
(setq-default fill-column 80)

(use-feature! emacs
  :config

  ;; Available cycle positions for `recenter-top-bottom'.
  (setopt recenter-positions '(middle top bottom))

  ;; Disable buffer line wrapping by default.
  (set-default 'truncate-lines t)

;;;; Auto-revert buffers

  ;; Ensure the non-file-visiting buffers are also auto-reverted as needed. For
  ;; example, this will cause Dired to refresh a file list when the directory
  ;; contents have changed.
  (setopt global-auto-revert-non-file-buffers t)

  ;; (setopt auto-revert-interval 0.01)

  ;; Automatically revert a buffer if its file has changed on disk.
  (global-auto-revert-mode t)

;;;; Buffer selection

  (keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)

  (setopt ibuffer-movement-cycle t))

;;; Enable highlighting of the current line with `hl-line' [builtin]

(use-feature! hl-line
  :commands (hl-line-mode)
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'package-menu-mode-hook #'hl-line-mode))

;;; Linkify URLs and email addresses in buffers with `goto-address' [builtin]

(use-feature! goto-addr
  :config
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;;; Disambiguate identically-named buffers with `uniquify' [builtin]

(use-feature! uniquify
  :config
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")
  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)
  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))

;;; `link-hint' :: <https://github.com/noctuid/link-hint.el>

;;  avy-based link jumping

(use-package link-hint
  :after (avy)
  :commands ( link-hint-open-link
              link-hint-open-link-at-point
              link-hint-copy-link)

  :init
  ;; FIXME: unavailable initially (probably due to `:after')
  (define-keymap :keymap (current-global-map)
    "M-s u" #'link-hint-open-link
    "M-s U" #'link-hint-copy-link)

  :config
  (after! [evil]
    (declare-function evil-define-key "evil")
    (evil-define-key '(normal) 'global
      "gO" #'link-hint-open-link)))

;; FIXME: how to bind to leader map??? no idea how the mode's internal keybinding works despite looking at source...
(use-feature! outline
  :defines (outline-minor-mode-map)
  :config
  (setq-default outline-minor-mode t)

  ;; Corresponds to default binding of `C-c @'.
  ;; FIXME: doesn't work -- conflict?
  ;;        what if it just needs to be global map with C-c ?
  ;; (keymap-set mode-specific-map "@" '("(outline)" . outline-mode-prefix-map))
  )

;;; `expand-region' :: <https://github.com/magnars/expand-region.el>

(use-package expand-region
  :commands er/expand-region
  :config
  (keymap-global-set "C-=" #'er/expand-region))

;;; `scratch' :: <https://codeberg.org/emacs-weirdware/scratch>

;;  Mode-specific scratch buffers.

(use-package scratch
  :commands scratch)

;;; `outli' :: <https://github.com/jdtsmith/outli>

;; Simple comment-based outlines

;; NOTE: In `emacs-lisp-mode' buffers, `outli-mode' should be enabled *after*
;; `lispy-mode'. See the package configuration for `lispy'.

(use-package outli
  :ensure (:host github :repo "jdtsmith/outli")

  :init
  (def-hook! +outli-mode-maybe-enable-h ()
    '(prog-mode-hook text-mode-hook)
    "Enable `outli-mode' conditionally, excluding some modes."
    (let ((exclude-modes '(emacs-lisp-mode))
          (excludep (lambda (excluded-mode)
                      (eq major-mode excluded-mode))))
      (unless (seq-some excludep exclude-modes)
        (outli-mode))))

  :config
  (define-keymap :keymap outli-mode-map
    "C-c C-n" #'outline-next-heading
    "C-c C-p" #'outline-previous-heading
    "C-c M-h" #'outline-promote
    "C-c M-l" #'outline-demote))


;;; page-break-lines :: <https://github.com/purcell/page-break-lines>

;;  Make form-feed (page-break) characters serve their purpose, visually.

(use-package page-break-lines
  :commands (global-page-break-lines-mode)
  :config
  ;; FIXME: no effect in buffer despite being enabled -- once toggled, it works
  (global-page-break-lines-mode))

;;; `rainbow-mode' :: <https://elpa.gnu.org/packages/rainbow-mode.html>

;; Colorize color names and hexcodes in buffers

(use-package rainbow-mode)

(provide 'init-buffer)
;;; init-buffer.el ends here
