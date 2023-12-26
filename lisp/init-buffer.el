;;; init-buffer.el --- Buffers configuration         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; See also `init-ui' for initial `avy' configuration.

;;; Code:

(require 'lib-keys)

;; Remember location in a file when saving files.
(setq save-place-file (expand-file-name "saveplace" cmx-local-dir))
(save-place-mode 1)

;; Disable buffer line wrapping by default.
(set-default 'truncate-lines t)

;;
;;; `goto-address' (internal)
;; Linkify URLs and email addresses in buffers.

(use-feature! goto-addr
  :config
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)

  (after! [evil]
    ;; TODO: allow graceful fail if not at URL
    ;; FIXME: breaks help buffer behavior -- thinks files are URLs to open in browser
    ;; (evil-define-key '(normal) 'global (kbd "<return>") #'goto-address-at-point)

    ))

;;; `uniquify' (internal)
;;  Disambiguate identically-named buffers.
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
              link-hint-open-link-at-point)
  :config
  (after! [evil]
    (evil-define-key '(normal) 'global "gO" #'link-hint-open-link)))

;; FIXME: how to bind to leader map??? no idea how the mode's internal keybinding works despite looking at source...
(use-feature! outline
  :defines (outline-minor-mode-map)
  :config
  (setq-default outline-minor-mode t)

  ;; Corresponds to default binding of `C-c @'.
  ;; FIXME: doesn't work
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


;;; page-break-lines :: <https://github.com/purcell/page-break-lines>
;;  Make form-feed (page-break) characters serve their purpose, visually.
(use-package page-break-lines
  :commands (global-page-break-lines-mode)
  :config
  (global-page-break-lines-mode))

;;; `rainbow-mode' :: <https://elpa.gnu.org/packages/rainbow-mode.html>
;;  colorize color names and hexcodes in buffers
(use-package rainbow-mode
  :defer t)

;; FIXME: `consult-project-buffer' does not reliably find files! cannot be trusted
;; (after! [consult-buffer]
;;   ;; TODO:
;;   ;; (setopt consult-buffer-filter ...)
;;   )

(provide 'init-buffer)
;;; init-buffer.el ends here
