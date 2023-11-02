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

(use-feature emacs
;;
;;; `goto-address' (internal)
;; Linkify URLs and email addresses in buffers.

(use-feature goto-addr
  :config
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)

  (after! [evil]
    ;; TODO: allow graceful fail if not at URL
    ;; FIXME: breaks help buffer behavior -- thinks files are URLs to open in browser
    ;; (evil-define-key '(normal) 'global (kbd "<return>") #'goto-address-at-point)

    ))

;;; `uniquify' (internal)
;;  Disambiguate identically-named buffers.
(use-feature uniquify
  :config
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")
  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)
  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))

;;
;;; `expand-region' :: <https://github.com/magnars/expand-region.el>
;;

(use-package expand-region
  :commands er/expand-region
  :config
  (keymap-global-set "C-=" #'er/expand-region))

;;
;;; `scratch' :: <https://codeberg.org/emacs-weirdware/scratch>
;;
;;  Open a new scratch buffer initialized to the same major mode as the current buffer.

(use-package scratch
  :commands scratch
  :config
  (keymap-set cmx-buffer-keymap "X"
              `("*scratch* (same mode)" . ,(cmd! (scratch major-mode)))))

;;
;;; page-break-lines :: <https://github.com/purcell/page-break-lines>
;;

(use-package page-break-lines
  :defer 1
  :commands (global-page-break-lines-mode)
  :config
  (global-page-break-lines-mode))


(provide 'init-buffer)
;;; init-buffer.el ends here
