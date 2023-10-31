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
  :config
  ;; Linkify URLs and email addresses in all buffers.
  (global-goto-address-mode))

;;
;;; `ace-link' :: <https://github.com/abo-abo/ace-link>
;;
;;  Quickly jump to and follow links with `avy'

;; FIXME: seems entirely broken

;; (use-package ace-link
;;   :after avy

;;   :commands
;;   ace-link-addr
;;   ace-link-setup-default

;;   :config
;;   (ace-link-setup-default)
;;   (keymap-global-set "M-o" #'ace-link-addr)
;;   (after! [lispy]
;;     (keymap-set lispy-mode-map "M-o" nil))

;;   (after! [evil]
;;     ;; FIXME: won't work with evil normal state? "wrong number of arguments (4 . 4), 1"
;;     ;; (evil-define-key '(normal) 'cmx-intercept-mode-map "go" #'ace-link-addr)
;;     ;; (evil-define-key '(normal) 'cmx-intercept-mode-map "M-o" #'ace-link-addr)
;;     ))

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
  (keymap-set cmx-buffer-keymap "X" '("*scratch* (same mode)" . scratch)))


(provide 'init-buffer)
;;; init-buffer.el ends here
