;;; init-eglot.el --- Eglot support                  -*- lexical-binding: t; -*-

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

;; Configure Eglot.

;; TODO: Consider implementing Doom's approach to handling performance.
;; <https://github.com/doomemacs/doomemacs/blob/03d692f129633e3bf0bd100d91b3ebf3f77db6d1/modules/tools/lsp/config.el>

;;; Code:

;;; Requirements

(require 'ceamx-keymaps)

(require 'lib-common)

;;; Configure

(setopt eglot-sync-connect 1)
(setopt eglot-autoshutdown t)
(setopt eglot-send-changes-idle-time 0.5)

;; Disable events buffer, which poses performance issues over time as the
;; buffer grows in a longer-running Emacs instance.
(setopt eglot-events-buffer-size 0)

;; Prevent frequent focus-stealing.
(setopt eglot-auto-display-help-buffer nil)

(define-keymap :keymap ceamx-code-map
  "a" '("action.." . eglot-code-actions)
  "r" '("rename..." . eglot-rename))

(after! 'eglot
  (defvar eglot-server-programs)

  (def-advice! +eglot--ensure-available-mode (fn)
    :around #'eglot-ensure
    "Run `eglot-ensure' in supported modes."
    (when (alist-get major-mode eglot-server-programs nil nil
            (lambda (modes key)
              (if (listp modes)
                (member key modes)
                (eq key modes))))
      (funcall fn)))

  ;; Register eglot help buffers as popup windows.
  (after! [popper]
    (defvar popper-reference-buffers)
    (add-to-list 'popper-reference-buffers "^\\*eglot-help")))

;;; Configure Flycheck integration

(package! flycheck-eglot
  (add-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode))

;;; `consult-eglot' :: <https://github.com/mohkale/consult-eglot>

(package! consult-eglot
  (defalias 'ceamx/list-workspace-symbols #'consult-eglot-symbols))

(provide 'init-eglot)
;;; init-eglot.el ends here
