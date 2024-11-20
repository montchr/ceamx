;;; init-lsp.el --- Eglot support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2023-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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
(setopt eglot-sync-connect 1)
(setopt eglot-autoshutdown t)
(setopt eglot-send-changes-idle-time 0.5)

;; Disable events buffer, which poses performance issues over time as the
;; buffer grows in a longer-running Emacs instance.
(setopt eglot-events-buffer-size 0)

;; Prevent frequent focus-stealing.
(setopt eglot-auto-display-help-buffer nil)
(package! (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  (after! eglot
    (eglot-booster-mode)))
(setopt eglot-booster-no-remote-boost t)
(add-hook 'prog-mode-hook #'eglot-ensure)

(after! eglot
  (defvar eglot-server-programs)

  (def-advice! +eglot--ensure-available-mode (fn)
    :around #'eglot-ensure
    "Run `eglot-ensure' in supported modes."
    (when (alist-get major-mode eglot-server-programs nil nil
                     (lambda (modes key)
                       (if (listp modes)
                           (member key modes)
                         (eq key modes))))
      (funcall fn))))
(after! (eglot popper)
  (defvar popper-reference-buffers)
  (add-to-list 'popper-reference-buffers "^\\*eglot-help"))
(package! flycheck-eglot
  (add-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode))
(package! consult-eglot
  (defalias 'ceamx/list-workspace-symbols #'consult-eglot-symbols))
(keymap-global-set "C-c l a" '("action.." . eglot-code-actions))
(keymap-global-set "C-c l r" '("rename..." . eglot-rename))
(keymap-global-set "C-c l o" #'consult-eglot-symbols)

(after! eglot
  ;; Override the default binding for `xref-find-apropos'.
  (keymap-set eglot-mode-map "C-M-." #'consult-eglot-symbols))

(after! lsp-mode
    (keymap-global-set "C-c l o" #'consult-lsp-symbols)
    ;; Override the default binding for `xref-find-apropos'.
    (keymap-set lsp-mode-map "C-M-." #'consult-lsp-symbols))

(provide 'init-lsp)
;;; init-lsp.el ends here
