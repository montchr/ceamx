;;; init-lsp-mode.el --- LSP-Mode support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chris@cdom.io>

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
;;; Code:

(package! lsp-mode
  (setopt lsp-keymap-prefix "C-c l")
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(package! lsp-ui)

(after! popper
  (add-to-list 'popper-reference-buffers (rx bol "*lsp-" (group (or "help" "install")))))
(setopt
 lsp-enable-folding nil
 lsp-enable-on-type-formatting nil
 lsp-headerline-breadcrumb-enable nil)
(setopt lsp-ui-peek-enable t
        lsp-ui-doc-max-width 72
        lsp-ui-doc-max-height 8
        lsp-ui-doc-delay 0.2
        ;; Prevent the doc flyout from disappearing on hover.
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point
        ;; Prevent conflict with Flycheck error overlays.
        lsp-ui-sideline-show-hover nil)

(after! lsp-ui
  lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
(after! (lsp-mode corfu)
  (setopt lsp-completion-provider :none)

  (defun +orderless-dispatch-flex-first-h (_pattern index _total)
    "Dispatch flex completion styles before all others.
Intended for use as a hook function on `orderless-style-dispatchers'."
    (and (eq index 0) 'orderless-flex))

  (def-hook! +lsp-mode-setup-completion-h ()
    'lsp-completion-mode-hook
    "Use `orderless' completion style with `lsp-mode'."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Consider the first word as a flex filter.
    (add-hook 'orderless-style-dispatchers #'+orderless-dispatch-flex-first-h nil 'local)
    (setq-local completion-at-point-functions
                (list (cape-capf-buster #'lsp-completion-at-point)))))
(require 'json)

(after! lsp-mode
  (def-advice! lsp-booster--json-parse-as-bytecode-a (old-fn &rest args)
    :around #'json-parse-buffer
    "Maybe parse JSON as bytecode from \"emacs-lsp-booster\"."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args))))
(after! lsp-mode
  (def-advice! lsp-booster--resolve-final-command-a (old-fn cmd &optional test?)
    :around #'lsp-resolve-final-command
    "Wrap the LSP server command CMD with a call to \"emacs-lsp-booster\"."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result))))
(package! consult-lsp
  (after! lsp-mode
    ;; Override the default binding for `xref-find-apropos'.
    (keymap-set lsp-mode-map "C-M-." #'consult-lsp-symbols)))
(package! dap-mode
  (dap-auto-configure-mode))

(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here
