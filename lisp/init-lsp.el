;;; init-lsp.el --- LSP configuration -*- lexical-binding: t -*-

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

;;  Configuration for LSP servers.

;;; Code:

(defvar +lsp-defer-shutdown 3)

(autoload '+lsp-defer-server-shutdown-a "lib-lsp")

(use-package lsp-mode
  :commands (lsp-deferred)

  :init
  (setq lsp-use-plists t)
  (setq lsp-restart 'ignore)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Keybindings are handled manually.
  (setq lsp-keymap-prefix nil)

  ;;; Disable performance-hindering features.
	(setq lsp-enable-file-watchers nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-log-io nil)

  :general
  (+general-global-code
    "a"  '(lsp-execute-code-action :which-key "action")
    "r"  '(lsp-rename :which-key "rename..."))

  :config
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-enable-on-type-formatting nil)
  ;; FIXME: snippets are good? but this depends on yasnippet
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-xref t)
  (setq lsp-lens-enable t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-code-actions-segments '(count icon name))
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-workspace-status-enable t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-signature-auto-activate '(:on-trigger-char :on-server-request :after-completion))

  (advice-add '+lsp-defer-server-shutdown-a :around #'lsp--shutdown-workspace)

  :hook
  ((lsp-mode . (lambda () (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :after (lsp-mode)
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
	(setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-delay 0.2)
  (setq lsp-ui-doc-max-width 50)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-show-with-cursor t)

  (setq lsp-ui-sideline-delay 0.2)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode 'point))

(use-package consult-lsp
  :after (lsp-mode consult)
  :general
  (+general-global-search
    "j" '(consult-lsp-file-symbols :which-key "file symbols")
    "J" '(consult-lsp-symbols :which-key "workspace symbols"))
  (lsp-mode-map
   [remap xref-find-apropos]  #'consult-lsp-symbols))

;;; optionally:
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language



(provide 'init-lsp)
;;; init-lsp.el ends here
