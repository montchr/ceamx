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

(elpaca-use-package lsp-mode
  :commands (lsp-deferred)

  :init
  (setq lsp-use-plists t)
  (setq lsp-restart 'ignore)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Keybindings are handled manually.
  (setq lsp-keymap-prefix nil)

  ;;; Disable performance-hindering features.
	(setq lsp-enable-file-watchers nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-text-document-color nil)

  :general
  (+general-global-code
    "a"  '(lsp-execute-code-action :which-key "action")
    "r"  '(lsp-rename :which-key "rename..."))

  :config
  (advice-add '+lsp-defer-server-shutdown-a :around #'lsp--shutdown-workspace)

  :hook
  ((lsp-mode . (lambda () (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))
   (lsp-mode . lsp-enable-which-key-integration)))

(elpaca-use-package consult-lsp
  :after (lsp-mode consult)
  :general
  (+general-global-search
    "j" '(consult-lsp-file-symbols :which-key "file symbols")
    "J" '(consult-lsp-symbols :which-key "workspace symbols"))
  (lsp-mode-map
   [remap xref-find-apropos]  #'consult-lsp-symbols))

;;; optionally:
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language



(provide 'init-lsp)
;;; init-lsp.el ends here
