;;; init-lsp.el --- LSP configuration -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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

;;  Configuration for LSP servers.

;; FIXME: make this more generalized for "IDE" stuff, not just LSP

;;; Code:

(defvar +lsp-defer-shutdown 3)

(autoload '+lsp-defer-server-shutdown-a "lib-lsp")

;;
;;; `lsp-mode' :: <https://emacs-lsp.github.io/lsp-mode/>
;;

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-execute-code-action)
  :autoload (lsp--shutdown-workspace lsp-enable-which-key-integration)
  :defines (lsp-modeline-code-actions-segments) ; idk why tho

  :init
  ;; FIXME: ideally would be enabled, but lately has been failing
  (setopt lsp-use-plists nil)

  ;; (setopt lsp-restart 'ignore)
  (setopt lsp-headerline-breadcrumb-enable nil)
  ;; Keybindings are handled manually.
  ;; FIXME: figure out how to disable for real?
  ;; FIXME: invalid value -- should be string
  ;; (setopt lsp-keymap-prefix nil)

  ;;; Disable performance-hindering features.
	(setopt lsp-enable-file-watchers nil)
  (setopt lsp-enable-folding nil)
  (setopt lsp-enable-text-document-color nil)
  (setopt lsp-log-io nil)

  :config
  ;; > By default, lsp-mode configures `indent-region-function' so that Emacs uses
  ;; > language servers' `textDocument/rangeFormatting' request to format text in
  ;; > buffers. So EditorConfig settings are ignored unless language servers
  ;; > themselves support loading configs from .editorconfig.
  ;; via <https://github.com/editorconfig/editorconfig-emacs?tab=readme-ov-file#editorconfig-format-buffer-does-not-work-well-with-lsp-mode>
  (setopt lsp-enable-indentation nil)

  (setopt lsp-eldoc-enable-hover t)
  (setopt lsp-enable-on-type-formatting nil)
  ;; lsp-mode snippets depend on yasnippet, which we don't use.
  (setopt lsp-enable-snippet nil)
  (setopt lsp-enable-xref t)
  (setopt lsp-lens-enable t)
  (setopt lsp-modeline-code-actions-enable t)
  (setopt lsp-modeline-code-actions-segments '(count icon name))
  (setopt lsp-modeline-diagnostics-enable t)
  (setopt lsp-modeline-workspace-status-enable t)
  (setopt lsp-signature-render-documentation t)
  (setopt lsp-signature-auto-activate '(:on-trigger-char :on-server-request :after-completion))

  (advice-add '+lsp-defer-server-shutdown-a :around #'lsp--shutdown-workspace)

  (keymap-set lsp-mode-map "M-<return>" #'lsp-execute-code-action)

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; FIXME: needs hook or will not load (i think...?)
(use-package lsp-ui
  :after (lsp-mode)
  :commands (lsp-ui-mode)
  :config
  (setopt lsp-ui-doc-enable t)
  ;; `at-point' displays the ui just above the symbol,
  ;; obscuring the buffer contents that tend to be most relevant
  ;; as leading up to the thing at point.
  ;; `top' is similarly annoying, and it conflicts with sideline ui.
	(setopt lsp-ui-doc-position 'bottom)  ; alt: top, at-point
  ;; Long delay, as the doc is more helpful when I'm pausing in confusion.
  (setopt lsp-ui-doc-delay 1.0)
  ;; TODO: auto determine by window width?
  (setopt lsp-ui-doc-max-width 80)
  (setopt lsp-ui-doc-max-height 30)
  (setopt lsp-ui-doc-include-signature t)
  (setopt lsp-ui-doc-header t)
  (setopt lsp-ui-doc-show-with-cursor t)

  (setopt lsp-ui-sideline-delay 0.5)
  (setopt lsp-ui-sideline-show-diagnostics nil)
  (setopt lsp-ui-sideline-show-hover nil)
  (setopt lsp-ui-sideline-show-code-actions t)
  (setopt lsp-ui-sideline-update-mode 'point))

(use-package consult-lsp
  :after (lsp-mode consult)
  :commands (consult-lsp-symbols)
  :init
  ;; FIXME: incorrect rebind syntax -- i think there's some newer function for this
  ;; TODO: only when supported by language server? otherwise results in error
  (keymap-set lsp-mode-map "<remap> <xref-find-apropos>" #'consult-lsp-symbols))

;;; optionally:
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language



(provide 'init-lsp)
;;; init-lsp.el ends here
