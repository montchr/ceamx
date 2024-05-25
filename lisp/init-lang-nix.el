;;; init-lang-nix.el --- Nix language support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

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

(require 'ceamx-lib)

(require 'config-prog)

(require 'lib-help)
(package! nix-mode
  (when (eq 'eglot ceamx-lsp-client)
    (add-hook 'nix-mode-hook #'eglot-ensure))
  (when (eq 'lsp-mode ceamx-lsp-client)
    (add-hook 'nix-mode-hook #'lsp-deferred)))
(package! nix-ts-mode
  (when (eq 'eglot ceamx-lsp-client)
    (add-hook 'nix-ts-mode-hook #'eglot-ensure))
  (when (eq 'lsp-mode ceamx-lsp-client)
    (add-hook 'nix-ts-mode-hook #'lsp-deferred)))
(after! reformatter
  (reformatter-define nixfmt-format
    :group 'ceamx
    :program "nixfmt")

  (add-hook 'nix-mode-hook #'nixfmt-format-on-save-mode)
  (add-hook 'nix-ts-mode-hook #'nixfmt-format-on-save-mode))
(with-eval-after-load 'apheleia
  (add-to-list 'safe-local-variable-values '(apheleia-formatter . nixfmt))
  (add-to-list 'apheleia-mode-alist '(nix-mode . nixfmt))
  (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt)))
(after! reformatter
  (reformatter-define alejandra-format
    :group 'ceamx
    :program "alejandra"))
(with-eval-after-load 'apheleia
  (add-to-list 'safe-local-variable-values '(apheleia-formatter . alejandra))
  (add-to-list 'apheleia-formatters '(alejandra "alejandra")))

;;; Configure Nix language servers

;; TODO: defcustom
(defvar ceamx-lsp-server-nix-lang "nixd")

(with-eval-after-load 'eglot
  (defvar eglot-server-programs)

  (add-to-list 'eglot-server-programs `((nix-mode nix-ts-mode) . (,ceamx-lsp-server-nix-lang))))

;; via `lsp-mode'
(after! lsp-nix
  (setopt lsp-nix-nil-formatter nil)

  (when (string= "nixd" ceamx-lsp-server-nix-lang)
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                      :major-modes '(nix-mode nix-ts-mode)
                      :priority 0
                      :server-id 'nixd))))
(def-hook! +devdocs-install-nix-docs ()
  '(nix-mode-hook nix-ts-mode-hook)
  "Install `devdocs' documents for the Nix language."
  (+devdocs-maybe-install "nix"))
(require 'config-keys)

(with-eval-after-load 'nix-mode
  (keymap-set nix-mode-map ceamx-keys-repl-toggle #'nix-repl)

  ;; FIXME: this is dumb, but the simplest way i found to avoid yelling without use-package
  (with-eval-after-load 'tempel
    ;; (eval-when-compile (require 'tempel))
    (tempel-key "C-c i t a" modargs nix-mode-map)))

(with-eval-after-load 'nix-ts-mode
  (keymap-set nix-ts-mode-map ceamx-keys-repl-toggle #'nix-repl))

(with-eval-after-load 'nix-repl
  (keymap-set nix-repl-mode-map ceamx-keys-repl-toggle #'quit-window))

(provide 'init-lang-nix)
;;; init-lang-nix.el ends here
