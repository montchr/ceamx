;;; init-lang-nix.el --- Nix language support -*- lexical-binding: t -*-

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

;;  Configure Nix language support.

;;; Code:

(require 'ceamx-lib)
(require 'lib-help)

;;; Install and configure ~nix-mode~

;; <https://github.com/NixOS/nix-mode>

;; NOTE: ~nix-mode~ should not be loaded when using ~nix-ts-mode~.

(package! nix-mode
  (require 'nix-mode)

  (when (eq 'eglot ceamx-lsp-client)
    (add-hook 'nix-mode-hook #'eglot-ensure))

  (when (eq 'lsp-mode ceamx-lsp-client)
        (add-hook 'nix-mode-hook #'lsp-deferred)))

;;; DISABLED Install and configure ~nix-ts-mode~

;; <https://github.com/remi-gelinas/nix-ts-mode>

;; FIXME: Disabled because it is currently inferior to ~nix-mode~ in a few ways,
;; especially broken indentation and pairing:
;; <https://github.com/remi-gelinas/nix-ts-mode/issues/14>
;; <https://github.com/remi-gelinas/nix-ts-mode/pull/15>

(noop!
  (package! nix-ts-mode
    (when (treesit-language-available-p 'nix)
      (require 'nix-ts-mode)

      (add-hook 'nix-ts-mode-hook #'eglot-ensure)

      (when (eq 'eglot ceamx-lsp-client)
        (add-hook 'nix-ts-mode-hook #'eglot-ensure))

      (when (eq 'lsp-mode ceamx-lsp-client)
        (add-hook 'nix-ts-mode-hook #'lsp-deferred))

      (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
      (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))))

;;; Configure formatters

;;;; Set the official formatter (=nixfmt=) as the default formatter

(with-eval-after-load 'apheleia
  (defvar apheleia-mode-alist)
  (with-eval-after-load 'nix-ts-mode
    (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt))))

;;;; Register =alejandra= as an additional formatter

(with-eval-after-load 'apheleia
  (defvar apheleia-formatters)
  (add-to-list 'apheleia-formatters '(alejandra "alejandra")))

;;; Configure Nix language servers

(with-eval-after-load 'eglot
  (defvar eglot-server-programs)
  ;; (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil")))
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nixd"))))

;; via `lsp-mode' package
(with-eval-after-load 'lsp-nix
  (setopt lsp-nix-nil-formatter nil)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                    :major-modes '(nix-mode nix-ts-mode)
                    :priority 0
                    :server-id 'nixd)))

;;; Install ~devdocs~ Nix docset

(def-hook! +devdocs-install-nix-docs ()
  '(nix-mode-hook nix-ts-mode-hook)
  "Install `devdocs' documents for the Nix language."
  (+devdocs-maybe-install "nix"))

;;; Keybindings

;; These are too annoying to maintain for both ~nix-mode~ and ~nix-ts-mode~
;; because ~nix-ts-mode~ does not derive from ~nix-mode~ and I'm not using it
;; right now anyway.  So mode-specific keybindings stay in ~nix-mode~ only.

(require 'config-keys)

(with-eval-after-load 'nix-mode
  (defvar nix-mode-map)
  (defvar nix-repl-mode-map)
  (declare-function nix-repl "nix-repl")

  (keymap-set nix-mode-map ceamx-keys-repl-toggle #'nix-repl)
  (keymap-set nix-repl-mode-map ceamx-keys-repl-toggle #'quit-window)

  ;; FIXME: this is dumb, but the simplest way i found to avoid yelling without use-package
  (with-eval-after-load 'tempel
    ;; (eval-when-compile (require 'tempel))
    (tempel-key "C-c i t a" modargs nix-mode-map)))

(provide 'init-lang-nix)
;;; init-lang-nix.el ends here
