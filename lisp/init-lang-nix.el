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

(require 'lib-common)

;;; Install and configure `nix-mode'

;; <https://github.com/NixOS/nix-mode>

(package! nix-mode
  (unless (treesit-language-available-p 'nix)
    (require 'nix-mode)

    (add-hook 'nix-mode-hook #'eglot-ensure)))

;;; Install and configure `nix-ts-mode'

;; <https://github.com/remi-gelinas/nix-ts-mode>

(package! nix-ts-mode
  (when (treesit-language-available-p 'nix)
    (require 'nix-ts-mode)

    (add-hook 'nix-ts-mode-hook #'eglot-ensure)

    (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
    (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))))

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
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil"))))

;; via `lsp-mode' package
(with-eval-after-load 'lsp-nix
  (setopt lsp-nix-nil-formatter nil))

(provide 'init-lang-nix)
;;; init-lang-nix.el ends here
