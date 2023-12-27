;;; init-lang-nix.el --- Nix language support -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

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

(autoload 'after! "lib-common" t)
(autoload 'reformatter-define "reformatter" t)

(use-package nix-mode
  ;; NOTE: `magit-section' is a hard dependency (but why?),
  ;;       but does not install automatically with `nix-mode'
  :after (magit-section)
  :functions (nix-mode-hook))

(after! [nix-mode reformatter]
  ;; <https://github.com/kamadorueda/alejandra>
  (reformatter-define nix-format-alejandra
    :program "alejandra")

  ;; <https://github.com/serokell/nixfmt>
  (reformatter-define nix-format-nixfmt
    :program "nixfmt"))

(after! [nix-mode aggressive-indent]
  (add-to-list 'aggressive-indent-excluded-modes 'nix-mode))

(after! [nix-mode smartparens]
  (sp-pair "''" "''"))

(use-feature! eglot
  :after (nix-mode)
  :defines (eglot-server-programs)

  :config
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil"))))

;; via `lsp-mode' package
(use-feature! lsp-nix
  :after (lsp-mode)
  :defines (lsp-nix-nil-formatter)
  :config
  (setopt lsp-nix-nil-formatter nil))

(provide 'init-lang-nix)
;;; init-lang-nix.el ends here
