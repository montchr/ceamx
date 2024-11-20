;;; init-lang-data.el --- Language support for data syntaxes  -*- lexical-binding: t;  -*-

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

(require 'ceamx-lib)
;; TODO: add bindings

;; (package! json-navigator)
(after! reformatter
  (reformatter-define toml-taplo-fmt
    :group 'ceamx
    :program "taplo"
    :args (list "format" "--diff"
                "--stdin-filepath" (buffer-file-name)
                "-"))

  (add-hook 'conf-toml-mode-hook #'toml-taplo-fmt-on-save-mode)
  (add-hook 'toml-ts-mode-hook #'toml-taplo-fmt-on-save-mode))
(when (featurep 'lsp-toml)
  (setopt lsp-toml-cache-path (file-name-as-directory
                               (concat ceamx-lsp-mode-cache-dir "server/toml"))))
(package! yaml-mode)
(package! yaml-pro
  (add-hook 'yaml-mode-hook #'yaml-pro-mode)
  (add-hook 'yaml-ts-mode-hook #'yaml-pro-mode)

  ;; The package does not expose `yaml-pro-ts-mode' until `yaml-pro-mode' is
  ;; activated.  I consider this less-than-ideal behavior and should probably
  ;; file a bug report.
  (when (featurep 'treesit)
    (add-hook 'yaml-pro-mode-hook (lambda () (yaml-pro-ts-mode)))))
(when (eq 'lsp ceamx-lsp-client)
  (after! (yaml-mode)
    (add-hook 'yaml-mode-hook #'lsp-deferred)
    (add-hook 'yaml-ts-mode-hook #'lsp-deferred)))
(when (eq 'lsp ceamx-lsp-client)
  (setopt lsp-yaml-schemas nil)

  ;; Keep this cached file with all of the other LSP server caches.
  (setopt lsp-yaml-schema-store-local-db
          (file-name-concat ceamx-lsp-mode-cache-dir "server/yaml/lsp-yaml-schemas.json"))

  ;; Download the YAML Schema Store database if not present.
  ;; FIXME: handle periodic updates of cached data
  (after! lsp-yaml
    (defer! 2
      (unless (file-exists-p lsp-yaml-schema-store-local-db)
        (lsp-yaml-download-schema-store-db)))))
(use-feature! nxml-mode
  :mode "\\.p\\(?:list\\|om\\)\\'"      ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"        ; xslt, xsd
  :mode "\\.rss\\'"

  :config
  (setq nxml-slash-auto-complete-flag t)
  (setq nxml-auto-insert-xml-declaration-flag t))
(package! csv-mode)

(after! csv-mode
  (define-keymap :keymap csv-mode-map
    "a" #'csv-align-fields
    "u" #'csv-unalign-fields
    "s" #'csv-sort-fields
    "S" #'csv-sort-numeric-fields
    "k" #'csv-kill-fields
    "t" #'csv-transpose))

(provide 'init-lang-data)
;;; init-lang-data.el ends here
