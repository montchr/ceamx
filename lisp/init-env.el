;;; init-env.el --- Environmental integrations  -*- lexical-binding: t;  -*-

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

(require 'config-env)
(require 'ceamx-lib)
;; Disable unnecessary OS-specific command-line options.
(unless +sys-mac-p
  (setq command-line-ns-option-alist nil))
(unless +sys-linux-p
  (setq command-line-x-option-alist nil))
(package! exec-path-from-shell
  (require 'exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "LSP_USE_PLISTS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
(package! inheritenv
  (with-eval-after-load 'exec-path-from-shell
    (require 'inheritenv)))
(package! with-editor
  (keymap-global-set "<remap> <async-shell-command>"
                     #'with-editor-async-shell-command)
  (keymap-global-set "<remap> <shell-command>"
                     #'with-editor-shell-command)

  (add-hook 'shell-mode-hook #'with-editor-export-editor)
  (add-hook 'eshell-mode-hook #'with-editor-export-editor)
  (add-hook 'term-exec-hook #'with-editor-export-editor)

  ;; Make sure that `eat' does not break `magit-commit'.
  ;; <https://codeberg.org/akib/emacs-eat/issues/55#issuecomment-871388>
  (with-eval-after-load 'eat
    (add-hook 'eat-mode-hook #'shell-command-with-editor-mode)))
(package! envrc
  (with-eval-after-load 'exec-path-from-shell
    (envrc-global-mode)))
(elpaca-wait)
(setopt tramp-default-method "ssh")
(setopt tramp-default-remote-shell "/bin/bash")
(setopt tramp-connection-timeout (* 60 10))
;; Do not auto-save remote files. Note the reversed logic.
(setopt remote-file-name-inhibit-auto-save t)                 ; Emacs 30
(setopt remote-file-name-inhibit-auto-save-visited t)
;; Avoid expensive operations on remote files.
(setopt remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30

(after! tramp
  (dolist (path '("~/.local/bin"
                  "~/.nix-profile/bin"
                  "~/.local/state/nix/profiles/profile/bin/"
                  "/nix/var/nix/profiles/default/bin"
                  "/run/current-system/sw/bin"))
    (add-to-list 'tramp-remote-path path)))

(provide 'init-env)
;;; init-env.el ends here
