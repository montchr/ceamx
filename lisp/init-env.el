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

;; Dependencies


;; [[file:../config.org::*Dependencies][Dependencies:1]]
(require 'config-env)
(require 'ceamx-lib)
;; Dependencies:1 ends here

;; Disable unnecessary OS-specific command-line options :macos:


;; [[file:../config.org::*Disable unnecessary OS-specific command-line options][Disable unnecessary OS-specific command-line options:1]]
(unless +sys-mac-p
  (setq command-line-ns-option-alist nil))

(unless +sys-linux-p
  (setq command-line-x-option-alist nil))
;; Disable unnecessary OS-specific command-line options:1 ends here

;; ~exec-path-from=shell~: Inherit environment variables from variable environments :package:


;; [[file:../config.org::*~exec-path-from=shell~: Inherit environment variables from variable environments][~exec-path-from=shell~: Inherit environment variables from variable environments:1]]
(package! exec-path-from-shell
  (require 'exec-path-from-shell)
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "LSP_USE_PLISTS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))
;; ~exec-path-from=shell~: Inherit environment variables from variable environments:1 ends here

;; ~inheritenv~: Make temporary buffers inherit buffer-local environment variables :package:

;; - website :: <https://github.com/purcell/inheritenv>


;; [[file:../config.org::*~inheritenv~: Make temporary buffers inherit buffer-local environment variables][~inheritenv~: Make temporary buffers inherit buffer-local environment variables:1]]
(package! inheritenv
  (with-eval-after-load 'exec-path-from-shell
    (require 'inheritenv)))
;; ~inheritenv~: Make temporary buffers inherit buffer-local environment variables:1 ends here

;; ~with-editor~: Ensure shell/term modes use session as =$EDITOR= :package:


;; [[file:../config.org::*~with-editor~: Ensure shell/term modes use session as =$EDITOR=][~with-editor~: Ensure shell/term modes use session as =$EDITOR=:1]]
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
;; ~with-editor~: Ensure shell/term modes use session as =$EDITOR=:1 ends here

;; ~envrc~: Direnv integration :package:

;; - src :: <https://github.com/purcell/envrc>
;; - upstream :: <https://github.com/direnv/direnv>

;; Q: How does this differ from `direnv.el`?

;; <https://github.com/wbolster/emacs-direnv> repeatedly changes the global
;; Emacs environment, based on tracking what buffer you're working on.

;; Instead, `envrc.el` simply sets and stores the right environment in each
;; buffer, as a buffer-local variable.


;; [[file:../config.org::*~envrc~: Direnv integration][~envrc~: Direnv integration:1]]
(package! envrc
  (with-eval-after-load 'exec-path-from-shell
    (envrc-global-mode)))
;; ~envrc~: Direnv integration:1 ends here

;; Elpaca-Wait № 3: ~exec-path-from-shell~ :wait:


;; [[file:../config.org::*Elpaca-Wait № 3: ~exec-path-from-shell~][Elpaca-Wait № 3: ~exec-path-from-shell~:1]]
(elpaca-wait)
;; Elpaca-Wait № 3: ~exec-path-from-shell~:1 ends here

;; TRAMP Support


;; [[file:../config.org::*TRAMP Support][TRAMP Support:1]]
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
;; TRAMP Support:1 ends here

;; Terminal/TTY Support


;; [[file:../config.org::*Terminal/TTY Support][Terminal/TTY Support:1]]
(autoload 'mwheel-install "mwheel")
;; Terminal/TTY Support:1 ends here

;; [[file:../config.org::*Terminal/TTY Support][Terminal/TTY Support:2]]
(defun ceamx/console-frame-setup ()
  (xterm-mouse-mode 1)
  (mwheel-install))

;; Make the mouse wheel scroll.
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

;; (add-hook 'after-make-console-frame-hooks 'ceamx/console-frame-setup)
;; Terminal/TTY Support:2 ends here

(provide 'init-env)
;;; init-env.el ends here
