;;; init-env.el --- Environment configuration -*- lexical-binding: t -*-

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

;;  Ensure proper integration with the user environment.

;;; Code:

(use-package exec-path-from-shell
  :demand t
  :commands (exec-path-from-shell-initialize)
  :defines (exec-path-from-shell-variables)

  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;; inheritenv
;;  Make temp buffers inherit buffer-local environment variables.
;;  <https://github.com/purcell/inheritenv>
(use-package inheritenv
  :demand t
  :after (exec-path-from-shell))

(use-package envrc
  :demand t
  :after (exec-path-from-shell)
  ;; FIXME: because exec-path-from-shell doesn't start reliably, this never happens
  ;; :when (executable-find "direnv")
  :commands (envrc-global-mode)
  :init
  (envrc-global-mode)
  :config
  ;; <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/modules/tools/direnv/config.el#L11-L24>
  (defun +direnv-init-global-mode-earlier-h ()
    (let ((fn #'envrc-global-mode-enable-in-buffers))
      (if (not envrc-global-mode)
          (remove-hook 'change-major-mode-after-body-hook fn)
        (remove-hook 'after-change-major-mode-hook fn)
        (add-hook 'change-major-mode-after-body-hook fn 100))))
  ;; TODO: remove or re-enable
  ;;(add-hook 'envrc-global-mode-hook #'+direnv-init-global-mode-earlier-h)
  )



(provide 'init-env)
;;; init-env.el ends here
