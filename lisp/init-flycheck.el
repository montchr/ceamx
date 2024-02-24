;;; init-flycheck.el --- Flycheck support            -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; <https://www.flycheck.org/en/latest/index.html>

;; If you are ever wonder "why do you need `flycheck'? Why not just use
;; `flymake'?" The answer is that `flymake' is pretty terrible compared to
;; `flycheck'.
;;
;; `flymake' does not even come anywhere close, especially in terms of language
;; support. At the time of writing, Flycheck's comparison states that Flymake
;; only supports 10 languages by default, with little option for expansion in
;; terms of third-party package support.
;;
;; <https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html>

;; FIXME: sometimes flycheck seems to randomly stop working and needs to be
;; disabled then re-enabled

;;; Code:

(require 'config-keys)

(require 'lib-common)
(require 'lib-keys)

(use-package flycheck
  :commands (global-flycheck-mode flycheck-mode)
  :defines (flycheck-emacs-lisp-load-path
             flycheck-check-syntax-automatically
             flycheck-global-modes)

  :init
  (setopt flycheck-emacs-lisp-load-path 'inherit)
  ;; The default includes `newline', which would happen too frequently.
  (setopt flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  (defmap! ceamx-toggle-map "f" #'flycheck-mode)

  (add-hook 'ceamx-after-init-hook #'global-flycheck-mode)

  :config

  (setopt flycheck-idle-change-delay 1.0)
  (setopt flycheck-display-errors-delay 0.25)
  (setopt flycheck-buffer-switch-check-intermediate-buffers t))

;;; Enable Flycheck integration with Consult via the `consult-flycheck' extension

(use-package consult-flycheck
  :after (consult flycheck)
  :commands (consult-flycheck)
  :init
  (keymap-global-set "M-g f" #'consult-flycheck))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
