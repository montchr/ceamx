;;; init-term.el --- Terminal emulators inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

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

;; Configuration for terminal emulators running inside Emacs.

;; Not to be confused with configurations for Emacs specific to a TTY
;; environment, which are handled in `init-env-tty'.

;;; Code:

(require 'config-keys)
(require 'lib-common)

;;; `eat' :: <https://codeberg.org/akib/emacs-eat/>

;; "Emulate A Terminal"

(use-package eat
  :commands (eat
              eat-eshell-mode
              eat-eshell-visual-command-mode)
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  :config
  (keymap-set ceamx-launch-map "t" #'eat)

  (use-feature! popper
    :config
    (defvar popper-reference-buffers)
    (setopt popper-reference-buffers
      (append popper-reference-buffers '("\\*eat\\*")))))

(provide 'init-term)
;;; init-term.el ends here
