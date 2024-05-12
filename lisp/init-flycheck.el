;;; init-flycheck.el --- Flycheck support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chris@cdom.io>

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
;;; Code:

(require 'ceamx-keymaps)
(require 'ceamx-lib)
(package! flycheck
  (setopt flycheck-emacs-lisp-load-path 'inherit)

  ;; The default includes `newline', which is just too often.
  (setopt flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  (setopt flycheck-idle-change-delay 1.0)
  (setopt flycheck-display-errors-delay 0.25)
  (setopt flycheck-buffer-switch-check-intermediate-buffers t)

  (keymap-set ceamx-toggle-map "f" #'flycheck-mode)

  (add-hook 'ceamx-after-init-hook #'global-flycheck-mode))
(package! consult-flycheck
  (keymap-global-set "M-g f" #'consult-flycheck)

  (after! (consult flycheck)
    (require 'consult-flycheck)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
