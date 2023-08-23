;;; init-clippy.el --- Help -*- lexical-binding: t; -*-

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

;; "It looks like you're writing an Emacs. Would you like help?"

;;; Code:

(use-package helpful
  :config
  (keymap-global-set "<remap> <describe-function>" #'helpful-callable)
  (keymap-global-set "<remap> <describe-command>" #'helpful-command)
  (keymap-global-set "<remap> <describe-variable>" #'helpful-variable)
  (keymap-global-set "<remap> <describe-key>" #'helpful-key))

(use-package flycheck
  :commands (global-flycheck-mode)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq-default flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode +1))

;; FIXME: causes startup errors!
;; (use-package hyperbole
;;   :config
;;   (hyperbole-mode 1))

(provide 'init-clippy)
;;; init-clippy.el ends here
