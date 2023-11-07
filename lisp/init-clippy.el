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

;; If you are ever wonder "why do you need `flycheck'? Why not just use
;; `flymake'?" The answer is that `flymake' is pretty terrible compared to
;; `flycheck'. `flymake' does not even come anywhere close, especially in terms
;; of language support. At the time of writing, Flycheck's comparison states
;; that Flymake only supports 10 languages by default, with little option for
;; expansion in terms of third-party package support.
;;
;; <https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html>

;;; Code:

(use-package helpful
  :config
  (keymap-set help-mode-map "C-h" #'helpful-at-point)
  (dolist (map (list cmx-help-keymap help-mode-map))
    (define-keymap :keymap map
      "c" #'helpful-command
      "f" #'helpful-callable
      "h" #'helpful-at-point
      "k" #'helpful-key
      "o" #'helpful-symbol
      "v" #'helpful-variable)))


(use-package flycheck
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq-default flycheck-global-modes '(prog-mode))
  ;; `global-flycheck-mode' seems to have no effect, so load on hook.
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (global-flycheck-mode +1))

(provide 'init-clippy)
;;; init-clippy.el ends here
