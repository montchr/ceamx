;;; init-help.el --- Help -*- lexical-binding: t; -*-

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
  :commands ( helpful-at-point helpful-command helpful-callable
              helpful-key helpful-symbol helpful-variable)
  :config
  (keymap-set help-mode-map "C-h" #'helpful-at-point)
  ;; FIXME: partially moved into `init-keys-bindings', but not yet the `help-mode-map' overrides
  ;; (dolist (map (list cmx-help-keymap help-mode-map))
  ;;   (define-keymap :keymap map
  ;;     "c" #'helpful-command
  ;;     "f" #'helpful-callable
  ;;     "h" #'helpful-at-point
  ;;     "k" #'helpful-key
  ;;     "o" #'helpful-symbol
  ;;     "v" #'helpful-variable))
  )

;;; `elisp-demos' :: <https://github.com/xuchunyang/elisp-demos>
;;  Display usage examples inside help buffers for Emacs Lisp callables.
(use-package elisp-demos
  :after (helpful)
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(provide 'init-help)
;;; init-help.el ends here
