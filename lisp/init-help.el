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

;; FIXME: there are some blocking bugs that have gone unfixed for quite a while
;;        some symbols' helpful pages cannot be displayed.
;;        <https://github.com/Wilfred/helpful/issues/329>
;;        consider maybe: <https://www.emacswiki.org/emacs/HelpPlus>
(use-package helpful
  :commands ( helpful-at-point helpful-command helpful-callable
              helpful-key helpful-symbol helpful-variable)
  :config
  ;; Meow compatibility, overriding default target of "SPC h h".
  (keymap-set help-map "C-h" #'helpful-at-point))

;;; `elisp-demos' :: <https://github.com/xuchunyang/elisp-demos>
;;  Display usage examples inside help buffers for Emacs Lisp callables.
(use-package elisp-demos
  :after (helpful)
  :autoload (elisp-demos-advice-helpful-update)
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(provide 'init-help)
;;; init-help.el ends here
