;;; init-flymake.el --- Flymake support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2024  Chris Montgomery <chmont@proton.me>

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

;; Disable Flycheck checkers covered by Flymake equivalents.
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers
                (append (default-value 'flycheck-disabled-checkers)
                        '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck))))

(package! flymake-flycheck
  (add-hook 'flymake-mode-hook #'flymake-flycheck-auto))

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'text-mode-hook #'flymake-mode)

(after! flymake
  (define-keymap :keymap flymake-mode-map
    "C-c ! l" #'flymake-show-buffer-diagnostics
    "C-c ! n" #'flymake-goto-next-error
    "C-c ! p" #'flymake-goto-previous-error
    "C-c ! c" #'flymake-show-buffer-diagnostics)

  (setopt eldoc-documentation-function #'eldoc-documentation-compose)

  (def-hook! ceamx-flymake-eldoc-function-h ()
    'flymake-mode-hook
    "Use Flymake's Eldoc integration."
    (add-hook 'eldoc-documentation-functions #'flymake-eldoc-function nil t)))

(provide 'init-flymake)
;;; init-flymake.el ends here
