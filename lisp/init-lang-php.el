;;; init-lang-php.el --- PHP language support  -*- lexical-binding: t;  -*-

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

(require 'ceamx-lib)
(appendq! xref-ignored-files
          '("_ide_helper_models.php"
            "_ide_helper.php"))
(package! (php-ts-mode :host github :repo "emacs-php/php-ts-mode")
  (when (eq 'php-ts-mode ceamx-lang-php-major-mode-provider)
    (add-to-list 'auto-mode-alist `(,ceamx-lang-php-extension-regexp . php-ts-mode))))
(package! neon-mode)
(after! (:or php-mode phps-mode php-ts-mode)
  (when (featurep 'dap)
    (require 'dap-php)))
(package! flymake-phpstan
  (add-hook 'php-mode-hook #'flymake-phpstan-turn-on)
  ;; NOTE: I'm not positive that this is the right name.
  (add-to-list 'flycheck-disabled-checkers 'phpstan))
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "vendor"))
(after! web-mode
  ;; This should override the default file extension association.
  (pushnew! web-mode-engines-alist '(("blade"  . "\\.blade\\."))))

(provide 'init-lang-php)
;;; init-lang-php.el ends here
