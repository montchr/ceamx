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
    (add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode))
    (add-to-list 'major-mode-remap-alist '(php-mode-maybe . php-ts-mode))))
(package! neon-mode)
(after! (:or php-mode phps-mode php-ts-mode)
  (when (featurep 'dap)
    (require 'dap-php)))
(package! flymake-phpstan
  (add-hook 'php-mode-hook #'flymake-phpstan-turn-on)
  ;; NOTE: I'm not positive that this is the right name.
  (after! flycheck
    (add-to-list 'flycheck-disabled-checkers 'phpstan)))
(defun +reformatter--phpcbf-fmt-exit-code-success-p (exit-code)
    "Handle PHPCBF non-standard exit codes."
    (or (= 0 exit-code)
        (= 1 exit-code)))

(after! reformatter
  ;; FIXME: okay this hasn't worked either...
  (reformatter-define php-cs-fixer-fmt
    :program (format "%s/vendor/bin/php-cs-fixer" (getenv "PRJ_ROOT"))
    :args '("fix" "--diff" "--using-cache=no" "-q" "-"))

  ;; FIXME: phpcbf is really finicky and doesn't play nice with the usual
  ;; formatter standards.  the exit codes are nonsense.  and apparently the
  ;; `:exit-code-success-p' lambda is not a function?
  (reformatter-define phpcbf-fmt
    :program (format "%s/vendor/bin/phpcbf" (getenv "PRJ_ROOT"))
    :args (list "--stdin-path" input-file
                "-q"
                "-")
    :exit-code-success-p +reformatter--phpcbf-fmt-exit-code-success-p))
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "vendor"))
(after! web-mode
  ;; This should override the default file extension association.
  (pushnew! web-mode-engines-alist '(("blade"  . "\\.blade\\."))))

(provide 'init-lang-php)
;;; init-lang-php.el ends here
