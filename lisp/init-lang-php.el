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
(package! php-mode
  ;; PHP is not a templating language and it never was.  `web-mode' does
  ;; templates better.  Furthermore, as of <2024-06-18 Tue 22:37> `php-ts-mode'
  ;; does not currently support embedded HTML syntax, so disabling this feature
  ;; of `php-mode' adds consistency.
  (setopt php-mode-template-compatibility nil))
(when (and (fboundp 'php-ts-mode)
           (treesit-language-available-p 'php))
  (add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode))
  (add-to-list 'major-mode-remap-alist '(php-mode-maybe . php-ts-mode)))
(package! neon-mode)
(after! (:or php-mode phps-mode php-ts-mode)
  (when (featurep 'dap)
    (require 'dap-php)))
(package! flymake-phpstan
  (add-hook 'php-mode-hook #'flymake-phpstan-turn-on)
  ;; NOTE: I'm not positive that this is the right name.
  (after! flycheck
    (add-to-list 'flycheck-disabled-checkers 'phpstan)))
(require 'ceamx-lib)

(defun +reformatter--phpcbf-fmt-exit-code-success-p (exit-code)
  "Handle PHPCBF non-standard exit codes."
  (or (= 0 exit-code)
      (= 1 exit-code)))

(after! reformatter
  (reformatter-define php-ecs-fmt
    :group 'ceamx
    :program (format "%s/vendor/bin/ecs" (getenv "PRJ_ROOT"))
    ;; XXX: Flags are very broken upstream <https://github.com/easy-coding-standard/easy-coding-standard/issues/213>
    :args `("check" "--no-ansi" "--no-progress-bar" "--no-interaction" "--quiet" "--fix" "--" ,input-file)
    :stdin nil
    :stdout nil)

  ;; FIXME: okay this hasn't worked either...
  (reformatter-define php-cs-fixer-fmt
    :program (format "%s/vendor/bin/php-cs-fixer" (getenv "PRJ_ROOT"))
    :args '("fix" "--diff" "--using-cache=no" "-q" "-"))

  ;; FIXME: phpcbf is really finicky and doesn't play nice with the usual
  ;; formatter standards.
  ;; + the exit codes are nonsense -- if there are any unfixed errors left in
  ;;   the file, phpcbf will still return non-zero.
  ;; + i wonder if the stupid exit codes mean that the patch/diff is output to
  ;;   stderr instead of stdout as expected by `reformatter-define'?
  (reformatter-define phpcbf-fmt
    :program (format "%s/vendor/bin/phpcbf" (getenv "PRJ_ROOT"))
    :args (list "--stdin-path" input-file
                "-q"
                "-")
    ;; XXX: apparently `:exit-code-success-p' does not really accept a lambda? maybe report upstream?
    :exit-code-success-p +reformatter--phpcbf-fmt-exit-code-success-p))
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "vendor"))
(after! web-mode
  ;; Blade: Override the default engine in case `web-mode' is associated with the php extension by default.
  (add-to-list 'web-mode-engines-alist '("blade"  . "\\.blade\\."))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php'" . web-mode))
  ;; Twig
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode)))

(provide 'init-lang-php)
;;; init-lang-php.el ends here
