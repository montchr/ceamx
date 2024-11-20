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
(add-hook 'php-mode-hook #'display-line-numbers-mode)
(package! neon-mode)
(after! (:or php-mode phps-mode php-ts-mode)
  (when (featurep 'dap)
    (require 'dap-php)))
(package! flycheck-phpstan
  (def-hook! +php-mode-load-flycheck-phpstan-h ()
    '(php-mode-hook php-ts-mode-hook)
    "Load the Flycheck checker for PHPStan in PHP buffers."
    (require 'flycheck-phpstan)))
(after! reformatter
  (reformatter-define php-ecs-fmt
    :group 'ceamx
    :program (format "%s/vendor/bin/ecs" (getenv "PRJ_ROOT"))
    ;; XXX: Flags are very broken upstream <https://github.com/easy-coding-standard/easy-coding-standard/issues/213>
    :args `("check" "--no-ansi" "--no-progress-bar" "--no-interaction" "--quiet" "--fix" "--" ,input-file)
    :stdin nil
    :stdout nil))
;; php-cs-fixer pretends to be a patch-output-friendly formatter, but there's no
;; way to stop it from outputting extraneous garbage.  and "--quiet" literally
;; silences everything, even the diff output.  based on similar output issues
;; with ECS (and identically-named flags), i suspect this the fault of the
;; Symfony Console library, which is unfortunately quite widely used.
(after! reformatter
  (reformatter-define php-cs-fixer-fmt
    :group 'ceamx
    :program (format "%s/vendor/bin/php-cs-fixer" (getenv "PRJ_ROOT"))
    :args `("fix" "--using-cache=no" "--sequential" "--no-interaction"
            "--" ,input-file)
    ;; FIXME: symfony/console yet again
    ;; :args `("fix" "--diff" "--using-cache=no" "--show-progress=no"
    ;;         ,(format "--config=%s/%s"
    ;;                  (getenv "PRJ_ROOT")
    ;;                  (or (and (boundp 'ceamx-php-cs-fixer-config-file-path)
    ;;                           ceamx-php-cs-fixer-config-file-path)
    ;;                      ".php-cs-fixer.php"))
    ;;         "--show-progress=none"
    ;;         "-")
    :stdin nil
    :stdout nil))
(defun +reformatter--phpcbf-fmt-exit-code-success-p (exit-code)
  "Handle PHPCBF non-standard exit codes."
  (or (= 0 exit-code)
      (= 1 exit-code)))

;; FIXME: phpcbf is really finicky and doesn't play nice with the usual
;; formatter standards.
;; + the exit codes are nonsense -- if there are any unfixed errors left in
;;   the file, phpcbf will still return non-zero.
;; + i wonder if the stupid exit codes mean that the patch/diff is output to
;;   stderr instead of stdout as expected by `reformatter-define'?
(after! reformatter
  (reformatter-define phpcbf-fmt
    :program (format "%s/vendor/bin/phpcbf" (getenv "PRJ_ROOT"))
    :args (list "--stdin-path" input-file
                "-q"
                "-")
    ;; XXX: apparently `:exit-code-success-p' does not really accept a lambda? maybe report upstream?
    ;; :exit-code-success-p +reformatter--phpcbf-fmt-exit-code-success-p
    ))
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "vendor"))
;; (defconst ceamx-php-wordpress-template-names-regexp
;;   (rx))

;; (defun ceamx-php-wordpress-spaghetti-template-p (file)
;;   "Whether a file matches the pattern for a WordPress classic theme template."
;;   )
(after! web-mode
  ;; Blade: Override the default engine in case `web-mode' is associated with the php extension by default.
  (add-to-list 'web-mode-engines-alist '("blade"  . "\\.blade\\."))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php'" . web-mode))
  ;; Twig
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode)))

(provide 'init-lang-php)
;;; init-lang-php.el ends here
