;;; lib-package.el --- Package management library functions and macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery
;; Copyright (C) 2019-2023  Protesilaos Stavrou <info@protesilaos.com>

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;;         Chris Montgomery <chris@cdom.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Sources:

;; - <https://git.sr.ht/~protesilaos/dotfiles/tree/e21affc0153e556e06a28813efb252c7757b6aff/item/emacs/.emacs.d/init.el>

;;; Code:

(require 'package)
(require 'use-package)

(require 'config-package)

;; TODO: make interactive command?
(defun ceamx-package-install (package &optional method)
  "Install PACKAGE with optional METHOD.

If METHOD is nil or the `builtin' symbol, PACKAGE is not
installed as it is considered part of Emacs.

If METHOD is a string, it must be a URL pointing to the version
controlled repository of PACKAGE.  Installation is done with
`package-vc-install'.

If METHOD is a quoted list, it must have a form accepted by
`package-vc-install' such as:

\\='(denote :url \"https://git.sr.ht/~protesilaos/denote\" :branch \"main\")

If METHOD is any other non-nil value, install PACKAGE using
`package-install'."
  (unless (or (eq method 'builtin) (null method))
    (unless (package-installed-p package)
      (when (or (stringp method) (listp method))
        (package-vc-install method))
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install package))))

(defun cmx-loaded-packages ()
  "Return a list of all loaded packages.
Here packages include both `cmx-loaded-packages' and
`package-activated-list'.  The latter only covers what is found
in the `package-archives', whereas the former is for anything
that is expanded with the `package!' macro."
  (delete-dups (append cmx-loaded-packages package-activated-list)))

;;; Macros

;; TODO: make this more useful than just a name...
(defmacro use-feature! (name &rest args)
  "Simple wrapper for `use-package', passing through NAME and ARGS.

Functionally no different from `use-package' currently.

Exists primarily as an indication of intent to configure an
already-configured package. Also kept around for backward
compatibility.

Refer to the `use-package' documentation for further information."
  (declare (indent defun))
  `(use-package ,name
     ;; :elpaca nil
     ,@args))

(defmacro package! (package &rest body)
  "Require PACKAGE with BODY configurations.

PACKAGE is an unquoted symbol that is passed to `require'.  It
thus conforms with `featurep'.

BODY consists of ordinary Lisp expressions.  There are,
nevertheless, two unquoted plists that are treated specially:

1. (:install METHOD)
2. (:delay NUMBER)

These plists can be anywhere in BODY and are not part of its
final expansion.

The :install property is the argument passed to
`cmx-package-install' and has the meaning of METHOD
described therein.

The :delay property makes the evaluation of PACKAGE with the
expanded BODY happen with `run-with-timer'.

Also see `cmx-configure'."
  (declare (indent 1))
  (unless (memq package cmx-exclude-packages)
    (let (install delay)
      (dolist (element body)
        (when (plistp element)
          (pcase (car element)
            (:install (setq install (cdr element)
                            body (delq element body)))
            (:delay (setq delay (cadr element)
                          body (delq element body))))))
      (let ((common `(,(when install
                         `(cmx-package-install ',package ,@install))
                      (require ',package)
                      (add-to-list 'cmx-loaded-packages ',package)
                      ,@body
                      ;; (message "Ceamx loaded package: %s" ',package)
                      )))
        (cond
         ((featurep package)
          `(progn ,@body))
         (delay
          `(run-with-timer ,delay nil (lambda () ,@(delq nil common))))
         (t
          `(progn ,@(delq nil common))))))))

(defmacro config! (&rest body)
  "Evaluate BODY as a `progn'.
BODY consists of ordinary Lisp expressions.  The sole exception
is an unquoted plist of the form (:delay NUMBER) which evaluates
BODY with NUMBER seconds of `run-with-timer'.

N.B. `config!' does not try to autoload anything. Use it only for
forms that evaluate regardless!

Also see `package!'"
  (declare (indent 0))
  (let (delay)
    (dolist (element body)
      (when (plistp element)
        (pcase (car element)
          (:delay (setq delay (cadr element)
                        body (delq element body))))))
    (if delay
        `(run-with-timer ,delay nil (lambda () ,@body))
      `(progn ,@body))))

(provide 'lib-package)
;;; lib-package.el ends here
